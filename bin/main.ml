let words = Stdio.In_channel.with_file "macbeth.txt" ~f:In_channel.input_all

[@@@warning "-32"]

type finger =
  | LPinky
  | LRing
  | LMiddle
  | LIndex
  | RIndex
  | RMiddle
  | RRing
  | RPinky
  | Noop
[@@deriving show { with_path = false }, enum]

type keyboard =
  { keys: char array;
    dist: float
  }
[@@deriving show { with_path = false }]

[@@@warning "+32"]

let key_to_finger key =
  match key mod 10 with
  | 0 -> LPinky
  | 1 -> LRing
  | 2 -> LMiddle
  | 3 | 4 -> LIndex
  | 5 | 6 -> RIndex
  | 7 -> RMiddle
  | 8 -> RRing
  | 9 -> RPinky
  | _ -> Noop
;;

let pp_keys keys =
  keys
  |> Array.iteri (fun idx key ->
    if idx mod 10 = 0 then Fmt.epr "@.";
    Fmt.epr "%c" key
  );
  Fmt.epr "@."
;;

let coords ?(max = 10) idx =
  let x = idx mod max in
  let y = idx / max in
  x, y
;;

let ( <-> ) a b =
  let xa, ya = coords a in
  let xb, yb = coords b in
  let d = abs ((yb * yb) - (ya * ya) + (xb * xb) - (xa * xa)) in
  sqrt @@ float d
;;

let calc_dist keys =
  let fingers = [| 10; 11; 12; 13; 16; 17; 18; 19 |] in
  String.fold_left
    (fun acc letter ->
      let curr_key =
        keys
        |> Array.find_index (( = ) @@ Char.lowercase_ascii letter)
        |> Option.value ~default:(-1)
      in
      match key_to_finger curr_key with
      | Noop -> acc
      | finger ->
        let finger_idx = finger_to_enum finger in
        let prev_key = fingers.(finger_idx) in
        let d = prev_key <-> curr_key in
        let home_row_penalty = 100.0 *. d *. d in
        let pinky_penalty = float finger_idx -. 3.5 in
        acc +. home_row_penalty +. (pinky_penalty *. pinky_penalty)
    )
    0.0
    words
;;

let shuffle arr =
  let arr = Array.copy arr in
  let l = Array.length arr in
  Iter.(l - 1 --^ 0)
  |> Iter.iter (fun i ->
    let j = Random.int (i + 1) in
    Core.Array.swap arr i j
  );
  arr
;;

let init_population n =
  let letters = Seq.init 26 (fun idx -> Char.chr (idx + 97)) in
  let special = [ ';'; ','; '.'; '/' ] |> List.to_seq in
  let keys = Seq.append letters special |> Array.of_seq in
  Array.init n (fun _ ->
    let keys = shuffle keys in
    let dist = calc_dist keys in
    { keys; dist }
  )
;;

let roulette population =
  let min_kb = population.(0) in
  let max_kb = population.(Array.length population - 1) in
  let scores =
    population
    |> Iter.of_array
    |> Iter.map (fun kb -> (min_kb.dist +. max_kb.dist) /. kb.dist)
    |> Iter.persistent_lazy
  in
  let total_score = Iter.sumf scores in
  Iter.fold_map
    (fun acc score ->
      let prob = acc +. (score /. total_score) in
      prob, prob
    )
    0.0
    scores
  |> Iter.to_array
;;

module IntSet = Set.Make (Int)

let select_pair population =
  let cdf = roulette population in
  let rec aux a b =
    if a = b
    then (
      let x1 = Random.float 1.0 in
      let x2 = Random.float 1.0 in
      let a =
        Array.find_index (fun prob -> x1 <= prob) cdf
        |> Option.value ~default:0
      in
      let b =
        Array.find_index (fun prob -> x2 <= prob) cdf
        |> Option.value ~default:0
      in
      aux a b
    )
    else a, b
  in
  let a, b = aux 0 0 in
  population.(a), population.(b)
;;

module Iter = struct
  include Iter

  let transpose ?(from = 0) ?upto arr =
    let upto = Option.value ~default:(Array.length arr) upto in
    Iter.append Iter.(from -- (upto - 1)) Iter.(0 -- (from - 1))
    |> Iter.map (fun idx ->
      let x, y = coords ~max:3 idx in
      let idx = y + (x * 10) in
      idx, arr.(idx)
    )
  ;;
end

module CharSet = Set.Make (Char)

let crossover population =
  let parent1, parent2 = select_pair population in
  let offspring = Array.copy parent1.keys in
  let len = Array.length offspring in
  let split = int_of_float (Random.float 1.0 *. float len) in
  let used_keys =
    Iter.transpose ~upto:split parent1.keys
    |> Iter.fold
         (fun set (idx, key) ->
           offspring.(idx) <- key;
           CharSet.add key set
         )
         CharSet.empty
  in
  let rest_keys =
    Iter.transpose ~from:split parent2.keys
    |> Iter.filter (fun (_, key) -> not @@ CharSet.mem key used_keys)
    |> Iter.to_seq_persistent
  in
  let _ =
    Iter.transpose ~from:split parent1.keys
    |> Iter.to_seq_persistent
    |> Seq.zip rest_keys
    |> Seq.fold_left
         (fun set ((_, key), (idx, _)) ->
           offspring.(idx) <- key;
           CharSet.add key set
         )
         used_keys
  in
  { keys = offspring; dist = calc_dist offspring }
;;

let random_int_pair ~max =
  let rec aux a b = if a = b then aux a (Random.int max) else a, b in
  aux (Random.int max) (Random.int max)
;;

let mutation ~rate kb =
  if Random.float 1.0 < rate
  then (
    let idx1, idx2 = random_int_pair ~max:(Array.length kb.keys) in
    Core.Array.swap kb.keys idx1 idx2;
    { kb with dist = calc_dist kb.keys }
  )
  else kb
;;

let optimize () =
  let rec aux step best_kb population =
    let pop_size = Array.length population in
    if step = 0
    then (
      Fmt.epr "%f@." best_kb.dist;
      pp_keys best_kb.keys
    )
    else (
      Array.sort (fun a b -> compare a.dist b.dist) population;
      let min_kb = population.(0) in
      let elite_idx = pop_size / 3 in
      let population =
        population
        |> Array.mapi (fun idx kb ->
          if idx > elite_idx then crossover population else kb
        )
        |> Array.map (fun kb -> mutation ~rate:0.1 kb)
      in
      let average =
        Array.fold_left (fun acc kb -> acc +. kb.dist) 0.0 population
        /. float pop_size
      in
      Fmt.pr "%f %f@." min_kb.dist average;
      aux (step - 1) min_kb population
    )
  in
  let population = init_population 30 in
  aux 1000 population.(0) population
;;

let () =
  Random.init @@ int_of_string Sys.argv.(1);
  optimize ()
;;

(* let () = *)
(*   let qwerty = *)
(*     calc_dist @@ Array.of_seq @@ String.to_seq "qwertyuiopasdfghjkl;zxcvbnm,./" *)
(*   in *)
(*   let dvorak = *)
(*     calc_dist @@ Array.of_seq @@ String.to_seq "',.pyfgcrlaoeuidhtns;qjkxbmwvz" *)
(*   in *)
(*   Fmt.pr "qwerty: %f@." qwerty; *)
(*   Fmt.pr "dvorak: %f@." dvorak *)
(* ;; *)
