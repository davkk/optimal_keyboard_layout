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
  { keys : char array
  ; dist : float
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

let coords ?(max = 10) idx =
  let x = idx mod max in
  let y = idx / max in
  x, y
;;

let ( <-> ) a b =
  let xa, ya = coords a in
  let xb, yb = coords b in
  let d = abs ((yb * yb) - (ya * ya) + (xb * xb) - (xa * xa)) in
  d |> float |> sqrt
;;

let calc_dist text keys =
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
      | finger -> begin
        let finger_idx = finger_to_enum finger in
        let prev_key = fingers.(finger_idx) in
        let d = prev_key <-> curr_key in
        let home_row_penalty = exp d -. 1.0 in
        let pinky_penalty = exp @@ float (max (abs (finger_idx - 4)) 2) in
        acc +. home_row_penalty +. pinky_penalty
      end)
    0.0
    text
;;

let shuffle arr =
  let arr = Array.copy arr in
  let l = Array.length arr in
  for i = l - 1 downto 1 do
    let j = Random.int (i + 1) in
    Core.Array.swap arr i j
  done;
  arr
;;

let init_population n =
  let letters = Seq.init 26 (fun idx -> Char.chr (idx + 97)) in
  let special = [ ';'; ','; '.'; '/' ] |> List.to_seq in
  let keys = Seq.append letters special |> Array.of_seq in
  Array.init n (fun _ ->
    let keys = shuffle keys in
    let dist = calc_dist words keys in
    { keys; dist })
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
      prob, prob)
    0.0
    scores
  |> Iter.to_array
;;

module IntSet = Set.Make (Int)

let select_pair population =
  let cdf = roulette population in
  let rec aux set =
    if IntSet.cardinal set = 2
    then set
    else (
      let x = Random.float 1.0 in
      let rand_kb =
        Array.find_index (fun prob -> x < prob) cdf
        |> Option.value ~default:(-1)
      in
      aux @@ IntSet.add rand_kb set)
  in
  match aux IntSet.empty |> IntSet.elements with
  | [ a; b ] -> population.(a), population.(b)
  | _ -> failwith "cannot perform the crossover"
;;

let pp_keys keys =
  keys
  |> Array.iteri (fun idx key ->
    if idx mod 10 = 0 then Fmt.epr "@.";
    Fmt.epr "%c " key);
  Fmt.epr "@."
;;

let transposed_idx_iter ~from ~upto =
  let iter = Iter.(from -- (upto - 1)) in
  let iter =
    if from = 0 then iter else Iter.append iter Iter.(0 -- (from - 1))
  in
  iter
  |> Iter.map (fun idx ->
    let x, y = coords ~max:3 idx in
    y + (x * 10))
;;

let crossover population =
  let parent1, parent2 = select_pair population in
  let offspring = Array.copy parent1.keys in
  let len = Array.length offspring in
  let split = len / 2 in
  (* Fmt.pr "@.split: %d@." split; *)
  let lh_keys =
    transposed_idx_iter ~from:0 ~upto:split
    |> Iter.map (fun idx -> parent1.keys.(idx))
    |> Iter.to_array
  in
  let rh_keys =
    transposed_idx_iter ~from:split ~upto:len
    |> Iter.map (fun idx -> parent2.keys.(idx))
    |> Iter.filter (fun key -> not @@ Array.mem key lh_keys)
    |> Iter.to_array
  in
  transposed_idx_iter ~from:split ~upto:len
  |> Iter.take (Array.length rh_keys)
  |> Iter.iteri (fun idx key_idx -> offspring.(key_idx) <- rh_keys.(idx));
  (* pp_keys parent1.keys; *)
  (* pp_keys parent2.keys; *)
  (* pp_keys offspring; *)
  { keys = offspring; dist = calc_dist words offspring }
;;

let random_int_pair ~max =
  let rec aux a b =
    if a = b
    then (
      let a = Random.int max in
      let b = Random.int max in
      a, b)
    else aux a b
  in
  aux 0 0
;;

let mutation ~rate kb =
  let idx1, idx2 = random_int_pair ~max:(Array.length kb.keys) in
  if Random.float 1.0 < rate
  then begin
    Core.Array.swap kb.keys idx1 idx2;
    { kb with dist = calc_dist words kb.keys }
  end
  else kb
;;

let optimize () =
  let rec aux step best_kb population =
    let size = Array.length population in
    let min_kb =
      Core.Array.min_elt population ~compare:(fun a b -> compare a.dist b.dist)
    in
    match min_kb with
    | Some keyboard ->
      if step = 10000
      then begin
        Fmt.epr "@.%f@." best_kb.dist;
        pp_keys best_kb.keys
      end
      else begin
        Array.sort (fun a b -> compare a.dist b.dist) population;
        let elite_idx = int_of_float (float (Array.length population) *. 0.9) in
        let population =
          population
          |> Array.mapi (fun idx kb ->
            if idx < elite_idx then crossover population else kb)
          |> Array.map (fun kb -> mutation ~rate:0.03 kb)
        in
        let average =
          Array.fold_left (fun acc kb -> acc +. kb.dist) 0.0 population
          /. float size
        in
        Fmt.pr "%d %f %f@." step keyboard.dist average;
        aux
          (step + 1)
          (if keyboard.dist < best_kb.dist then keyboard else best_kb)
          population
      end
    | None -> ()
  in
  let population = init_population 10 in
  aux 0 population.(0) population
;;

let () =
  Random.init 2001;
  optimize ()
;;
