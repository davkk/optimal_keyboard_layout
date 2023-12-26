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
  ; score : float
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

let coords idx =
  let x = idx mod 10 in
  let y = idx / 10 in
  x, y
;;

let dist a b =
  let xa, ya = coords a in
  let xb, yb = coords b in
  float @@ abs ((yb * yb) - (ya * ya) + (xb * xb) - (xa * xa))
;;

let fitness text keys =
  let fingers = [| 11; 12; 13; 14; 17; 18; 19; 20 |] in
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
        fingers.(finger_idx) <- curr_key;
        acc +. dist prev_key curr_key
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
    let score = fitness words keys in
    { keys; score })
;;

let roulette population =
  Array.sort (fun a b -> compare a.score b.score) population;
  let total_score =
    Array.fold_left (fun acc kb -> acc +. kb.score) 0.0 population
  in
  let _, cdf =
    Array.fold_left_map
      (fun acc kb ->
        let prob = acc +. (1.0 -. (kb.score /. total_score)) in
        prob, prob)
      0.0
      population
  in
  cdf
;;

module IntSet = Set.Make (Int)

let select_pair population =
  let cdf = roulette population in
  let rec aux set =
    if IntSet.cardinal set = 2
    then set
    else (
      let rand_kb =
        Array.find_index (fun prob -> Random.float 1.0 < prob) cdf
      in
      match rand_kb with
      | Some kb -> aux @@ IntSet.add kb set
      | None -> failwith "impossible 1")
  in
  match aux IntSet.empty |> IntSet.elements with
  | [ a; b ] -> population.(a), population.(b)
  | _ -> failwith "impossible 2"
;;

let crossover population =
  let parent1, parent2 = select_pair population in
  let offspring =
    Array.init (Array.length parent1.keys) (fun idx ->
      let x1, _ = coords idx in
      if x1 < 5 then Some parent1.keys.(idx) else None)
  in
  Array.iter
    (fun key ->
      match Array.find_index Option.is_none offspring with
      | Some idx ->
        if not @@ Array.mem (Some key) offspring
        then offspring.(idx) <- Some key
      | None -> ())
    parent2.keys;
  let offspring = Core.Array.filter_map ~f:(fun kb -> kb) offspring in
  { keys = offspring; score = fitness words offspring }
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

let mutation population =
  let p_mut = 0.03 in
  Array.iter
    (fun kb ->
      let idx1, idx2 = random_int_pair ~max:(Array.length kb.keys) in
      if Random.float 1.0 < p_mut then Core.Array.swap kb.keys idx1 idx2)
    population;
  population
;;

let pp_keys kb =
  kb.keys
  |> Array.iteri (fun idx key ->
    if idx mod 10 = 0 then Fmt.epr "@.";
    Fmt.epr "%c " key)
;;

let optimize () =
  let rec aux step population =
    let size = Array.length population in
    if step = 100
    then
      Core.Array.min_elt population ~compare:(fun a b ->
        compare a.score b.score)
      |> function
      | Some keyboard -> pp_keys keyboard
      | None -> ()
    else begin
      let population =
        Array.init size (fun _ -> crossover population) |> mutation
      in
      let average =
        Array.fold_left (fun acc kb -> acc +. kb.score) 0.0 population
      in
      Fmt.pr "%d %f@." step average;
      aux (step + 1) population
    end
  in
  aux 0 @@ init_population 10
;;

let () =
  Random.init 2001;
  optimize ()
;;
