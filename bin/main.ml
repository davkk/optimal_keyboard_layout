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
  ; cols : int
  }

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
  | _ -> failwith "unknown key"
;;

let dist kb a b =
  let ya = a mod kb.cols in
  let xa = a / kb.cols in
  let yb = b mod kb.cols in
  let xb = b / kb.cols in
  abs @@ ((yb * yb) - (ya * ya) + (xb * xb) - (xa * xa))
;;

let fitness kb text =
  let fingers = [| 11; 12; 13; 14; 17; 18; 19; 20 |] in
  String.fold_left
    (fun acc letter ->
      let key =
        Array.find_index (( = ) (Char.lowercase_ascii letter)) kb.keys
      in
      match key with
      | Some key -> begin
        match key_to_finger key with
        | finger -> begin
          let finger_idx = finger_to_enum finger in
          let prev = fingers.(finger_idx) in
          fingers.(finger_idx) <- key;
          acc + dist kb prev key
        end
      end
      | None -> acc)
    0
    text
;;

(* let gen_init_population *)
(* let crossover *)
(* let mutation *)

let () =
  let letters = Seq.init 26 (fun idx -> Char.chr (idx + 97)) in
  let special = [ ';'; ','; '.'; '/' ] |> List.to_seq in
  let keyboard =
    { keys = Seq.append letters special |> Array.of_seq; cols = 10 }
  in
  let words =
    Stdio.In_channel.with_file "macbeth.txt" ~f:(fun channel ->
      In_channel.input_all channel)
  in
  let result = fitness keyboard words in
  Fmt.pr "@.%d@." result
;;
(* Fmt.pr "@.%a@." (Fmt.array ~sep:Fmt.sp Fmt.char) keys *)
