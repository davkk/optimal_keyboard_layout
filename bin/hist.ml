module CharMap = Map.Make (Char)

let () =
  Stdio.In_channel.with_file "macbeth.txt" ~f:In_channel.input_all
  |> String.fold_left
       (fun map c ->
         CharMap.update
           (Char.lowercase_ascii c)
           (function
             | Some value -> Some (value + 1)
             | None -> Some 1
             )
           map
       )
       CharMap.empty
  |> CharMap.to_list
  |> List.sort (fun (_, count1) (_, count2) -> -compare count1 count2)
  |> List.to_seq
  |> Seq.iter (fun (c, count) -> Fmt.pr "%c: %d@." c count)
;;
