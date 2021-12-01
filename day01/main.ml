let count_increased depths =
  List.fold_left
    (fun (prev, acc) cur -> cur, if cur > prev then acc + 1 else acc)
    (Int.max_int, 0) depths |> snd

let count_increased_window depths =
  let rec aux acc = function
    | a :: (b :: c :: _ as tl) -> aux (a + b + c :: acc) tl
    | _ -> acc
  in
  aux [] depths |> List.rev |> count_increased

let () =
  let depths =
    let chan = open_in "input" in
    let rec aux acc =
      try (input_line chan |> int_of_string) :: acc |> aux
      with End_of_file -> close_in chan; acc in
    aux [] |> List.rev
  in
  (*
  Format.printf "%a\n"
    (Format.pp_print_list ~pp_sep:Format.pp_print_newline Format.pp_print_int)
    depths
  *)
  Format.printf "%d\n" @@ count_increased depths;
  Format.printf "%d\n" @@ count_increased_window depths
