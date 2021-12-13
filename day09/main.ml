module Array = struct
  include Array

  let get_opt a i = try Some(get a i) with Invalid_argument _ -> None
end

module Loc = struct
  type t = int * int

  let compare (x1, y1) (x2, y2) =
    let c = compare x1 x2 in
    if c <> 0 then c
    else compare y1 y2
end
module LocSet = Set.Make(Loc)

module HeightMap = struct
  include Map.Make(Loc)

  let binding_opt k map = match find_opt k map with
    | None -> None
    | Some v -> Some (k, v)

  let neighbors (x, y) map =
    let up =  binding_opt (x - 1, y) map in
    let down = binding_opt (x + 1, y) map in
    let left = binding_opt (x, y - 1) map in
    let right = binding_opt (x, y + 1) map in
    List.filter_map (fun x -> x) [ up; down; left; right ]

  let lowpoints map =
    fold (fun k v acc ->
        let ns = neighbors k map in
        if List.exists (fun (_, n) -> v >= n) ns then acc
        else (k, v) :: acc)
      map []

  let basin pos map =
    let rec aux visited (p: key) =
      let v = find p map in
      List.fold_left
        (fun vs ((nk: key), nv) ->
           if LocSet.mem nk vs || nv = 9 || nv < v then vs
           else aux vs nk)
        (LocSet.add p visited)
        (neighbors p map)
    in aux LocSet.empty pos

  let pp ?(pp_sep = fun _ () -> ()) pp_key fmt =
    iter
      (fun (x, y) v ->
         if y = 0 && x <> 0 then Format.pp_print_newline fmt ()
         else pp_sep fmt ();
         pp_key fmt v)
end


let aoc data =
  (* Format.printf "%a\n" (HeightMap.pp Format.pp_print_int) data; *)
  let lowpoints = HeightMap.lowpoints data in
  let bsizes =
    List.fold_left
      (fun acc (p, _) -> (HeightMap.basin p data |> LocSet.cardinal) :: acc)
      [] lowpoints |>
    List.sort (fun x y -> - compare x y)
  in
  let score1 = List.fold_left (fun acc (_, v) -> v + 1 + acc) 0 lowpoints in
  let score2 = match bsizes with
    | fst :: snd :: thd :: _ -> Some (fst * snd * thd)
    | _ -> None
  in
  Format.printf "Score 1: %d\n" score1;
  Format.printf "Score 2: %a\n"
    (Format.pp_print_option Format.pp_print_int) score2

let digit_of_char = function
  | '0' -> 0 | '1' -> 1 | '2' -> 2 | '3' -> 3 | '4' -> 4
  | '5' -> 5 | '6' -> 6 | '7' -> 7 | '8' -> 8 | '9' -> 9
  | _ -> failwith "digit_of_char"

let parse chan =
  let data_of_line x str =
    String.to_seqi str |> Seq.map (fun (y, c) -> (x, y), digit_of_char c) |>
    HeightMap.of_seq
  in
  let rec aux x acc =
    try
      input_line chan |> data_of_line x |>
      HeightMap.union (fun _ _ _ -> assert false (* by parsing *)) acc |>
      aux (x + 1)
    with End_of_file -> acc
  in aux 0 HeightMap.empty

let () =
  try
    let chan = Array.get Sys.argv 1 |> open_in in
    let data = parse chan in
    close_in chan; aoc data
  with
  | Sys_error msg -> Format.eprintf "%s\n" msg
  | Invalid_argument _ -> Format.eprintf "Input file required\n"
  | Failure _ -> Format.eprintf "Expected digit input\n"
