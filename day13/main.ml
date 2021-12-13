module Loc = struct
  type t = int * int
  let compare = compare
end
module LocSet = Set.Make(Loc)

let (let*) = Option.bind

type instr = Horizontal of int | Vertical of int
type origami = {
  dots: LocSet.t;
  instrs: instr list;
}

let range x y =
  let d = y - x in
  let op = if d > 0 then ( + ) else ( - ) in
  List.init (abs d + 1) (fun i -> op x i)

(* Ugly, don't care *)
let pp_dots fmt dots =
  let xmax, ymax =
    LocSet.fold
      (fun (x, y) (xmax, ymax) -> (max xmax x, max ymax y))
      dots (0, 0)
  in
  let xrange = range 0 xmax in
  let yrange = range 0 ymax in
  List.iter (fun y ->
      List.iter (fun x ->
          match LocSet.find_opt (x, y) dots with
          | None -> Format.pp_print_char fmt '.'
          | Some _ -> Format.pp_print_char fmt '#'
        ) xrange;
      Format.pp_print_newline fmt ())
    yrange

(* TODO: Factorize *)
let step_horizontal dots yfold =
  LocSet.filter_map (fun (x, y) ->
      if y < yfold then Some (x, y)
      else if y > yfold then Some (x, yfold - (y - yfold))
      else None
    ) dots

let step_vertical dots xfold =
  LocSet.filter_map (fun (x, y) ->
      if x < xfold then Some (x, y)
      else if x > xfold then Some (xfold - (x - xfold), y)
      else None
    ) dots

let step dots = function
  | Horizontal y -> step_horizontal dots y
  | Vertical x -> step_vertical dots x

let aoc = function
  | None | Some { instrs = []; _ } -> Format.eprintf "Invalid input\n"
  | Some { dots; instrs = fst :: instrs } ->
    let dots = step dots fst in
    LocSet.cardinal dots |>
    Format.printf "Number of dots after first fold: %d\n";
    let dots = List.fold_left step dots instrs in
    Format.printf "Code after all folds:\n%a\n" pp_dots dots

let dot x y =
  let* x = int_of_string_opt x in
  let* y = int_of_string_opt y in
  Some (x, y)

let instr axis v =
  let* v = int_of_string_opt v in
  match axis with
  | "y" -> Some (Horizontal v)
  | "x" -> Some (Vertical v)
  | _ -> None

let line =
  let fold = Str.regexp "fold along \\([x|y]\\)=\\([0-9]+\\)" in
  fun p l -> match String.split_on_char ',' l with
  | [x; y] ->
    let* dot = dot x y in
    Some { p with dots = LocSet.add dot p.dots }
  | [line] when Str.string_match fold line 0 ->
    let* instr = instr (Str.matched_group 1 line) (Str.matched_group 2 line) in
    Some { p with instrs = instr :: p.instrs }
  | [ "" ] -> Some p
  | _ -> None

let parse chan =
  let rec aux = function
    | None -> None
    | Some a ->
      try input_line chan |> line a |> aux
      with End_of_file -> Some { a with instrs = List.rev a.instrs }
  in aux @@ Some { dots = LocSet.empty; instrs = [] }

let () =
  try
    let chan = Array.get Sys.argv 1 |> open_in in
    let data = parse chan in
    close_in chan; aoc data
  with
  | Sys_error msg -> Format.eprintf "%s\n" msg
  | Invalid_argument _ -> Format.eprintf "Input file required\n"
