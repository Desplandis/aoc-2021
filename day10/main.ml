module String = struct
  include String
  let get_opt s i = try Some(get s i) with Invalid_argument _ -> None
end

module List = struct
  include List

  let pop = function
    | [] -> assert false
    | hd :: tl -> hd, tl
end

type token = Open of kind | End of kind
and kind = PAREN | BRACKET | BRACE | CMP
(* type chunk = Chunk of kind * chunk list *)

let token_of_char = function
  | '(' -> Some (Open PAREN)    | ')' -> Some (End PAREN)
  | '[' -> Some (Open BRACKET)  | ']' -> Some (End BRACKET)
  | '{' -> Some (Open BRACE)    | '}' -> Some (End BRACE)
  | '<' -> Some (Open CMP)      | '>' -> Some (End CMP)
  | _ -> None

type err = Incomplete of kind list | Corrupted of kind * kind
type res = (unit, err) result

let token tstack c = match token_of_char c with
  | None ->
    Format.printf "Invalid token %c" c;
    exit (-1)
  | Some (Open t) -> Ok(t :: tstack)
  | Some (End t) ->
    let start, stack = List.pop tstack in (* TODO: Add err case *)
    if start = t then Ok(stack) else Error(Corrupted (start, t))

let line l =
  let rec aux (stack: kind list) i = match String.get_opt l i with
    | None -> if stack = [] then Ok (()) else Error (Incomplete stack)
    | Some c -> match token stack c with
      | Ok(stack) -> aux stack (i + 1)
      | Error e -> Error e
  in aux [] 0

let parse chan =
  let rec aux acc =
    try (input_line chan |> line) :: acc |> aux
    with End_of_file -> acc
  in aux []

let score_of_corrupted = function
  | PAREN -> 3
  | BRACKET -> 57
  | BRACE -> 1197
  | CMP -> 25137

let score_of_incomplete stack =
  let score = function
    | PAREN -> 1
    | BRACKET -> 2
    | BRACE -> 3
    | CMP -> 4
  in
  let rec aux acc = function
    | [] -> acc
    | hd :: tl -> aux (acc * 5 + score hd) tl
  in aux 0 stack

let aoc (results: res list) =
  let corrupted =
    List.filter_map
      (function Error (Corrupted (_, t)) -> Some t | _ -> None)
      results in
  let incomplete =
    List.filter_map
      (function Error (Incomplete stack) -> Some stack | _ -> None)
      results in
  let score_corrupted =
    List.fold_left (fun acc t -> acc + score_of_corrupted t) 0 corrupted in
  let scores_incomplete =
    List.map score_of_incomplete incomplete |>
    List.sort Int.compare in
  Format.printf "Corrupted lines score: %d\n" score_corrupted;
  Format.printf "Incomplete lines score: %d\n"
    (List.nth scores_incomplete (List.length scores_incomplete / 2))

let () =
  try
    let chan = Array.get Sys.argv 1 |> open_in in
    let data = parse chan in
    close_in chan; aoc data
  with
  | Sys_error msg -> Format.eprintf "%s\n" msg
  | Invalid_argument _ -> Format.eprintf "Input file required\n"
