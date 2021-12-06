module IntSet = Set.Make(Int)
module IntMap = Map.Make(Int)

module List = struct
  include List

  let foldi_left f accu l =
    List.fold_left (fun (i, acc) a -> i + 1, f i acc a) (0, accu) l |> snd
end

module Loc = struct
  type t = int * int
  let pos row col = (row, col)
  let compare = compare
  let pp fmt (row, col) = Format.fprintf fmt "(%d, %d)" row col
end

module Board = struct
  type t = {
    data: int list list;
    size: int * int;
    refs: Loc.t IntMap.t; (* locs: int -> loc list *)
  }

  (* TODO: hardcoded size *)
  (* TODO: Unicity not checked *)
  let from data =
    let preprocess_field row col acc (v: int) =
      { acc with refs = IntMap.add v (Loc.pos row col) acc.refs }
    in
    let preprocess_row row = List.foldi_left (preprocess_field row) in
    let board = {data; size = (5, 5); refs = IntMap.empty; } in
    List.foldi_left preprocess_row board data

  let size board = board.size

  let loc_opt x board = IntMap.find_opt x board.refs

  let score marked draw board =
    draw * List.fold_left
      (List.fold_left
         (fun acc x -> if IntSet.mem x marked then acc else x + acc))
      0 board.data

  let pp fmt board =
    (Format.pp_print_list ~pp_sep:Format.pp_print_newline
       (Format.pp_print_list ~pp_sep:Format.pp_print_space
          Format.pp_print_int)) fmt board.data
end

module Env = struct
  type t = {
    marked: IntSet.t;
    rcount: int IntMap.t;  (* count: loc -> int *)
    ccount: int IntMap.t;
    score:  int option;
  }

  let empty = {
    marked = IntSet.empty;
    rcount = IntMap.empty;
    ccount = IntMap.empty;
    score  = None;
  }

  let mark (row, col) x env =
    let rcount = Option.value (IntMap.find_opt row env.rcount) ~default:0 + 1 in
    let ccount = Option.value (IntMap.find_opt col env.ccount) ~default:0 + 1 in
    (rcount, ccount),
    { env with
      marked = IntSet.add x env.marked;
      rcount = IntMap.add row rcount env.rcount;
      ccount = IntMap.add col ccount env.ccount; }

  let mem_marked x env = IntSet.mem x env.marked
end

let draw_field x board pos env =
  let (rcount, ccount), env = Env.mark pos x env in
  let (rsize, csize) = Board.size board in
  if rcount < rsize && ccount < csize then env
  else
    let score = Board.score env.marked x board in
    Format.printf "%a\n" Board.pp board;
    Format.printf "Win with score: %d" score;
    IntSet.iter (fun x -> Format.printf " %d " x) env.marked;
    Format.printf "\n";
    { env with score = Some(score) }

let draw_board x board env =
  if Env.mem_marked x env || env.score <> None then env
  else match Board.loc_opt x board with
    | None -> env
    | Some loc -> draw_field x board loc env

let play boards =
  let env = Array.map (fun _ -> Env.empty) boards in
  List.fold_left (fun env x ->
      (* Format.printf "Drawing %d\n" x; *)
      Array.map2 (draw_board x) boards env) env

let parse_and_check chan =
  let (draw, data) = Lexing.from_channel chan |> Parser.parse Lexer.token in
  let boards = Array.map Board.from data in
  (draw, boards)

let () =
  let draw, boards =
    let chan = open_in "input" in
    let bvs = parse_and_check chan in
    close_in chan; bvs
  in
  let _ = play boards draw in
  ()
