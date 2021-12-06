open Types

module LocSet = Set.Make(Loc)
module LocMap = Map.Make(Loc)

let parse chan = Lexing.from_channel chan |> Parser.parse Lexer.token

let range x y =
  let d = y - x in
  let op = if d > 0 then ( + ) else ( - ) in
  List.init (abs d + 1) (fun i -> op x i)

let locs_of_diagonal ((x1, y1), (x2, y2)) =
  List.combine (range x2 x1) (range y2 y1)

let locs_of_horizontal ((x1, y), (x2, _)) =
  List.map (fun x -> (x, y)) (range x2 x1)

let locs_of_vertical ((x, y1), (_, y2)) =
  List.map (fun y -> (x, y)) (range y2 y1)

let locs_of_line l =
  if Line.is_horizontal l then locs_of_horizontal l
  else if Line.is_vertical l then locs_of_vertical l
  else if Line.is_diagonal l then locs_of_diagonal l
  else []

let map_of_line l =
  locs_of_line l |>
  List.fold_left (fun map loc -> LocMap.add loc 1 map) LocMap.empty

let draw =
  List.fold_left (fun map line ->
      map_of_line line |>
      LocMap.union (fun _ count c -> Some(count + c)) map)

let () =
  let lines =
    let chan = open_in "input" in
    let lines = parse chan in
    close_in chan; lines
  in
  let hv, others = List.partition Line.is_hv lines in
  let map_hv = draw LocMap.empty hv in
  let map_all = draw map_hv others in
  let count map = map |> LocMap.filter (fun _ i -> i > 1) |> LocMap.cardinal in
  Format.printf "Score 1: %d\n" (map_hv |> count);
  Format.printf "Score 2: %d\n" (map_all |> count);
