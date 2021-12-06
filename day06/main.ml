module IntMap = struct
  include Map.Make(Int)

  let update_add key x =
    update key (function None -> Some(x) | Some(y) -> Some(x + y))
end


let parse chan =
  input_line chan |> String.split_on_char ',' |>
  List.map int_of_string |>
  List.fold_left (fun map day -> IntMap.update_add day 1 map) IntMap.empty

let simulate_day map =
  IntMap.fold (fun day n map ->
      if day = 0 then IntMap.update_add 6 n map |> IntMap.update_add 8 n
      else IntMap.update_add (day - 1) n map
    ) map IntMap.empty

let simulate days fish =
  let rec aux acc day =
    if day >= days then acc
    else aux (simulate_day acc) (day + 1)
  in aux fish 0

let count fish = IntMap.fold (fun _ n acc -> n + acc) fish 0

let () =
  let fish =
    let chan = open_in "input" in
    let lines = parse chan in
    close_in chan; lines
  in
  let fish1 = simulate 80 fish in
  let fish2 = simulate 176 fish1 in
  Format.printf "Score 1: %d\n" (count fish1);
  Format.printf "Score 2: %d\n" (count fish2);
