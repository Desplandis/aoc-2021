module String = struct
  include String
  let hash = Hashtbl.hash

  let is_uppercase s = String.uppercase_ascii s = s
end
module StringSet = Set.Make(String)

module G = Graph.Persistent.Graph.Concrete(String)

let enumerate_path ?(visit_twice=false) g =
  let rec aux (vis, twice) = function
    | "end" -> 1
    | v ->
      let vis, twice =
        if StringSet.mem v vis && twice && v <> "start"
        then StringSet.remove v vis, false
        else vis, twice
      in
      if StringSet.mem v vis then 0
      else
        let vis = if String.is_uppercase v then vis else StringSet.add v vis in
        let succs = G.succ g v in
        List.fold_left (fun acc s -> acc + aux (vis, twice) s) 0 succs
  in
  aux (StringSet.empty, visit_twice) "start"

let aoc (g: G.t) =
  let npaths = enumerate_path g in
  Format.printf "Score 1: %d\n" npaths;
  let npaths = enumerate_path ~visit_twice:true g in
  Format.printf "Score 2: %d\n" npaths

let line g l = match String.split_on_char '-' l with
  | [src; dst] -> G.add_edge g src dst
  | _ ->
    Format.eprintf "Expected cave connection of the form `src-dst`";
    exit(1)

let parse chan =
  let rec aux g =
    try input_line chan |> line g |> aux
    with End_of_file -> g
  in aux G.empty

let () =
  try
    let chan = Array.get Sys.argv 1 |> open_in in
    let data = parse chan in
    close_in chan; aoc data
  with
  | Sys_error msg -> Format.eprintf "%s\n" msg
  | Invalid_argument _ -> Format.eprintf "Input file required\n"
