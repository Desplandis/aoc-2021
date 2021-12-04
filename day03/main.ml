module B = Bitvector_bool

let parse chan =
  match Lexing.from_channel chan |> Parser.parse Lexer.token with
  | [] -> Some ([], 0)
  | (hd :: tl) as bvs ->
    let length = B.length hd in
    if List.for_all (fun bv -> B.length bv = length) tl
    then Some (bvs, length) else None

let mcb_of_bitvectors size bvs =
  let aux count bv =
    B.foldi
      (fun bit acc b ->
         (Parray.get acc bit) + (if b then 1 else -1) |> Parray.set acc bit)
      count bv
  in
  let count = List.fold_left aux (Parray.make size 0) bvs in
  B.init size (fun i -> Parray.get count i >= 0)

let rating f size bvs =
  let rec aux bit = function
    | [] -> None
    | [ bv ] -> Some (bv)
    | bvs ->
      if bit < 0 then None
      else
        let b =
          f (List.fold_left
            (fun acc bv -> acc + if B.get bv bit then 1 else -1)
            0 bvs) 0
        in
        let filtered = List.filter (fun bv -> B.get bv bit = b) bvs in
        (*
        Format.printf "[At bit %d | %s] bvs : %a\n\n"
          bit
          (if b then "1" else "0")
          (Format.pp_print_list Bitvector.pp) filtered;
           *)
        aux (bit - 1) filtered
  in aux (size - 1) bvs

let rating_carbon_dioxyde = rating (>=)
let rating_dioxygen = rating (<)

let () =
  let bvs =
    let chan = open_in "input" in
    let bvs = parse chan in
    close_in chan; bvs
  in
  (*
  Format.printf "Bitvectors:\n%a\n"
    (Format.pp_print_list Bitvector.pp) bvs;
  *)
  let (bvs, size) = Option.get bvs in
  let gamma = mcb_of_bitvectors size bvs in
  let epsilon = B.not gamma in
  let carbon_opt = rating_carbon_dioxyde size bvs in
  let oxygen_opt = rating_dioxygen size bvs in
  Format.printf "Most common bits (gamma): %d\n" (B.to_int gamma);
  Format.printf "Least common bits (epsilon): %d\n" (B.to_int epsilon);
  Format.printf "Score 1: %d\n" ((B.to_int gamma) * (B.to_int epsilon));
  Format.printf "Carbon dioxyde rating: %a\n"
    (Format.pp_print_option Format.pp_print_int)
    (Option.map B.to_int carbon_opt);
  Format.printf "Dioxygen rating: %a\n"
    (Format.pp_print_option Format.pp_print_int)
    (Option.map B.to_int oxygen_opt);
  Format.printf "Score 2: %a\n"
    (Format.pp_print_option Format.pp_print_int)
    (Option.bind carbon_opt @@ fun carbon ->
     Option.bind oxygen_opt @@ fun oxygen ->
     Some (B.to_int carbon * B.to_int oxygen));
