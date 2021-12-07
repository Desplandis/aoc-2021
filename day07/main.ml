let sum =
  let rec aux acc = function
    | 0 -> acc
    | n -> aux (n + acc) (n - 1)
  in aux 0

let parse chan =
  input_line chan |> String.split_on_char ',' |>
  List.map int_of_string

let cost_diff x = Array.fold_left (fun acc c -> acc + abs(c - x)) 0
let cost_sumdiff x = Array.fold_left (fun acc c -> acc + sum (abs(c - x))) 0

(**
 *  min f(x) s.t. f(x) = \sum_{i=0}^{n-1} |c_i - x|
 *  Let C = {c_0, c_1, ..., c_{n-1}} be sorted
 *
 *  f'(x) = - \sum_{i=0}^{n-1} sign(c_i - x)
 *
 *  x     | c_0             m         c_{n-1} |
 *  f'(x) |        -        0        +        |
 *
 *  And f'(x) = 0 if there exists k such that
 *  \sum_{i=0}^{k} \sign(c_i - x) = - \sum_{i=k+1}{n-1} \sign(c_i - x)
 *  i.e. k = n / 2.
 *  Then, f'(x) = 0 <=> x = c_{n/2} = median(C)
 *)
let min_cost_diff sorted_a =
  let median = sorted_a.(Array.length sorted_a / 2) in
  cost_diff median sorted_a

(**
 *  min f(x) s.t. f(x) = \sum_{i=0}^{n-1} \sum_{k=0}^{|c_i - x|} k
 *  Let C = {c_0, c_1, ..., c_{n-1}} be sorted
 *
 *  f'(x) = \sum_{i=0}^{n-1} (c_i - x)(2|c_i - x| + 1) / 2|c_i - x|
 *  h(x)  = \sum_{i=0}^{n-1} (c_i - x)
 *
 *  x     | c_0             m         c_{n-1} |
 *  h(x)  |        -        0        +        |
 *  f'(x) |        -        0        +        |
 *
 *  h(x) = 0 <=> \sum_{i=0}^{n-1} (x - c_i) = 0
 *           <=> nx = \sum{i=0}^{n-1} c_i
 *           <=> x = mean(C)
 *)
let min_cost_sumdiff a =
  let mean = Array.fold_left (+) 0 a / Array.length a in
  let cost_lower = cost_sumdiff mean a in
  let cost_upper = cost_sumdiff (mean + 1) a in
  min cost_lower cost_upper

let aoc crabs =
  let a = Array.of_list crabs in
  Array.sort Int.compare a;
  min_cost_diff a |> Format.printf "Min cost diff: %d\n";
  min_cost_sumdiff a |> Format.printf "Min cost sumdiff: %d\n";
  ()


let () =
  try
    let fname = Array.get Sys.argv 1 in
    let crabs =
      let chan = open_in fname in
      let lines = parse chan in
      close_in chan; lines
    in aoc crabs
  with _ -> Format.eprintf "Invalid file.\n"
