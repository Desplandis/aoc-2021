module Loc = struct
  type t = int * int
  let pos row col = (row, col)
  let compare = compare
  let pp fmt (row, col) = Format.fprintf fmt "(%d, %d)" row col
end

module Line = struct
  type t = Loc.t * Loc.t

  let is_vertical ((x1, _), (x2, _)) = x1 = x2
  let is_horizontal ((_, y1), (_, y2)) = y1 = y2
  let is_diagonal ((x1, y1), (x2, y2)) = abs (x2 - x1) = abs (y2 - y1)
  let is_hv l = is_vertical l || is_horizontal l

  let dx ((x1, _), (x2, _)) = x2 - x1
  let dy ((_, y1), (_, y2)) = y2 - y1
  let slope ((x1, y1), (x2, y2)) = (y2 - y1) / (x2 - x1)

  let pp fmt (l1, l2) = Format.fprintf fmt "%a -> %a" Loc.pp l1 Loc.pp l2
end
