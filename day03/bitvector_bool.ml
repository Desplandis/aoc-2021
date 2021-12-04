let bitvector_size = Sys.int_size - 1

type t = { size: int; bits: bool Parray.t }

let init size f =
  if size < 0 || size > bitvector_size then invalid_arg "Bitvector.create"
  else { size; bits = Parray.init size f }

let length bv = bv.size

let get bv = Parray.get bv.bits
let set bv bit b = { bv with bits = Parray.set bv.bits bit b }

let foldi f acc bv =
  let rec aux acc bit =
    if bit < 0 then acc
    else aux (f bit acc (get bv bit)) (bit - 1)
  in aux acc (bv.size - 1)

let to_int = foldi (fun bit acc b -> if b then acc lor (1 lsl bit) else acc) 0
let not bv =
  { bv with
    bits = foldi (fun bit acc b -> not b |> Parray.set acc bit) bv.bits bv
  }

let pp fmt bv =
  foldi (fun _ _ b -> Format.fprintf fmt "%c" (if b then '1' else '0'))
    () bv
