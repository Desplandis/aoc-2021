let bitvector_size = Sys.int_size - 1

let get i bit = i land (1 lsl bit)
let set i bit = i lor (1 lsl bit)
let res i bit = i land (lnot (1 lsl bit))
let mask bit = (1 lsl bit) - 1

(* Invariant: bits from [size] to [Sys.int_size] are not set. *)
type t = { size: int; bits: int }

let init size f =
  if size < 0 || size > bitvector_size then invalid_arg "Bitvector.create"
  else
    let rec aux acc bit =
      if bit >= size then acc
      else aux ((if f bit then set else res) acc bit) (bit + 1)
    in { size; bits = aux 0 0 }

let length bv = bv.size

let get bv bit = (get bv.bits bit) <> 0
let set bv bit b = { bv with bits = (if b then set else res) bv.bits bit }

let not bv = { bv with bits = (lnot bv.bits) land (mask bv.size) }

let to_int bv = bv.bits

let foldi f acc bv =
  let rec aux acc bit =
    if bit < 0 then acc
    else aux (get bv bit |> f bit acc) (bit - 1)
  in
  aux acc (bv.size - 1)

let pp fmt bv =
  (* TODO: Format to binary *)
  Format.fprintf fmt "@[{ size = %d; bits = %d }@]" bv.size bv.bits
