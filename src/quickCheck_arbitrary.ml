
open Gen
open Util

module type ARBITRARY = sig
  type t
  val arbitrary : t gen
end

module Arbitrary_unit = struct
  type t = unit
  let arbitrary = ret_gen ()
end

module Arbitrary_bool = struct
  type t = bool
  let arbitrary = elements [true; false]
end

module Arbitrary_char = struct
  type t = char
  let arbitrary =
    choose_int (32,255) >>= fun c ->
      ret_gen (Char.chr c)
end

module Arbitrary_string = struct
  type t = string
  let arbitrary =
    sized choose_int0 >>=
      vector Arbitrary_char.arbitrary >>=
      (fun cl -> ret_gen (list_to_string cl))
end

module Arbitrary_int = struct
  type t = int
  let arbitrary = sized (fun n -> choose_int (-n, n))
end

module Arbitrary_float = struct
  type t = float
  let arbitrary =
    Arbitrary_int.arbitrary >>= fun a ->
      Arbitrary_int.arbitrary >>= fun b ->
        sized choose_int0 >>= fun c ->
          ret_gen
            (float a +. (float b /. (float c +. 1.)))
end

module Aribitrary_pair(Fst:ARBITRARY)(Snd:ARBITRARY) = struct
  type t = Fst.t * Snd.t
  let arbitrary =
    Fst.arbitrary >>= fun v1 ->
      Snd.arbitrary >>= fun v2 ->
        ret_gen (v1,v2)
end

module Aribitrary_triple(Fst:ARBITRARY)(Snd:ARBITRARY)(Trd:ARBITRARY) = struct
  type t = Fst.t * Snd.t * Trd.t
  let arbitrary =
    Fst.arbitrary >>= fun v1 ->
      Snd.arbitrary >>= fun v2 ->
        Trd.arbitrary >>= fun v3 ->
          ret_gen (v1,v2,v3)
end

module Arbitrary_list(Elt:ARBITRARY) = struct
  type t = Elt.t list
  let arbitrary =
    sized choose_int0 >>= vector Elt.arbitrary
end
