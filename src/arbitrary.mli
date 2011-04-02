
module type ARBITRARY = sig
  type t
  val arbitrary : t Gen.gen
end

module Arbitrary_unit : sig
  type t = unit
  val arbitrary : unit Gen.gen
end

module Arbitrary_bool : sig
  type t = bool
  val arbitrary : bool Gen.gen
end

module Arbitrary_char : sig
  type t = char
  val arbitrary : char Gen.gen
end

module Arbitrary_string : sig
  type t = string
  val arbitrary : string Gen.gen
end

module Arbitrary_int : sig
  type t = int
  val arbitrary : int Gen.gen
end

module Arbitrary_float : sig
  type t = float
  val arbitrary : float Gen.gen
end

module Aribitrary_pair :
  functor (Fst : ARBITRARY) ->
    functor (Snd : ARBITRARY) ->
      sig
        type t = Fst.t * Snd.t
        val arbitrary : (Fst.t * Snd.t) Gen.gen
      end

module Aribitrary_triple :
  functor (Fst : ARBITRARY) ->
    functor (Snd : ARBITRARY) ->
      functor (Trd : ARBITRARY) ->
        sig
          type t = Fst.t * Snd.t * Trd.t
          val arbitrary : (Fst.t * Snd.t * Trd.t) Gen.gen
        end

module Arbitrary_list :
  functor (Elt : ARBITRARY) ->
    sig
      type t = Elt.t list
      val arbitrary : Elt.t list Gen.gen
    end
