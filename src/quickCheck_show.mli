
type pretty_str = Format.formatter -> unit -> unit

module type PSHOW = sig
  type t
  val show : t -> pretty_str
end

module type SHOW = sig
  type t
  val show : t -> string
end

module Show :
  functor (P : PSHOW) -> sig
    type t = P.t
    val show : P.t -> string
  end

module PShow_bool : sig
  type t = bool
  val show : bool -> Format.formatter -> unit -> unit
end

module PShow_char : sig
  type t = char
  val show : char -> Format.formatter -> unit -> unit
end

module PShow_string : sig
  type t = string
  val show : string -> Format.formatter -> unit -> unit
end

module PShow_int : sig
  type t = int
  val show : int -> Format.formatter -> unit -> unit
end

module PShow_float : sig
  type t = float
  val show : float -> Format.formatter -> unit -> unit
end

module PShow_pair :
  functor (Fst : PSHOW) ->
    functor (Snd : PSHOW) ->
      sig
        type t = Fst.t * Snd.t
        val show : Fst.t * Snd.t -> Format.formatter -> unit -> unit
      end

module PShow_triple :
  functor (Fst : PSHOW) ->
    functor (Snd : PSHOW) ->
      functor (Trd : PSHOW) ->
        sig
          type t = Fst.t * Snd.t * Trd.t
          val show :
            Fst.t * Snd.t * Trd.t -> Format.formatter -> unit -> unit
        end

module PShow_list :
  functor (Elt : PSHOW) ->
    sig
      type t = Elt.t list
      val show : Elt.t list -> Format.formatter -> unit -> unit
    end
