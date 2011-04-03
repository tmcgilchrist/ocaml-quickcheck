
open Testable

type config = {
  maxTest : int;
  maxFail : int;
  size : int -> int;
  every : Format.formatter -> int * Show.pretty_str list -> unit;
}

val quick : config
val verbose : config
val done_ : string -> int -> string list list -> unit
val tests : config -> result Gen.gen -> int -> int -> string list list -> unit

module Check :
  functor (T : TESTABLE) ->
    sig
      module E : sig val evaluate : T.t -> result Gen.gen end
      val check : config -> T.t -> unit
      val test : T.t -> unit
      val quickCheck : T.t -> unit
      val verboseCheck : T.t -> unit
    end

