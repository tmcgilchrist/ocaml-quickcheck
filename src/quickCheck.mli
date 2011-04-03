
type config = {
  maxTest : int;
  maxFail : int;
  size : int -> int;
  every : Format.formatter -> int * QuickCheck_show.pretty_str list -> unit;
}

val quick : config
val verbose : config
val done_ : string -> int -> string list list -> unit
val tests : config ->
  QuickCheck_testable.result QuickCheck_gen.gen ->
  int ->
  int ->
  string list list ->
  unit

module Check :
  functor (T : QuickCheck_testable.TESTABLE) ->
    sig
      module E : sig
        val evaluate : T.t -> QuickCheck_testable.result QuickCheck_gen.gen
      end
      val check : config -> T.t -> unit
      val test : T.t -> unit
      val quickCheck : T.t -> unit
      val verboseCheck : T.t -> unit
    end

