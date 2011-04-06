
type 'a show = 'a -> string

val show_bool : bool show

val show_char : char show

val show_string : string show

val show_int : int show

val show_float : float show

val show_pair : 'a show -> 'b show -> 'a * 'b -> string

val show_triple : 'a show -> 'b show -> 'c show -> 'a * 'b * 'c -> string

val show_list : 'a show -> 'a list -> string

type 'a arbitrary = 'a QuickCheck_gen.gen

val arbitrary_unit : unit QuickCheck_gen.gen

val arbitrary_bool : bool QuickCheck_gen.gen

val arbitrary_char : char QuickCheck_gen.gen

val arbitrary_string : string QuickCheck_gen.gen

val arbitrary_int : int QuickCheck_gen.gen

val arbitrary_float : float QuickCheck_gen.gen

val arbitrary_pair : 'a arbitrary ->
                     'b arbitrary ->
                     ('a * 'b) QuickCheck_gen.gen

val arbitrary_triple : 'a arbitrary ->
                       'b arbitrary ->
                       'c arbitrary ->
                       ('a * 'b *'c) QuickCheck_gen.gen

val arbitrary_list : 'a arbitrary -> 'a list QuickCheck_gen.gen


type result = {
  ok : bool option;
  stamp : string list;
  arguments : string list;
}

type property = Prop of result QuickCheck_gen.gen

val nothing : result

val result : result -> property

module type TESTABLE = sig
  type t
  val property : t -> property
end

module Testable_unit : sig
  type t = unit
  val property : unit -> property
end

module Testable_bool : sig
  type t = bool
  val property : bool -> property
end

module Testable_result : sig
  type t = result
  val property : result -> property
end

module Testable_property : sig
  type t = property
  val property : 'a -> 'a
end

module Evaluate :
  functor (T : TESTABLE) -> sig
    val evaluate : T.t -> result QuickCheck_gen.gen
  end

module ForAll :
  functor (S : SHOW) ->
    functor (T : TESTABLE) ->
      sig
        module E : sig val evaluate : T.t -> result QuickCheck_gen.gen end
        val forAll : S.t QuickCheck_gen.gen -> (S.t -> T.t) -> property
      end

(* boo *)
module Testable_fun :
  functor (A : ARBITRARY) ->
    functor (S : SHOW with type t = A.t) ->
      functor (T : TESTABLE) ->
        sig
          module F : sig
              module E : sig
                val evaluate : T.t -> result QuickCheck_gen.gen
              end
              val forAll : S.t QuickCheck_gen.gen -> (S.t -> T.t) -> property
          end
          type t = A.t -> T.t
          val property : t -> property
        end

module Implies :
  functor (T : TESTABLE) -> sig
    val ( ==> ) : bool -> T.t -> property
  end

module Label :
  functor (T : TESTABLE) ->
    sig
      module E : sig
        val evaluate : T.t -> result QuickCheck_gen.gen
      end
      val label : string -> T.t -> property
    end

module Classify :
  functor (T : TESTABLE) -> sig
    module L : sig
      module E : sig
        val evaluate : T.t -> result QuickCheck_gen.gen
      end
      val label : string -> T.t -> property
    end
    val classify : bool -> string -> T.t -> property
    val trivial : bool -> T.t -> property
  end

module Collect :
  functor (S : SHOW) ->
    functor (T : TESTABLE) -> sig
      module L : sig
        module E : sig
          val evaluate : T.t -> result QuickCheck_gen.gen
        end
        val label : string -> T.t -> property
      end
      val collect : S.t -> T.t -> property
    end




type config = {
  maxTest : int;
  maxFail : int;
  size : int -> int;
  every : Format.formatter -> int * string list -> unit;
}

val quick : config
val verbose : config
val done_ : string -> int -> string list list -> unit
val tests : config ->
  result QuickCheck_gen.gen ->
  int ->
  int ->
  string list list ->
  unit

module Check :
  functor (T : TESTABLE) ->
    sig
      module E : sig
        val evaluate : T.t -> result QuickCheck_gen.gen
      end
      val check : config -> T.t -> unit
      val test : T.t -> unit
      val quickCheck : T.t -> unit
      val verboseCheck : T.t -> unit
    end

