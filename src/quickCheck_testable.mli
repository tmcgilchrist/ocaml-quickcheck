

type result = {
  ok : bool option;
  stamp : string list;
  arguments : QuickCheck_show.pretty_str list;
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
  functor (S : QuickCheck_show.PSHOW) ->
    functor (T : TESTABLE) ->
      sig
        module E : sig val evaluate : T.t -> result QuickCheck_gen.gen end
        val forAll : S.t QuickCheck_gen.gen -> (S.t -> T.t) -> property
      end

module Testable_fun :
  functor (A : QuickCheck_arbitrary.ARBITRARY) ->
    functor (S : sig type t = A.t val show : t -> QuickCheck_show.pretty_str end) ->
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
  functor (S : QuickCheck_show.SHOW) ->
    functor (T : TESTABLE) -> sig
      module L : sig
        module E : sig
          val evaluate : T.t -> result QuickCheck_gen.gen
        end
        val label : string -> T.t -> property
      end
      val collect : S.t -> T.t -> property
    end

