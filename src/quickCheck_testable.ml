
open QuickCheck_show
open QuickCheck_gen
open QuickCheck_arbitrary

type result = {
  ok : bool option;
  stamp : string list;
  arguments : pretty_str list;
}

type property = Prop of result gen

module type TESTABLE = sig
  type t
  val property : t -> property
end

let nothing : result = {ok=None; stamp=[]; arguments=[]}

let result : result -> property =
  fun res -> Prop (ret_gen res)

module Testable_unit = struct
  type t = unit
  let property () = result nothing
end

module Testable_bool = struct
  type t = bool
  let property b = result {nothing with ok=Some b}
end

module Testable_result = struct
  type t = result
  let property r = result r
end

module Testable_property = struct
  type t = property
  let property p = p
end

module Evaluate(T:TESTABLE) = struct
  let evaluate : T.t -> result gen =
    fun a ->
      let Prop gen = T.property a in
      gen
end

module ForAll(S:PSHOW)(T:TESTABLE) = struct
  module E = Evaluate(T)
  let forAll : S.t gen -> (S.t -> T.t) -> property =
  fun gen body ->
    let argument a res =
      { res with arguments = S.show a ::res.arguments }
    in
    Prop
      (gen >>= fun a ->
         E.evaluate (body a) >>= fun res ->
           ret_gen (argument a res))
end

module Testable_fun
  (A:ARBITRARY)
  (S:PSHOW with type t = A.t)
  (T:TESTABLE) =
struct
  module F = ForAll(S)(T)
  type t = A.t -> T.t
  let property : t -> property =
    fun f ->
      F.forAll A.arbitrary f
end

module Implies(T:TESTABLE) = struct
  let (==>) : bool -> T.t -> property =
    fun b a ->
      if b
      then T.property a
      else Testable_unit.property ()
end

module Label(T:TESTABLE) = struct
  module E = Evaluate(T)
  let label : string -> T.t -> property =
    fun s a ->
      let add r = {r with stamp = s :: r.stamp } in
      let a' = E.evaluate a in
      Prop (map_gen add a')
end

module Classify(T:TESTABLE) = struct
  module L = Label(T)
  let classify : bool -> string -> T.t -> property =
    function
        true -> L.label
      | false -> fun _ -> T.property
  let trivial : bool -> T.t -> property =
    fun b -> classify b "trivial"
end

module Collect(S:SHOW)(T:TESTABLE) = struct
  module L = Label(T)
  let collect : S.t -> T.t -> property =
    fun v -> L.label (S.show v)
end
