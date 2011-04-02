
open Util
open Show
open Gen

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

type config = {
  maxTest : int;
  maxFail : int;
  size    : int -> int;
  every   : Format.formatter -> int * pretty_str list -> unit;
}

let quick = {
  maxTest = 100;
  maxFail = 1000;
  size    = (fun n -> 3 + n / 2);
  every   = (fun _ (_, _) -> ())
}

let verbose = {
  quick with
    every = begin fun f (n, args) ->
      let pargs fmt l =
        List.iter (fun a -> Format.fprintf fmt "@ %a" a ()) l
      in
      Format.fprintf f "@[%d:@[<hov 2>%a@]@]@." n pargs args
    end
}

let done_ : string -> int -> string list list -> unit =
  fun mesg ntest stamps ->
    let percentage n m =
      Format.sprintf "%2d%%" ((100 * n) / m)
    in
    let entry (n, xs) =
      Format.sprintf "%s %s" (percentage n ntest) (String.concat ", " xs)
    in
    let pairLength = function
        (xs::_) as xss -> (List.length xss, xs)
      | [] -> assert false
    in
    let display = function
        [] -> ".\n"
      | [x] -> Format.sprintf " (%s).\n" x
      | xs ->
          String.concat "\n" ("." :: List.map (Format.sprintf "%s.") xs)
    in
    let not_null = function [] -> false | _ -> true in
    let table =
      display
        (List.map entry
           (List.rev
              (List.sort compare
                 (List.map pairLength
                    (List.group
                       (List.sort compare
                          (List.filter not_null
                             stamps)))))))
    in
    Format.printf "%s %d tests%s" mesg ntest table

let rec tests : config -> result gen -> int -> int -> string list list -> unit =
    fun config gen ntest nfail stamps ->
      if ntest = config.maxTest
      then done_ "OK, passed" ntest stamps
      else if nfail = config.maxFail
      then done_ "Arguments exhausted after" nfail stamps
      else begin
        let result = generate (config.size ntest) gen in
        let () =
          Format.printf "@[%a@]@?" config.every (ntest, result.arguments)
        in
        match result.ok with
            None ->
              tests config gen ntest (nfail+1) stamps
          | Some true ->
              tests config gen (ntest+1) nfail (result.stamp :: stamps)
          | Some false ->
              let p f = function
                  [] -> ()
                | h::t ->
                    h f ();
                    List.iter (fun s -> Format.fprintf f "@ %a" s ()) t
              in
              Format.printf "@[<2>Falsifiable, after %d tests:@ %a@]@."
                ntest p result.arguments
      end

module Check(T:TESTABLE) = struct
  module E=Evaluate(T)
  let check : config -> T.t -> unit =
    fun config a ->
      tests config (E.evaluate a) 0 0 []
  let test = check quick
  let quickCheck = test
  let verboseCheck = check verbose
end

