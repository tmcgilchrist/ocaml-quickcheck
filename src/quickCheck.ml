
open QuickCheck_util
open QuickCheck_gen


module type SHOW = sig
  type t
  val show : t -> string
end

module Show(P:SHOW) = struct
  type t = P.t
  let show x = P.show x
end

module Show_bool = struct
  type t = bool
  let show c = Printf.sprintf "%B" c
end

module Show_char = struct
  type t = char
  let show c = Printf.sprintf "%C" c
end

module Show_string = struct
  type t = string
  let show c = Printf.sprintf "%s" c
end

module Show_int = struct
  type t = int
  let show c = Printf.sprintf "%d" c
end

module Show_float = struct
  type t = float
  let show c = Printf.sprintf "%f" c
end

module Show_pair(Fst:SHOW)(Snd:SHOW) = struct
  type t = Fst.t * Snd.t
  let show (l,r) =
    let (sl, sr) = (Fst.show l, Snd.show r) in
    Printf.sprintf "(%s, %s)" sl sr
end

module Show_triple(Fst:SHOW)(Snd:SHOW)(Trd:SHOW) = struct
  type t = Fst.t * Snd.t * Trd.t
  let show (f, s, t) =
    let (sf, ss, st) = (Fst.show f, Snd.show s, Trd.show t) in
    Printf.sprintf "(%s, %s, %s)" sf ss st
end

module Show_list(Elt:SHOW) = struct
  type t = Elt.t list
  let show xs =
    Printf.sprintf "[%s]" (join_string_list (List.map Elt.show xs) ";")
end

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
    list Arbitrary_char.arbitrary >>=
      (fun cl -> ret_gen (charlist_to_string cl))
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
    list Elt.arbitrary
end


type result = {
  ok : bool option;
  stamp : string list;
  arguments : string list;
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

module ForAll(S:SHOW)(T:TESTABLE) = struct
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
  (S:SHOW with type t = A.t)
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
  every   : Format.formatter -> int * string list -> unit;
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
        List.iter (fun a -> Format.fprintf fmt "@ %s" a) l
      in
      Format.fprintf f "@[%d:@[<hov 2>%a@]@]@." n pargs args
    end
}

let done_ mesg ntest stamps =
  let percentage n m =
    Format.sprintf "%2d%%" ((100 * n) / m)
  in
  let entry (n, xs) =
    Format.sprintf "%s %s" (percentage n ntest) (String.concat ", " xs)
  in
  let pairLength l = match l with
    | (xs::_) as xss -> (List.length xss, xs)
    | [] -> assert false
  in
  let display l = match l with
    | [] -> ".\n"
    | [x] -> Format.sprintf " (%s).\n" x
    | xs ->
      String.concat "\n" ("." :: List.map (Format.sprintf "%s.") xs)
  in
  let not_null = function [] -> false | _ -> true in
  let table =
    List.filter not_null stamps |> List.sort compare |>
        List.group |> List.map pairLength |> List.sort compare |>
            List.rev |> List.map entry |> display
    in
    Format.printf "%s %d tests%s" mesg ntest table

let rec tests config gen ntest nfail stamps =
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
      | None ->
        tests config gen ntest (nfail+1) stamps
      | Some true ->
        tests config gen (ntest+1) nfail (result.stamp :: stamps)
      | Some false ->
        Format.printf "@[<2>Falsifiable, after %d tests:\n %s."
          ntest (join_string_list result.arguments "\n")
  end

module Check(T:TESTABLE) = struct
  module E=Evaluate(T)
  let check config a =
    tests config (E.evaluate a) 0 0 []
  let test = check quick
  let quickCheck  = test
  let verboseCheck = check verbose
end

