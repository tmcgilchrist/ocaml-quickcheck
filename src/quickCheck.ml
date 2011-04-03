
open Util
open Testable

type config = {
  maxTest : int;
  maxFail : int;
  size    : int -> int;
  every   : Format.formatter -> int * Show.pretty_str list -> unit;
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

let rec tests config gen ntest nfail stamps =
  if ntest = config.maxTest
  then done_ "OK, passed" ntest stamps
  else if nfail = config.maxFail
  then done_ "Arguments exhausted after" nfail stamps
  else begin
    let result = Gen.generate (config.size ntest) gen in
    let () =
      Format.printf "@[%a@]@?" config.every (ntest, result.arguments)
    in
    match result.ok with
      | None ->
        tests config gen ntest (nfail+1) stamps
      | Some true ->
        tests config gen (ntest+1) nfail (result.stamp :: stamps)
      | Some false ->
        let p f l = match l with
          | [] -> ()
          | h::t ->
            h f (); List.iter (fun s -> Format.fprintf f "@ %a" s ()) t in
        Format.printf "@[<2>Falsifiable, after %d tests:@ %a@]@."
          ntest p result.arguments
  end

module Check(T:TESTABLE) = struct
  module E=Evaluate(T)
  let check config a =
    tests config (E.evaluate a) 0 0 []
  let test = check quick
  let quickCheck  = test
  let verboseCheck = check verbose
end

module Gen = Gen
module Show = Show
module Testable = Testable
module Arbitrary = Arbitrary

