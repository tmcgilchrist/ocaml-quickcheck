
open QuickCheck
open QuickCheck_arbitrary
open QuickCheck_show
open QuickCheck_testable

let prop_revrev xs =
  List.rev (List.rev xs) = xs

let prop_mem xs = match xs with
  | [] -> true
  | (x::_) -> List.mem x xs

let prop_str_copy s =
  s = (String.copy s)

(* for generating random int lists *)
module AL = Arbitrary_list(Arbitrary_int)
(* for printing out int lists *)
module SL = PShow_list(PShow_int)
(* for being able to test (int list -> bool) *)
module Testable_list_to_bool =
  Testable_fun
    (AL)
    (SL)
    (Testable_bool)
module CL = Check(Testable_list_to_bool)

module Testable_str_to_bool =
  Testable_fun
    (Arbitrary_string)
    (PShow_string)
    (Testable_bool)

module CS = Check(Testable_str_to_bool)


let () =
  CL.quickCheck prop_revrev;
  CL.quickCheck prop_mem;
  CS.quickCheck prop_str_copy

