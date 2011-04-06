
open QuickCheck

let prop_revrev xs =
  List.rev (List.rev xs) = xs

let prop_mem xs = match xs with
  | [] -> true
  | (x::_) -> List.mem x xs

let prop_str_copy s = s = (String.copy s)

(* for generating random int lists *)
let al = arbitrary_list arbitrary_int

(* for printing out int lists *)
let sl = show_list show_int

(* for being able to test (int list -> bool) *)
let testable_list_to_bool = testable_fun al sl testable_bool

let cl = quickCheck testable_list_to_bool

let testable_str_to_bool = testable_fun arbitrary_string show_string testable_bool
let cs = quickCheck testable_str_to_bool

let () =
  cl prop_revrev;
  cl prop_mem;
  cs prop_str_copy

