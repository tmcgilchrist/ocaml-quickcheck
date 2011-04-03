
type 'a gen = Gen of (int -> 'a)

val sized : (int -> 'a gen) -> 'a gen

val resize : int -> 'a gen -> 'a gen

val promote : ('a -> 'b gen) -> ('a -> 'b) gen

val variant : int -> 'a gen -> 'a gen

val generate : int -> 'a gen -> 'a

val map_gen : ('a -> 'b) -> 'a gen -> 'b gen

val ret_gen : 'a -> 'a gen

val (>>=) : 'a gen -> ('a -> 'b gen) -> 'b gen

val lift_gen : ('a -> 'b) -> 'a -> 'b gen

val choose_int : int * int -> int gen

val choose_int0 : int -> int gen

val choose_char : char * char -> char gen

val choose_float : float * float -> float gen

val  elements : 'a list -> 'a gen

val vector : 'a gen -> int -> 'a list gen

val oneof : 'a gen list -> 'a gen

val such_that : ('a -> bool) -> 'a gen -> 'a gen
