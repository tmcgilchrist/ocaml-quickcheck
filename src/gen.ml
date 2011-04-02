
open Util

type 'a gen = Gen of (int -> 'a)

(* generator functions *)

let sized f = Gen (fun n ->
  let Gen m = f n in
  m n)

let resize n (Gen m) = Gen (fun _ -> m n)

let promote f =
  Gen (fun n ->
    fun a ->
      let Gen m = f a in
      m n)

let variant _v (Gen m) = Gen (fun n -> m n)

let generate n (Gen m) =
  let size = Random.int n in
  m size

let map_gen f (Gen m) =
  Gen (fun n ->
    let v = m n in
    f v)

let ret_gen a = Gen (fun _n -> a)

let (>>=) (Gen m) k =
  Gen (fun n ->
    let v = m n in
    let Gen m' = k v in
    m' n)

let lift_gen f a = Gen (fun _ -> f a)

let choose_int = lift_gen Random.int_range
let choose_int0 = lift_gen Random.int
let choose_char = lift_gen Random.char_range
let choose_float = lift_gen Random.float_range

let elements xs = map_gen (List.nth xs) (choose_int0 (List.length xs))

let vector (Gen gelt) l =
  Gen (fun n ->
    let rec gen acc = function
      |    0 -> acc
      | l -> gen (gelt n :: acc) (l-1)
    in gen [] l)

let oneof gens = elements gens >>= fun x -> x
