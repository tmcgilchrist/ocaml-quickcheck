(*
 * QuickCheck_gen - different generators
 *)

(**

   Generator combinator and types.

**)

open QuickCheck_util

type 'a gen = Gen of (int -> 'a)


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
let choose_int32 = lift_gen Random.int32_range
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

let such_that_opt p gen =
  let rec try_ k n = match n with
    | 0 -> ret_gen None
    | n -> resize (2 * k + n) gen >>=
      (fun x -> if p x then ret_gen (Some x) else try_ (k+1) (n-1))
  in sized (try_ 0 % max 1)

let rec such_that p gen =
  such_that_opt p gen >>= (function
    | Some x -> ret_gen x
    | None -> sized (fun n -> resize (n+1) (such_that p gen)))

let frequency w_gens =
  let total = sum_int @@ List.map fst w_gens in
  let rec pick n lst = match lst with
    | (k, x)::_tail when n <= k -> x
    | (k, _)::tail -> pick (n - k) tail
    | _ -> failwith "pick used with empty list"
  in choose_int (1, total) >>= (fun n ->
    pick n w_gens)

let list gen = sized (fun n -> choose_int0 n >>= vector gen)

let list1 gen = sized (fun n -> choose_int (1, max 1 n) >>= vector gen)

let listN n gen = resize n (sized (fun n -> vector gen n))
