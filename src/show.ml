
type pretty_str = Format.formatter -> unit -> unit

module type PSHOW = sig
  type t
  val show : t -> pretty_str
end

module type SHOW = sig
  type t
  val show : t -> string
end

module Show(P:PSHOW) = struct
  open Buffer
  open Format
  type t = P.t
  let show x  =
    let f _ =
      let str = contents stdbuf in
      clear stdbuf;
      str
    in
    clear stdbuf;
    kfprintf f str_formatter "@[%a@]@?" (P.show x) ()
end

module PShow_bool = struct
  type t = bool
  let show c fmt () =
    Format.fprintf fmt "%B" c
end

module PShow_char = struct
  type t = char
  let show c fmt () =
    Format.fprintf fmt "%C" c
end

module PShow_string = struct
  type t = string
  let show c fmt () =
    Format.fprintf fmt "%s" c
end

module PShow_int = struct
  type t = int
  let show c fmt () =
    Format.fprintf fmt "%d" c
end

module PShow_float = struct
  type t = float
  let show c fmt () =
    Format.fprintf fmt "%f" c
end

module PShow_pair(Fst:PSHOW)(Snd:PSHOW) = struct
  type t = Fst.t * Snd.t
  let show (l,r) fmt () =
    let (sl, sr) = (Fst.show l, Snd.show r) in
    let pp = Format.fprintf in
    let snd f i = pp f ", %a" i () in
    Format.fprintf fmt "(%a%a)" sl () snd sr
end

module PShow_triple(Fst:PSHOW)(Snd:PSHOW)(Trd:PSHOW) = struct
  type t = Fst.t * Snd.t * Trd.t
  let show (f, s, t) fmt () =
    let (sf, ss, st) = (Fst.show f, Snd.show s, Trd.show t) in
    let pp = Format.fprintf in
    let trd ft i = pp ft ", %a" i () in
    let snd ft i = pp ft ", %a%a" i () trd st in
    Format.fprintf fmt "(%a%a)" sf () snd ss
end

module PShow_list(Elt:PSHOW) = struct
  type t = Elt.t list
  let show xs fmt () =
    let pp = Format.fprintf in
    match List.map Elt.show xs with
      | [] -> pp fmt "[]"
      | a1::an ->
        let pprest f =
          List.iter (fun e -> pp f ";@ %a" e ())
        in
        pp fmt "[%a%a]" a1 () pprest an
end
