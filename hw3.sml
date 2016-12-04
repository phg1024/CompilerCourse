(* Coursera Programming Languages, Homework 3, Provided Code *)

(*
val g = fn : (unit -> int) -> (string -> int) -> pattern -> int
val only_capitals = fn : string list -> string list
val longest_string1 = fn : string list -> string
val longest_string2 = fn : string list -> string
val longest_string_helper = fn : (int * int -> bool) -> string list -> string
val longest_string3 = fn : string list -> string
val longest_string4 = fn : string list -> string
val longest_capitalized = fn : string list -> string
val rev_string = fn : string -> string
val first_answer = fn : (’a -> ’b option) -> ’a list -> ’b
val all_answers = fn : (’a -> ’b list option) -> ’a list -> ’b list option
val count_wildcards = fn : pattern -> int
val count_wild_and_variable_lengths = fn : pattern -> int
val count_some_var = fn : string * pattern -> int
val check_pat = fn : pattern -> bool
val match = fn : valu * pattern -> (string * valu) list option
val first_match = fn : valu -> pattern list -> (string * valu) list option
*)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let
	val r = g f1 f2
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)
fun only_capitals(slist: string list) : string list =
  List.filter (fn s => Char.isUpper(String.sub(s, 0))) slist

fun longest_string1(slist: string list) : string =
  foldl (fn(a, b) => if String.size(a) > String.size(b) then a else b) "" slist

fun longest_string2(slist: string list) : string =
  foldl (fn(a, b) => if String.size(a) >= String.size(b) then a else b) "" slist

fun longest_string_helper f slist =
  foldl (fn(a, b) => if f(String.size(a), String.size(b)) then a else b) "" slist

fun longest_string3(slist: string list) : string =
  let
    fun comp(a, b) = a > b
  in
    longest_string_helper comp slist
  end

fun longest_string4(slist: string list) : string =
  let
    fun comp(a, b) = a >= b
  in
    longest_string_helper comp slist
  end

fun longest_capitalized(slist: string list) : string =
  let
    fun comp(a, b) = a > b
  in
    ((longest_string_helper comp) o only_capitals) slist
  end

fun rev_string s = (String.implode o rev o String.explode) s

fun first_answer f lst =
  case lst of
    [] => raise NoAnswer
  | x::xs => case f(x) of
              NONE => first_answer f xs
            | SOME v => v

fun all_answers f lst =
  foldl (fn(a, b) => if isSome b andalso isSome a then SOME ((valOf a) @ (valOf b)) else NONE) (SOME []) (List.map f lst)

fun count_wildcards p = g (fn _ => 1) (fn _ => 0) p

fun count_wild_and_variable_lengths p = g (fn _ => 1) (fn x => String.size(x)) p

fun count_some_var(s, p) = g (fn _ => 0) (fn x => if x = s then 1 else 0) p

fun check_pat p =
  let
    fun get_vars p =
      case p of
          Wildcard          => []
        | Variable x        => [x]
        | TupleP ps         => List.foldl (fn(p, vars) => vars @ (get_vars p)) [] ps
        | ConstructorP(_,p) => get_vars p
        | _                 => []
    fun has_duplicates(l) =
      case l of
        [] => false
      | head::tail => if List.exists (fn(x) => x = head) tail then true else has_duplicates tail
  in
    not ((has_duplicates o get_vars) p)
  end

fun match(v, p) =
  case p of
    Wildcard => SOME []
  | Variable s => SOME [(s, v)]
  | UnitP => (case v of
              Unit => SOME []
            | _ => NONE)
  | ConstP x => (case v of
                  Const x => SOME []
                | _ => NONE)
  | TupleP ps => (case v of
                  Tuple vs => all_answers match (ListPair.zip(vs, ps))
                | _ => NONE)
  | ConstructorP (s1, p) => (case v of
                              Constructor (s2, vu) => if s1 = s2 then match(vu, p) else NONE
                            | _ => NONE)

fun first_match v ps = SOME (first_answer (fn p => match(v, p)) ps)
                       handle NoAnswer => NONE
