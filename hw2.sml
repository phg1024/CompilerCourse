(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(*
val all_except_option = fn : string * string list -> string list option
val get_substitutions1 = fn : string list list * string -> string list
val get_substitutions2 = fn : string list list * string -> string list
val similar_names = fn : string list list * {first:string, last:string, middle:string}
-> {first:string, last:string, middle:string} list
val card_color = fn : card -> color
val card_value = fn : card -> int
val remove_card = fn : card list * card * exn -> card list
val all_same_color = fn : card list -> bool
val sum_cards = fn : card list -> int
val score = fn : card list * int -> int
val officiate = fn : card list * move list * int -> int
*)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
fun all_except_option(s: string, slist: string list): string list option =
  case slist of
    [] => NONE
  | h::t =>
    if same_string(s, h)
    then SOME t
    else
      case all_except_option(s, t) of
          NONE => NONE
        | SOME lst => SOME (h::lst)

fun get_substitutions1(substitutions: string list list, s: string): string list =
  case substitutions of
    [] => []
  | h::t =>
    case all_except_option(s, h) of
        NONE => get_substitutions1(t, s)
      | SOME lst => lst @ get_substitutions1(t, s)

fun get_substitutions2(substitutions: string list list, s: string): string list =
  let
    fun helper(subs: string list list, s: string, mem: string list) =
      case subs of
        [] => mem
      | h::t =>
        case all_except_option(s, h) of
          NONE => helper(t, s, mem)
        | SOME lst => helper(t, s, mem @ lst)
  in
    helper(substitutions, s, [])
  end

fun similar_names(substitutions: string list list, fullname: {first:string, last:string, middle:string}) =
  let
    val {first, last, middle} = fullname
    val all_subs = get_substitutions2(substitutions, first)
    fun gen_name(subs: string list) =
      case subs of
        [] => []
      | h::t => {first=h, last=last, middle=middle} :: gen_name(t)
  in
    fullname::gen_name(all_subs)
  end

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw

exception IllegalMove

(* put your solutions for problem 2 here *)
fun card_color(c: card): color =
  case c of
    (Clubs, _) => Black
  | (Spades, _)  => Black
  | (_, _) => Red

fun card_value(c: card): int =
  case c of
    (_, Num x) => x
  | (_, Ace) => 11
  | (_, _) => 10

fun remove_card(cs: card list, c: card, e) =
  case cs of
    [] => raise e
  | h::t => if h = c then t else h :: remove_card(t, c, e)

fun all_same_color(cs: card list) =
  case cs of
    [] => true
  | h::[] => true
  | h1::h2::t => (card_color h1 = card_color h2) andalso all_same_color(h2::t)

fun sum_cards(cs: card list) =
  let
    fun helper(clist: card list, s: int) =
      case clist of
        [] => s
      | h::t => helper(t, s + (card_value h))
  in
    helper(cs, 0)
  end

fun score(cs: card list, goal: int) =
  let
    val sum_val = sum_cards(cs)
    val pre_score = if sum_val > goal
                    then 3 * (sum_val - goal)
                    else goal - sum_val
  in
    if all_same_color(cs) then pre_score div 2 else pre_score
  end

fun officiate(cs: card list, ms: move list, goal: int) =
  let
    fun play_game(held: card list, cards: card list, moves: move list) =
      case moves of
        [] => score(held, goal)
      | ms_h::ms_t =>
        case ms_h of
          Draw =>
            (case cards of
              [] => score(held, goal)
            | cs_h::cs_t =>
                if sum_cards(cs_h::held) > goal
                then score(cs_h::held, goal)
                else play_game(cs_h::held, cs_t, ms_t))
        | Discard x => play_game(remove_card(held, x, IllegalMove), cards, ms_t)
  in
    play_game([], cs, ms)
  end

fun sum_cards_extended(cs: card list) =
  let
    fun add_to_list(scores: int list, cv: int) =
      case scores of
        [] => []
      | h::t => (h+cv) :: add_to_list(t, cv)
    fun helper(clist: card list, s: int list) =
      case clist of
        [] => s
      | h::t =>
        case h of
          (_, Ace) => helper(t, add_to_list(s, 1) @ add_to_list(s, 11))
        | (_, _) => helper(t, add_to_list(s, card_value h))
  in
    helper(cs, [0])
  end

fun score_challenge(cs: card list, goal: int) =
  let
    fun map_fun(f, l) =
      case l of
        [] => []
      | h::t => (f h) :: map_fun(f, t)
    val sum_vals = sum_cards_extended(cs)
    fun pre_score_func(x) = if x > goal then 3 * (x - goal) else goal - x
    val pre_scores = map_fun(pre_score_func, sum_vals)
    fun min_val(l) =
      case l of
        h::[] => h
      | h::t =>
        let
          val min_rest = min_val t
        in
          if h < min_rest then h else min_rest
        end
    val pre_score = min_val pre_scores
  in
    if all_same_color(cs) then pre_score div 2 else pre_score
  end

fun officiate_challenge(cs: card list, ms: move list, goal: int) =
  let
    fun play_game(held: card list, cards: card list, moves: move list) =
      case moves of
        [] => score_challenge(held, goal)
      | ms_h::ms_t =>
        case ms_h of
          Draw =>
            (case cards of
              [] => score_challenge(held, goal)
            | cs_h::cs_t =>
                if sum_cards(cs_h::held) > goal
                then score_challenge(cs_h::held, goal)
                else play_game(cs_h::held, cs_t, ms_t))
        | Discard x => play_game(remove_card(held, x, IllegalMove), cards, ms_t)
  in
    play_game([], cs, ms)
  end

fun careful_player(cs: card list, goal: int): move list =
  let
    exception NotImplementedError
  in
    raise NotImplementedError
  end
