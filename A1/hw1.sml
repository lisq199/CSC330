(* if you use this function to compare two strings (returns true if the same
   string), then you avoid some warning regarding polymorphic comparison  *)

fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for Part 1 here *)

(* Polymorphic function used more than once *)
fun is_in_list(e, l): bool =
    case l
     of [] => false
      | e' :: l' => if e' = e then true else is_in_list(e, l');

fun all_except_option(s: string, sl: string list): string list option =
    let
        fun safe_all_except_option(s: string, sl: string list) : string list =
            case sl
             of [] => []
              | s' :: sl' => if s' = s then sl'
                    else s' :: safe_all_except_option(s, sl')
    in
        if is_in_list(s, sl) then
            SOME (safe_all_except_option(s, sl))
        else NONE
    end;

fun get_substitutions1(sll: string list list, s: string): string list =
    case sll
     of [] => []
      | sl :: sll' => (case all_except_option(s, sl)
                        of NONE => []
                         | SOME sl' => sl') @ get_substitutions1(sll', s);

fun get_substitutions2(sll: string list list, s: string): string list =
    let
        fun helper_get_sub(sll: string list list, result: string list) =
            case sll
             of [] => result
              | sl :: sll' => case all_except_option(s, sl)
                               of NONE => helper_get_sub(sll', result)
                                | SOME sl' => helper_get_sub
                                        (sll', result @ sl')
    in
        helper_get_sub(sll, [])
    end;

fun similar_names(sll: string list list, {first = f, middle = m, last = l}) =
    let
        fun get_full_name_list(subs: string list) =
            case subs
             of [] => []
              | sub :: subs' => {first = sub, middle = m, last = l}
                    :: get_full_name_list(subs')
    in
        get_full_name_list(f :: get_substitutions1(sll, f))
    end;

(************************************************************************)
(* Game  *)

(* you may assume that Num is always used with valid values 2, 3, ..., 10 *)

datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw


exception IllegalMove

(* put your solutions for Part 2 here *)

fun card_color(s: suit, _: rank): color =
    case s
     of Clubs => Black
      | Spades => Black
      | _ => Red;

fun card_value(_: suit, r: rank): int =
    case r
     of Num i => i
      | Ace => 11
      | _ => 10;

fun remove_card(cs: card list, c: card, e: exn): card list =
    let
        fun safe_remove_card(cs: card list, c: card) =
            case cs
             of [] => []
              | c' :: cs' => if c' = c then cs'
                    else c' :: safe_remove_card(cs', c)
    in
        if is_in_list(c, cs) then safe_remove_card(cs, c)
        else raise e
    end;

fun all_same_color(cs: card list): bool =
    case cs
     of [] => true
      | [c] => true
      | c1 :: c2 :: cs' => card_color(c1) = card_color(c2)
            andalso all_same_color(c2 :: cs');

fun sum_cards(cs: card list): int =
    let
        fun helper_sum_cards(cs: card list, result: int) =
            case cs
             of [] => result
              | c :: cs' => helper_sum_cards(cs', result + card_value(c))
    in
        helper_sum_cards(cs, 0)
    end;

fun score(cs: card list, goal: int): int =
    let
        val sum = sum_cards(cs)
        val pre = if sum > goal then (sum - goal) * 2 else goal - sum 
    in
        pre div (if all_same_color(cs) then 2 else 1)
    end;

fun officiate(cs: card list, ms: move list, goal: int): int =
    0; (*TODO*)
