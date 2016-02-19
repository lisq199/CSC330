(*
 solutions to assignment #2
*)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)

fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

fun all_except_option(st,list) =
    case list of
        [] => NONE
      | h::tail =>
        if same_string(h, st) then
            SOME tail
        else
            case all_except_option(st,tail) of
                NONE => NONE
              | SOME ls => SOME (h :: ls)

fun get_substitutions1 (l, st) =
    case l of
        [] => []
      | (h::t) =>
        case all_except_option(st,h) of
            NONE => get_substitutions1(t,st)
          | SOME l1 => l1 @ get_substitutions1(t,st)

fun get_substitutions2 (l, st) =
    let
        fun helper(l, accum) =
            case l of
                [] => accum
              | (h::t) =>
                case all_except_option(st,h) of
                    NONE => helper(t, accum)
                  | SOME l1 => helper(t, accum @ l1)
    in
        helper(l, [])
    end

fun similar_names (list, name:{first:string, middle: string, last:string}) =
    let
        fun create (r, l) =
            case (r,l) of
                (_, []) => []
              | ({first=f,middle=m,last=l}, hd::tail) =>
                {first=hd,middle=m,last=l} :: create(r,tail)
    in
        case name of
            {first=f,middle=m,last=l} => name::create(name, get_substitutions1(list,f))
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

fun card_color (c:card) =
    case c of
        (Hearts,_) => Red
      | (Diamonds,_) => Red
      | _ => Black

fun card_value (c:card):int =
    case c of
        (_,Jack) => 10
      | (_,Queen) => 10
      | (_,King) => 10
      | (_,Ace) => 11
      | (_,Num n) => n


fun remove_card (cs: card list, c: card, ex) =
    case cs of
        [] => raise ex
      | (head::tail) => if head = c then tail else head::remove_card(tail, c, ex)

fun all_same_color(cards) =
    case cards of
        [] => true
      | hd::[] => true
      | (hd::mid::tail) =>
        if (card_color(hd) = card_color(mid))  then
            all_same_color(mid::tail)
        else
            false

fun sum_cards(cards) =
    let
        fun helper(cards, acc)  =
            case cards of
                [] => acc
              | hd::tail => helper(tail, acc+card_value(hd))
    in
        helper(cards, 0)
    end

fun score(cards:card list,goal:int) =
    let
        val sum = sum_cards(cards)
    in
        (if (sum > goal) then 2 * (sum -goal) else goal - sum)
            div
            (if all_same_color(cards) then 2 else 1)
    end


fun officiate(cards, moves, goal) =
    let

        fun run_game(cards, moves, held) =
            case (moves,cards) of
                ([],_) => score(held, goal)
              | (Discard c::restMoves, _) => run_game(cards,
                                                      restMoves,
                                                      remove_card(held, c, IllegalMove))
              | (Draw::restMoves, []) =>  score(held, goal)
              | (Draw::restMoves, hd::restCards) =>
                let
                    val newHeld = hd :: held
                    val sumCards = sum_cards(newHeld)
                    val currentScore = score(newHeld, goal)
                in
                    if (sumCards> goal) then
                        currentScore
                    else
                        run_game(restCards, restMoves, newHeld)
                end
    in
        run_game(cards, moves, [])
    end
