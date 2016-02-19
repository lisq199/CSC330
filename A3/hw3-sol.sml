(* Coursera Programming Languages, Homework 3, Provided Code *)

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


(* 1 *)
fun only_capitals xs =
    List.filter (fn x => Char.isUpper (String.sub (x,0))) xs

(* 2 *)
fun longest_string1 xs =
    List.foldl (fn (x,y) => if String.size (x) > String.size (y) then x else y) "" xs

(* 3 *)
fun longest_string2 xs =
    List.foldl (fn (x,y) => if String.size (x) >= String.size (y) then x else y) "" xs

(* 4 *)
fun longest_string_helper f xs =
    List.foldl (fn (x,y) => if f(String.size(x),String.size(y)) then x else y) "" xs

val longest_string3 = longest_string_helper (op >)

val longest_string4 = longest_string_helper (op >=)

(* 5 *)

val longest_capitalized = longest_string1 o only_capitals

(* 6 *)
val rev_string = implode o rev o explode

(* 7 *)
fun first_answer f xs =
    case xs of
        [] => raise NoAnswer
      | x::xs' =>
        case f x of
            NONE => first_answer f xs'
          | SOME x => x

(* 8 *)
fun all_answers f xs =
    let
        fun helper f xs acc =
            case xs of
                [] => SOME acc
              | x::xs' =>
                case f x of
                    NONE => NONE
                  | SOME y => helper f xs' (acc@y)
    in
        helper f xs []
    end

(* 9.b *)
val count_wildcards = g (fn x => 1) (fn x => 0)

(* 9.c *)
val count_wild_and_variable_lengths = g (fn x => 1) (fn x => String.size x)

(* 9.d *)
fun count_some_var (st, p) =
    g (fn x => 0) (fn x => if (x = st) then 1 else 0) p


(* 10 *)
fun check_pat p =
    let
        fun pat_strings p =
            case p of
	        Wildcard          => []
              | Variable x        => [x]
              | TupleP ps         => List.foldl (fn (p,i) => (pat_strings(p) @ i)) [] ps
              | ConstructorP(_,p) => pat_strings p
              | _                 => []
        fun exists_in st xs =
            List.exists (fn y:string => st = y) xs
        fun not_repeated xs =
            case xs of
                [] => true
              | x::xs' => (not (exists_in x xs')) andalso (not_repeated xs')
    in
        not_repeated(pat_strings(p))
    end


(* 11 *)
fun match (v, p) =
    let
      fun match_tuple(v,p)  =
          case (v,p) of
            (x::xs', y::ys') => (match (x , y)) :: match_tuple(xs', ys')
            | _ =>  []
    in
      case (p,v) of
	  (Wildcard,_)             => SOME []
        | (Variable x,_)           => SOME [(x, v)]
        | (UnitP, Unit)            => SOME []
        | (ConstP n, Const vv)     => if vv = n then SOME [] else NONE
        | (TupleP ps, Tuple vl)    =>
          if List.length(vl)       = List.length(ps) then
            all_answers (fn x      => x) (match_tuple(vl,ps))
          else
            NONE
        | (ConstructorP(s1,p1), Constructor(s2,v1)) =>
          if (s1 = s2) then match(v1,p1) else NONE
        |  _                       => NONE
    end


(* 12 *)
fun first_match v  pl =
    SOME (first_answer (fn p => match(v,p)) pl)
    handle noAnswer => NONE
