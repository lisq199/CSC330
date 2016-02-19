(* Assign 03 Provided Code *)

(*  Version 1.0 *)

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


(**** put all your code after this line ****)

fun only_capitals(sl: string list): string list =
    List.filter (fn s => Char.isUpper(String.sub(s, 0))) sl

fun longest_string1(sl: string list): string =
    List.foldl (
        fn (s1, s2) => if String.size(s1) > String.size(s2) then s1 else s2
    ) "" sl

fun longest_string2(sl: string list): string =
    List.foldl (
        fn (s1, s2) => if String.size(s1) >= String.size(s2) then s1 else s2
    ) "" sl

fun longest_string_helper(f: int * int -> bool) =
    fn sl: string list => List.foldl (
        fn (s1, s2) => if f(String.size(s1), String.size(s2)) then s1 else s2
    ) "" sl

val longest_string3 = longest_string_helper(fn (x, y) => x > y)

val longest_string4 = longest_string_helper(fn (x, y) => x >= y)

val longest_capitalized = longest_string1 o only_capitals

val rev_string = String.implode o List.rev o String.explode

fun first_answer f l =
    case l
     of [] => raise NoAnswer
      | e :: l' => (case f e
                     of NONE => first_answer f l'
                      | SOME v => v)

fun all_answers f l =
    let
        val applied = List.map f l
        fun has_none(l) =
            case l
             of [] => false
              | h :: t => (case h
                            of NONE => true
                             | SOME _ => has_none(t))
        fun safe_all_answers(applied) =
            case applied
             of [] => []
              | h :: t => (case h
                            of NONE => [] (* This will never happen *)
                             | SOME lst => lst @ safe_all_answers(t))
    in
        if has_none(applied) then NONE else SOME (safe_all_answers(applied))
    end

val count_wildcards = g (fn () => 1) (fn _ => 0)

val count_wild_and_variable_length = g (fn () => 1) String.size

fun count_some_var(s, p): int = g (fn () => 0) (
        fn x => if x = s then 1 else 0
    ) p

val check_pat =
    let
        fun get_strings p =
            case p
             of Variable x => [x]
              | TupleP ps => List.foldl (
                        fn (p, sl) => get_strings(p) @ sl
                    ) [] ps
              | ConstructorP(_,p) => get_strings(p)
              | _ => []
        fun has_repeats sl =
            case sl
             of [] => false
              | s :: sl' => (List.exists (fn s': string => s = s') sl')
                    orelse has_repeats(sl')
    in
        not o has_repeats o get_strings
    end

(*
 * Question 9 (a)
 * g takes f1: unit -> int, f2: string -> int and p: pattern, and returns 
 * an int. f1 and f2 are used to specify what number to be returned for 
 * each Wildcard and Variable in p respectively. The return value is the 
 * sum of all those numbers for all the patterns wrapped in p.
 *)

fun match(v, p) =
    case p
     of Wildcard => SOME []
      | Variable s => SOME [(s, v)]
      | UnitP => (case v
            of Unit => SOME []
             | _ => NONE)
      | ConstP x => (case v
            of Const y => if x = y then SOME [] else NONE
             | _ => NONE)
      | TupleP ps => (case v
            of Tuple vs =>
                if List.length(vs) = List.length(ps) then
                    all_answers match (ListPair.zip(vs, ps))
                else NONE
             | _ => NONE)
      | ConstructorP(s1, p) => (case v
            of Constructor(s2, v) =>
                if s1 = s2 then match(v, p) else NONE
             | _ => NONE)

fun first_match v ps =
    SOME (first_answer (fn p => match(v, p)) ps) handle NoAnswer => NONE
