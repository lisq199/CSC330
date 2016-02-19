(* Solutions to assignment #1

d.m.german

*)

type DATE = {year:int, month:int, day: int}
exception InvalidParameter

fun is_older(a: DATE, b: DATE): bool =
    let
	val yearA =  #year a;
	val monthA = #month a;
	val dayA =   #day a;
	val yearB =  #year b;
	val monthB = #month b;
	val dayB =   #day b;
    in
        yearA > yearB orelse
        (yearA = yearB andalso monthA > monthB) orelse
        (yearA = yearB andalso monthA = monthB andalso dayA > dayB)
    end

fun in_month(date: DATE, month: int) : bool =
    #month date = month

fun number_in_month(dates: DATE list, month: int): int =
    if dates = [] then 0
    else
       (if in_month(hd dates, month) then 1 else 0) + number_in_month(tl dates, month)

fun number_in_months(dates: DATE list, months: int list): int =
    if months = [] then 0
    else
	number_in_month(dates, hd months) + number_in_months(dates, tl months)

fun dates_in_month(dates: DATE list, month: int): DATE list =
    if dates = [] then []
    else
	if in_month(hd dates, month)
	then hd dates::dates_in_month(tl dates, month)
	else dates_in_month(tl dates, month)

fun dates_in_months(dates: DATE list, months: int list): DATE list =
    if months = [] then []
    else
	dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)


fun get_nth(strings: string list, n: int ): string =
    if strings = [] orelse n <= 0
    then raise InvalidParameter
    else if n = 1
    then hd strings
    else get_nth(tl strings, n-1)


fun date_to_string(date: DATE): string =
    let
	val names = ["January", "February", "March", "April","May","June","July","August","September","October","November","December"];
    in
	get_nth(names, #month date) ^ " " ^ Int.toString(#day date)^", " ^Int.toString(#year date)
    end

exception Number_before_reaching_sum

fun number_before_reaching_sum(sum: int, values: int list):int =
    if values = [] then
        raise Number_before_reaching_sum
    else if (hd values) >= sum then
        0
    else
        1 + number_before_reaching_sum(sum - hd values, tl values)


val monthLen = [31,28,31,30,
		31,30,31,31,
		30,31,30,31];

fun what_month(dayOfYear: int) :int =
    number_before_reaching_sum(dayOfYear, monthLen) +1


fun month_range(day1: int, day2: int): int list =
    if (day1 > day2) then
        []
    else
        what_month(day1) :: month_range(day1+1, day2)


fun oldest(dates : DATE list) : DATE option =
    if dates = []
    then NONE
    else
	let
	    val h1 = hd dates;
	    val tail = tl dates;
	    val tailOldest = oldest(tail);
	in
	    if isSome(tailOldest) andalso is_older(valOf(tailOldest),h1) then
	        tailOldest
	    else
                SOME h1
	end

fun remove_dups(values: int list) : int list =
    if values = [] then []
    else
	let
	    val tail = tl values
	    val head = hd values
	in
	    if (List.exists (fn y => head = y) tail) then
                remove_dups(tail)
	    else
                head :: remove_dups(tail)
	end


fun number_in_months_challenge(dates: DATE list, months: int list): int =
    number_in_months(dates, remove_dups(months))

fun reasonable_date(date: DATE) : bool =
    let
	val year =  #year date;
	val month = #month date;
	val day =   #day date;
	fun isLeapYear() : bool =
	    (year mod 400) = 0 orelse ((year mod 100) <>0 andalso (year mod 4) = 0)
	fun validDay() : bool =
	    (* at this point we know the month is valid *)
	    let
		val lengthMonth = List.nth(monthLen, month-1)
	    in
		if month = 2
		then
		    if isLeapYear() then day <= 29 else day <= 28
		else
                    day >= 1 andalso day <= lengthMonth
	    end
    in
	year > 0 andalso month > 0 andalso month < 13 andalso validDay()
    end
