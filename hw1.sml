(*
val is_older = fn : (int * int * int) * (int * int * int) -> bool
val number_in_month = fn : (int * int * int) list * int -> int
val number_in_months = fn : (int * int * int) list * int list -> int
val dates_in_month = fn : (int * int * int) list * int -> (int * int * int) list
val dates_in_months = fn : (int * int * int) list * int list -> (int * int * int) list
val get_nth = fn : string list * int -> string
val date_to_string = fn : int * int * int -> string
val number_before_reaching_sum = fn : int * int list -> int
val what_month = fn : int -> int
val month_range = fn : int * int -> int list
val oldest = fn : (int * int * int) list -> (int * int * int) option
*)

type Date = int * int * int
fun year(date : Date) : int = #1 date
fun month(date : Date) : int = #2 date
fun day(date : Date) : int = #3 date

fun is_older(date1 : Date, date2 : Date) : bool =
    (year date1) < (year date2)
  orelse
    (year date1) = (year date2)
    andalso (month date1) < (month date2)
  orelse
    (year date1) = (year date2)
    andalso (month date1) = (month date2)
    andalso (day date1) < (day date2)

fun number_in_month(dates : Date list, whichmonth : int) : int =
  case dates of
     [] => 0
   | h :: t =>
      (if (month h) = whichmonth then 1 else 0) + number_in_month(t, whichmonth)

fun number_in_months(dates : Date list, months : int list) : int =
  case months of
     [] => 0
   | h :: t => number_in_month(dates, h) + number_in_months(dates, t)

fun dates_in_month(dates: Date list, whichmonth : int) : Date list =
  case dates of
    [] => []
  | h :: t =>
    (if (month h) = whichmonth then [h] else []) @ dates_in_month(t, whichmonth)

fun dates_in_months(dates: Date list, months: int list) : Date list =
  case months of
    [] => []
  | h :: t => dates_in_month(dates, h) @ dates_in_months(dates, t)

fun get_nth(strings: string list, n: int) : string =
  if n = 1 then hd strings
  else get_nth(tl strings, n-1)

val months_as_strings =
  ["January", "February", "March", "April",
  "May", "June", "July", "August", "September",
  "October", "November", "December"]
fun date_to_string(date: Date) =
  get_nth(months_as_strings, month date)
  ^ " "
  ^ Int.toString(day date)
  ^ ", "
  ^ Int.toString(year date)

fun number_before_reaching_sum(sum: int, numbers: int list) =
  if sum <= (hd numbers) then 0
  else 1 + number_before_reaching_sum(sum - (hd numbers), tl numbers)

val days_in_months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
fun what_month(whichday: int): int =
  number_before_reaching_sum(whichday, days_in_months)

fun month_range(day1: int, day2: int) =
  if day1 > day2 then []
  else what_month(day1) :: month_range(day1 + 1, day2)

fun oldest(dates: Date list) : Date option =
  case dates of
    [] => NONE
  | h :: t =>
    let
      val oldest_in_tail = oldest(tl dates)
    in
      if isSome(oldest_in_tail) then
        if is_older(h, valOf(oldest_in_tail)) then
          SOME(h)
        else oldest_in_tail
      else SOME(h)
    end

fun unique(months: int list): int list =
  case months of
    [] => []
  | h :: t =>
    let
      fun eq a b = a = b
    in
      if List.exists (eq h) t then unique t
      else h :: unique t
    end

fun number_in_months_challenge(dates : Date list, months : int list) : int =
  number_in_months(dates, unique months)

fun dates_in_months_challenge(dates: Date list, months: int list) : Date list =
  dates_in_months(dates, unique months)
