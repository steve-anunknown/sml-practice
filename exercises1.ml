fun cube a: int = a * a * a;

fun cubereal a: real = a * a * a;

fun fourth inputList = (hd (tl (tl (tl inputList))));

fun min_of_3 (a, b, c) = if (a < b)
                         then (if (a < c)
                               then a
                               else c)
                         else (if (b < c) 
                               then b 
                               else c);

fun cut_2nd (a, b, c) = (a, c); 

fun third_char inputString = (hd (tl (tl (explode inputString))))

fun cycle_1st inputList =
    if null inputList then nil
    else (tl inputList) @ [hd inputList];

fun sort_3  (a:real, b:real, c:real) =
    if (a < b)
    then (if (b < c) then [a, b, c]
          else (if (a < c) then [a, c, b] else [c, a, b]))
    else (if (a < c) then [b, a, c]
          else (if (c < b) then [c, b, a] else [b, c, a]));

fun delete_3rd inputList = (tl (tl (tl inputList)));

fun square_sum n = n * n + square_sum (n-1);

fun cycle_over inputList n = cycle_over (cycle_1st inputList) (n-1);

fun power(a:real, b:int) = a * (power (a, (b-1)));

fun max inputList = 
    let 
        fun help (rest, curr) = if null rest
                                then curr
                                else (if ((hd rest) < curr)
                                      then help ((tl rest), curr)
                                      else help ((tl rest), (hd rest)));
    in
        help ((tl inputList), (hd inputList))
    end

fun isPrime n =
    let fun help num divisor = if (num * num > divisor) then true
                           else (if (num mod divisor = 0) then true
                                 else help num (divisor + 2))
    in
        (n <=2 orelse (n mod 2) = 0) orelse (help n 3)
    end

fun select (inputList, predicate) = if (predicate (hd inputList))
                                    then hd::(select ((tl inputList), predicate))
                                    else select ((tl inputList), predicate)
