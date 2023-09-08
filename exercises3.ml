fun int_to_real intList = map real intList;

fun ord_list charList = map ord charList;

fun square_list intList = map (fn x => x * x) intList;

fun mult_pairs intpairList = map (fn (a, b) => a * b) intpairList;

fun inc_list intList num = map (fn x => x + num) intList;

fun square_sum intList = foldl (op +) 0 (square_list intList);

fun list_or boolList = foldl (fn (a, b) => a orelse b) false boolList;

fun list_and boolList = foldl (fn (a, b) => a andalso b) true boolList;

fun list_xor boolList = foldl (fn (a, b) => (a andalso (not b)) orelse (b andalso (not a))) false boolList;

fun dup_list inputList = foldr (fn (a, b) => a::a::b) nil inputList;

fun my_length inputList = foldl (fn (_, acc) => acc + 1) 0 inputList;

fun abs_values intList = map (fn a => if a > 0 then real a else real (~ a)) intList;

fun true_count boolList = foldl (fn (el, count) => if el then count + 1 else count) 0 boolList;

fun max_pairs intpairList = foldr (fn ((a, b), acc) => if a > b then a::acc else b::acc) nil intpairList;

fun my_implode charList = foldr (fn (c, acc) => (str c) ^ acc) "" charList;

fun all_append inputList = foldr (fn (subList, acc) => subList @ acc) nil inputList;

fun max (first::rest) = foldr (fn (a, b) => if a > b then a else b) first (first::rest);

fun min (first::rest) = foldr (fn (a, b) => if a < b then a else b) first (first::rest);

fun member (element, inputList) = foldl (fn (a, res) => ((a = element) orelse res)) false inputList;

fun append aList bList = foldr (fn (a, acc) => a::acc) bList aList;

fun less (num, intList) = foldl (fn (a, acc) => if (a < num) then a::acc else acc) nil intList;

fun evens intList = foldl (fn (a, acc) => if (a mod 2 = 0) then a::acc else acc) nil intList;

fun convert pairList = foldr (fn ((a, b), (firsts, seconds)) => (a::firsts, b::seconds)) (nil, nil) pairList;

fun mymap f inList = foldr (fn (a, acc) => (f a)::acc) nil inList;

fun evaluate poly value = foldr (fn (a, res) => a + value*res) 0.0 poly;

fun my_map _ nil = nil
  | my_map f (first::rest) = (f first)::(my_map f rest);

fun my_foldr _ initial nil = initial
  | my_foldr f initial inList =
      let
        fun help nil  res  = res 
          | help (first::rest) res = f (first, help rest res)
      in
        help inList initial 
      end;

fun my_foldl _ initial nil = initial
  | my_foldl f initial inList =
      let
        fun help nil  res  = res 
          | help (first::rest) res = help rest (f (first, res))
      in
        help inList initial
      end

