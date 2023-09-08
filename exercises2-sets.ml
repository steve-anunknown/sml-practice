fun member (_, nil) = false
  | member (element, first::rest) = (element = first) orelse (member (element, rest));

fun union (set1, nil) = set1
  | union (nil, set2) = set2
  | union (el1::rest1, el2::rest2) =
      if (el1 = el2)
      then (el1::(union (rest1, rest2)))
      else (el1::el2::(union (rest1, rest2)));

fun intersection (set1, nil) = nil
  | intersection (nil, set2) = nil
  | intersection (el1::rest1, el2::rest2) = 
      if (el1 = el2)
      then (el1::(intersection (rest1, rest2)))
      else (intersection (rest1, rest2));

fun powerset nil = [nil]
  | powerset (el::rest) =
      let
        fun prependAll (_, nil) = nil
          | prependAll (element, subset::rest) =
              (element::subset)::(prependAll (element, rest))
        val powersetRest = powerset rest;
      in
        powersetRest@(prependAll (el, powersetRest))
      end

