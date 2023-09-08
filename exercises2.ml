fun member (_, nil) = false
  | member (element, first::rest) = (element = first) orelse (member (element, rest));

fun less (_, nil) = nil
  | less (element, first::rest) =
      if (first < element)
      then (first)::(less (element, rest))
      else (less (element, rest));
fun more (_, nil) = nil
  | more (element, first::rest) =
      if (first >= element)
      then (first)::(more (element, rest))
      else (more (element, rest));

fun repeats nil = false
  | repeats [_] = false
  | repeats (first::second::rest) = (first = second) orelse (repeats (second::rest));

fun evaluate (nil, _) = 0.0
  | evaluate (first::rest, num) = first + num*(evaluate (rest, num));

fun quickSort nil = nil
  | quickSort (pivot::rest) =
      let
        fun less (_, nil) = nil
          | less (element, first::rest) =
              if (first < element)
              then (first)::(less (element, rest))
              else (less (element, rest));
        fun more (_, nil) = nil
          | more (element, first::rest) =
              if (first >= element)
              then (first)::(more (element, rest))
              else (more (element, rest));
        val smaller = less (pivot, rest);
        val bigger  = more (pivot, rest);
      in
        (quickSort smaller) @ [pivot] @ (quickSort bigger)
      end

fun quickSortAlt (nil, _) = nil
  | quickSortAlt (pivot::rest, compare) =
      let
        fun positive (_, nil) = nil
          | positive (element, first::rest) =
              if (compare (element, first))
              then (first)::(positive (element, rest))
              else (positive (element, rest));
        fun negative(_, nil) = nil
          | negative(element, first::rest) =
              if (not (compare (element, first)))
              then (first)::(negative (element, rest))
              else (negative (element, rest));
        val no  = negative (pivot, rest);
        val yes = positive (pivot, rest);
      in
        (quickSortAlt (no, compare)) @ [pivot] @ (quickSortAlt (yes, compare))
      end



