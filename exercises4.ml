datatype cards = Clubs | Diamonds | Hearts | Spades;

fun card_name Clubs = "Clubs"
  | card_name Diamonds = "Diamonds"
  | card_name Hearts = "Hearts"
  | card_name Spades = "Spades";

datatype number = IntVal of int | RealVal of real;

fun plus (IntVal x) (IntVal y) = RealVal ((real x) + (real y))
  | plus (IntVal x) (RealVal y) = RealVal ((real x) + y)
  | plus (RealVal x) (IntVal y) = RealVal (x + (real y))
  | plus (RealVal x) (RealVal y) = RealVal (x + y);

datatype int_nest = INT of int | LIST of int_nest list;

fun addup (INT x) = x
  | addup (LIST (nil)) = 0
  | addup (LIST ((INT first)::rest)) = first + addup (LIST rest);

datatype 'element mylist = NIL | CONS of 'element * 'element mylist;

fun product NIL = 1
  | product (CONS (first, rest)) = first * product rest;

fun reverse NIL = NIL
  | reverse (CONS (first, rest)) = 
      let
        fun help (NIL, NIL) = NIL
          | help (NIL, CONS(first, rest)) = CONS(first, rest)
          | help (CONS(first, rest), NIL) = CONS(first, rest)
          | help (CONS(a, b), CONS(c, d)) = 
              help (CONS(c, CONS(a,b)), d);
      in
        help (CONS (first, NIL), rest)
      end;

fun append NIL NIL = NIL
  | append (CONS(a,b)) NIL = CONS(a,b)
  | append NIL (CONS(a,b)) = CONS(a,b)
  | append (CONS(a,b)) (CONS(c,d)) = append b (CONS(a, CONS(c, d)));

datatype 'data tree = Empty | Node of ('data tree) * 'data * ('data tree);

fun append_all Empty = nil
  | append_all (Node(left, data, right)) = append_all(left) @ [data] @ append_all(right);

fun isComplete Empty = true
  | isComplete (Node(left, _, Empty)) = false
  | isComplete (Node(Empty, _, right)) = false
  | isComplete (Node(left, _, right)) = isComplete(left) andalso isComplete(right);

datatype 'data bst = BSTEmpty | BSTNode of ('data bst) * 'data * ('data bst);

fun makeBST nil _ = BSTEmpty
  | makeBST [first] _ = BSTNode(BSTEmpty, first, BSTEmpty)
  | makeBST (first::sec::rest) compare = 
      if compare(first, sec)
      then BSTNode(makeBST (sec::rest) compare, first, BSTEmpty)
      else BSTNode(BSTEmpty, first, makeBST (sec::rest) compare);

fun searchBST BSTEmpty _ _ = false
  | searchBST (BSTNode(left, data, right)) compare el =
      (data = el) orelse
      if compare(data, el) 
      then searchBST left compare el
      else searchBST right compare el;
      



