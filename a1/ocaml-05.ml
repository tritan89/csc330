(*#utop_prompt_dummy
  let _ = UTop.set_show_box false *)

(************************************************************************)
(** Matching **)

(* Previous lecture: variants can be recursive, describing recursive data structures like trees *)
type expr =
  | Constant of int
  | Negate of expr
  | Add of expr * expr
  | Mul of expr * expr

let rec eval e =
  match e with
  | Constant i -> i
  | Negate e1 -> -eval e1
  | Add (e1, e2) -> eval e1 + eval e2
  | Mul (e1, e2) -> eval e1 * eval e2

let rec max_const (e : expr) : int =
  let max (x, y) = if x > y then x else y in
  match e with
  | Constant i -> i
  | Negate e1 -> max_const e1
  | Add (e1, e2) -> max (max_const e1, max_const e2)
  | Mul (e1, e2) -> max (max_const e1, max_const e2)

let rec has_const_not_under_add e =
  match e with
  | Constant i -> true
  | Negate e1 -> has_const_not_under_add e1
  | Add (e1, e2) -> false
  | Mul (e1, e2) -> has_const_not_under_add e1 || has_const_not_under_add e2

let rec number_of_adds e =
  match e with
  | Constant i -> 0
  | Negate e1 -> number_of_adds e1
  | Add (e1, e2) -> 1 + number_of_adds e1 + number_of_adds e2
  | Mul (e1, e2) -> number_of_adds e1 + number_of_adds e2

let example_exp = Add (Constant 19, Negate (Constant 4))
let example_ans = eval example_exp
let example_addcount = number_of_adds (Mul (example_exp, example_exp))

;;


(* Same features already used can almost define option *)
type int_option = NoInt | OneInt of int

let rec sum_int_options1 xs =
  if xs = [] then 0
  else
    match List.hd xs with
    | NoInt -> sum_int_options1 (List.tl xs)
    | OneInt i -> i + sum_int_options1 (List.tl xs)

let test1 = sum_int_options1 [ NoInt; OneInt 7; NoInt; OneInt 2; OneInt 1 ]

;;

(* in fact, we /can/ define our own polymorphic variant types *)
type 'a my_option = MyNone | MySome of 'a

let rec sum_int_options2 xs =
  if xs = [] then 0
  else
    match List.hd xs with
    | MyNone -> sum_int_options2 (List.tl xs)
    | MySome i -> i + sum_int_options2 (List.tl xs)

let test2 = sum_int_options2 [ MyNone; MySome 7; MyNone; MySome 2; MySome 1 ]

;;


(* indeed, the option type constructor is not "built in" at all; just in standard library *)
(* type 'a option = None | Some of 'a *)
(* from now on, use pattern-matching for options *not* the previous
    way we showed to use them *)
let rec sum_int_options3 xs =
  if xs = [] then 0
  else
    match List.hd xs with
    | None -> sum_int_options3 (List.tl xs)
    | Some i -> i + sum_int_options3 (List.tl xs)

let test3 = sum_int_options3 [ None; Some 7; None; Some 2; Some 1 ]

;;






(* similarly, we can define our own polymorphic list type *)
type 'a my_list = Empty | Cons of 'a * 'a my_list

let rec sum_int_options4 xs =
  match xs with
  | Empty -> 0
  | Cons (x, xs') -> (
      match x with
      | None -> sum_int_options4 xs'
      | Some i -> i + sum_int_options4 xs')

let test4 =
  sum_int_options4
    (Cons (None, Cons (Some 7, Cons (None, Cons (Some 2, Cons (Some 1, Empty))))))

(* this is exactly how built-in lists are defined /except/ special syntax [] and :: !
   So yes, we can pattern-match with those constructors and should no longer use = [],
   List.hd, or List.tl (!!) *)
let rec sum_int_options5 xs =
  match xs with
  | [] -> 0
  | x :: xs' -> (  (* parentheses are optional *)
      match x with
      | None -> sum_int_options5 xs'
      | Some i -> i + sum_int_options5 xs')

let test5 = sum_int_options5 [ None; Some 7; None; Some 2; Some 1 ]

(* spoiler alert: nested patterns can make this even more concise
   we aren't /quite/ there yet, but this is the style we expect on hw2 *)
let rec sum_int_options6 xs =
  match xs with
  | [] -> 0
  | None :: xs' -> sum_int_options6 xs'
  | Some i :: xs' -> i + sum_int_options6 xs'

let test6 = sum_int_options6 [ None; Some 7; None; Some 2; Some 1 ]

;;





(* Pattern-matching is the normal ML way to use lists; let's revisit prior functions *)
let rec length xs =
  match xs with
  | [] -> 0
  | x :: xs' -> 1 + length xs'

let rec append (xs, ys) =
  match xs with [] -> ys | x :: xs' -> x :: append (xs', ys)

let rec concat ss =
  match ss with
  | [] -> ""
  | s :: ss' -> s ^ concat ss'

(* Pattern-matching for each-of types (tuples shown; records can also be pattern-matched)*)

(* terrible style never used: one-arm match expressions *)
let sum_triple1 tr = match tr with x, y, z -> x + y + z

(* appropriate style: let expression syntax is /actually/
   let p = e1 in e2 *)
let sum_triple2 tr =
  let x, y, z = tr in
  x + y + z

(* even better when useful: can put a pattern right in the function binding:
   let rec f p = e
*)
let sum_triple3 (x, y, z) = x + y + z

(* in fact, thanks to a convenient fib, that's what we have done since lecture 2 !!! *)

;;


(* and one more nested-patterns spoiler *)
let rec sum_pairs xs =
  match xs with
  | [] -> 0
  | (x, y) :: xs' -> x + y + sum_pairs xs'

(* cute example of expressiveness of functions actually taking one tuple *)
let rotate_left (x, y, z) = (y, z, x)
let rotate_right tr = rotate_left (rotate_left tr)

(* just as never use one-branch match expressions with each-of patterns,
   it is also usually bad style to use let expressions with one-of patterns
   -- get a warning at compile-time plus a run-time exception if match fails *)
let get_risky1 opt =
  match opt with
  | None -> failwith "nopes"
  | Some v -> v

let get_risky2 opt =
  let (Some v) = opt in
  v

let get_risky3 (Some v) = v

get_risky1 None
