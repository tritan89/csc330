(*#utop_prompt_dummy
  let _ = UTop.set_show_box false *)

(************************************************************************)
(** Nested patterns **)

(* Two ways NOT to do zip3: *)
let rec meh_zip3_v1 (xs, ys, zs) =
  if xs = [] && ys = [] && zs = [] then
    []
  else if xs = [] || ys = [] || zs = [] then
    failwith "zip3 length mismatch"
  else
    (List.hd xs, List.hd ys, List.hd zs) :: meh_zip3_v1 (List.tl xs, List.tl ys, List.tl zs)

let rec meh_zip3_v2 (xs, ys, zs) =
  match xs with (* like life without && and || *)
  | [] -> (
      match ys with
      | [] -> (
        match zs with
        | [] -> []
        | _ -> failwith "zip3 length mismatch")
      | _ -> failwith "zip3 length mismatch")
  | x :: xs' -> (
      match ys with
      | [] -> failwith "zip3 length mismatch"
      | y :: ys' -> (
          match zs with
          | [] -> failwith "zip3 length mismatch"
          | z :: zs' -> (x, y, z) :: meh_zip3_v2 (xs', ys', zs')))

(* Instead... nested patterns give you "and" by matching the entire pattern,
   along with nested data extraction *)
let rec zip3 list_triple =
  match list_triple with
  | [], [], [] -> []
  | x :: xs', y :: ys', z :: zs' -> (x, y, z) :: zip3 (xs', ys', zs')
  | _ -> failwith "zip3 length mismatch"

(* the inverse function is also elegant *)
let rec unzip3 xyzs =
  match xyzs with
  | [] -> ([], [], [])
  | (x, y, z) :: xyzs' ->
      let xs, ys, zs = unzip3 xyzs' in
      (x :: xs, y :: ys, z :: zs)

;;


(* a couple more examples with lists *)
let rec is_sorted xs =
  match xs with
  | [] -> true
  | x :: [] -> true
  | head :: neck :: rest -> head <= neck && is_sorted (neck :: rest)

let rec cumulative_sum xs =
  match xs with
  | [] -> xs
  | x :: [] -> xs
  | head :: neck :: rest -> head :: cumulative_sum ((head + neck) :: rest)

type sign = P | N | Z

let multsign (x1, x2) =
  let sign_of_num x =
    if x = 0 then Z else if x > 0 then P else N
  in
  match (sign_of_num x1, sign_of_num x2) with
  | Z, _ -> Z
  | _, Z -> Z
  | P, P -> P
  | N, N -> P
  | _ -> N (* questionable style; we are okay with it *)

let rec length xs =
  match xs with [] -> 0 | _ :: xs -> 1 + length xs

let rec sum_pair_list xs =
  match xs with
  | [] -> 0
  | (x, y) :: xs' -> x + y + sum_pair_list xs'

;;






(************************************************************************)
(** Tail Recursion **)


let rec fact n =
  if n = 0 then 1 else n * fact (n - 1)

let e = (fact [@tailcall]) 5

let fact_tc n =
  let rec loop (n, acc) =
    if n = 0 then acc else loop (n - 1, acc * n)
  in
  loop (n, 1)

let e = fact_tc 5

;;


let rec last xs =
  match xs with
  | [] -> failwith "last: empty list"
  | x :: [] -> x
  (* you can check for tailcall with @tailcall here *)
  | _ :: xs' -> (last [@tailcall]) xs'

let e = last [1; 2; 3]

;;


let rec sum xs =
  match xs with [] -> 0 | i :: xs' -> i + sum xs'

let sum_tc xs =
  let rec f (xs, acc) =
    match xs with [] -> acc | i :: xs' -> f (xs', i + acc)
  in
  f (xs, 0)

let rec rev xs =
  match xs with [] -> [] | x :: xs' -> rev xs' @ [ x ]

let rev_tc xs =
  let rec loop (xs, acc) =
    match xs with [] -> acc | x :: xs' -> loop (xs', x :: acc)
  in
  loop (xs, [])





(************************************************************************)
(** Exceptions **)

exception MyUndesirableCondition
exception MyOtherException of int * int

let oh_no () =
  raise MyUndesirableCondition
let oh_no_with_info () =
  raise (MyOtherException (7, 42))
let catch_example () =
  try oh_no () with MyUndesirableCondition -> 0

let catch_example_with_info () =
  try oh_no_with_info () with MyOtherException (x, y) -> x + y

let boo () =
  try oh_no () with MyOtherException (x, y) -> x + y
let hd xs =
  match xs with [] -> raise MyUndesirableCondition | x :: _ -> x
let foo1 = hd [ 3; 4; 5 ]

(* let foo2 = hd [] (* exception *) *)

let bar1 =
  try Some (hd [ 3; 4; 5 ]) with MyUndesirableCondition -> None
let bar2 =
  try Some (hd []) with MyUndesirableCondition -> None

let rec maxlist (xs, ex) = (* int list * exn -> int *)
  match xs with
  | [] -> raise ex
  | x :: [] -> x
  | x :: xs' ->
      let m = maxlist (xs', ex) in
      if x > m then x else m

let m1 = maxlist ([ 3; 4; 5 ], MyUndesirableCondition)

let m2 =
  try maxlist ([ 3; 4; 5 ], MyUndesirableCondition)
  with MyUndesirableCondition -> 42

let m3 = maxlist ([], MyUndesirableCondition)

let m4 =
  try maxlist ([], MyUndesirableCondition) with MyUndesirableCondition -> 42
