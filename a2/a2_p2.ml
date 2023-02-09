



(* (* val zip : ('a list') -> ('b list) -> ('a * 'b) list option *)


  (* val get_strings : otoml -> (string * string) list *)
(* val is_uppercase : string -> bool *)
(* val get_uppercase_strings_1 : otoml -> string list *)
(* val get_uppercase_strings_2 : otoml -> string list *)
(* val omatch : opattern -> ovalue -> (string * ovalue) list option *) *)

let zip list1 list2 = 
  if List.length list1 <> List.length list2 then
    None
  else
  let rec loop l1 l2 acc = 
    match l1, l2 with
    | [], [] -> List.rev acc
    | h1::t1, h2::t2 -> loop t1 t2 ((h1,h2)::acc)
    | _, _ -> failwith "diff lengths"
  in Some (loop list1 list2 [])

(* val flatmap : ('a -> 'b list) -> 'a list -> 'b list *)
let flatmap fn list = 
  let rec flatten func list = 
    match list with 
    | [] -> []
    | h::t -> func h @ flatten func t
  in flatten fn list

  (* val flatmap_opt : ('a -> 'b list option) -> 'a list -> 'b list option *)
  let flatmap_opt fn list = 
    (* val flatten : ('a -> 'b list option) -> 'a list -> 'b list option *)
    let rec flatten func list = 
      match list with 
      | [] -> Some([])
      | h::t -> (match (func h, flatten func t) with
        | (Some(a), Some(lst)) -> Some(a @ lst)
        | (_, _) -> None
      )
    in flatten fn list
  






type ovalue =
(* a string constant, e.g., "foo" *)
| Str of string
(* an integer constant, e.g., 15 *)
| Int of int
(* table, e.g., {name1 = val1, name2 = val2} *)
| Table of okeyvalue list
(* list, e.g., [val1, val2] *)
| ListOf of ovalue list
and okeyvalue = string * ovalue
and otable = string * okeyvalue list
and otoml = otable list


let t = [ "table1", [ ("str", Str "hello")
; ("int", Int 5)
; ("table", Table ["a", Str "x"; "b", Str "Y"])
; ("list", ListOf [Int 1; Int 2] )]


; "table2", [ ("something", Str "maybe" )] ]   

  (* [("table1", "hello"), ("table1", "x"), ("table1", "y"), ("table2", "maybe")] *)

(* let rec loop_list tbl_name list acc = 
  match list with
  |[] -> acc
  |Str s::t -> loop_list tbl_name t ((tbl_name,s):: acc)
  |Int i::t -> acc
  |Table tbl :: t -> loop_table
  |ListOf l :: t -> acc *)

  

let rec loop_table tbl_name list acc = 
  match list with (* get the first table in the list head name*) 
    |[] -> acc
    |(str, Str s)::t -> loop_table tbl_name t (((tbl_name,s):: acc)) 
    |(str, Int i')::t -> loop_table tbl_name t acc
    |(str,Table tbl1)::t ->  loop_table tbl_name tbl1 (loop_table tbl_name t acc)
    |(str,ListOf listy)::t -> let rec loop_list list_ acc = match list_ with
                                |[] -> acc
                                |Str s::t -> loop_list t ((tbl_name,s):: acc)
                                |Int i::t -> acc
                                |Table tbl :: t -> loop_table tbl_name tbl (loop_list t acc )
                                |ListOf l :: t -> loop_list t acc
                                in loop_list listy (loop_table tbl_name t acc)
    let get_strings (data:otoml) = 
      let rec loop odata acc =
        match odata with (*loop through the list of tables *)
        | [] -> List.rev acc
        | (tblname, lil)::t ->loop t (loop_table tblname lil acc )  (* get the first table in the list head name*)            
      in loop data []

 let str = get_strings t


 let is_uppercase (str)  =
  let is_uppercase_char c =
    let code = Char.code c in
    code >= 65 && code <= 90
  in
  let rec loop s i =
    if i < 0 then true
    else if not (is_uppercase_char (String.get s i)) then false
    else loop s (i-1)
  in
  loop str (String.length str - 1)



let compose =
    fun f -> fun g -> (fun x -> f (g x))
let (%) = compose
let get_uppercase_strings_1  = 
  let rec loop recs = 
    match recs with 
    | [] -> []
    | (h1,h2) ::tail  -> if (is_uppercase h2 ) then h2:: loop tail else loop tail
  in (loop % get_strings)


  
let get_uppercase_strings_2 record = 
  let rec loop recs = 
    match recs with 
    | [] -> []
    | (h1,h2) ::tail  -> if (is_uppercase h2 ) then h2:: loop tail else loop tail
  in (get_strings record |> loop )