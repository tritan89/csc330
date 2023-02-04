
module type FSET = sig 
  type 'a set
  exception EmptySet
   val empty_set : ('a -> 'a -> int) -> 'a set
 val is_empty : 'a set -> bool
  val size : 'a set -> int 
  val insert : 'a set -> 'a -> 'a set
  val remove : 'a set -> 'a -> 'a set
  (* val union : 'a set -> 'a set -> 'a set *)
  (* val intersect : 'a set -> 'a set -> 'a set *)
  (* val from_list : ('a -> 'a -> int) -> 'a list -> 'a set *)
  (* val to_list : 'a set -> 'a list *)
  (* val map : ('a -> 'a) -> 'a set -> 'a set *)
  (* val fold : ('a -> 'b -> 'a) -> 'a -> 'b set -> 'a   *)
end

module FuncSet : FSET = struct
  type 'a set =  {cmp:('a -> 'a -> int); list_:'a list}
  exception EmptySet
  let empty_set compare = {cmp = compare; list_ = [] }
  let is_empty group  = group.list_ = []
  
  let size group = List.length group.list_
(* figure out how to add e  *)
  let insert group element = 
    if List.mem element group.list_ then
      group
    else
      (* add sort unique *)
      let added = element:: group.list_ in
      {cmp = group.cmp; list_= added}
      
      
     
      
       
      

      
  let remove group element =
    let rec loop(g, e) =  
      match g.list with 
      | [] -> []
      | h :: tail -> 
        (if h = element then
          loop(tail, e)
        else
          h:: loop(tail, e)
        )


  
    let rec union_h (set1, set2) = 
    match set1 with 
    | [] -> []
    | h:: tail -> 

      (if List.mem h set2 then 
        union_h(set1, set2)
        else
        h:: union_h(set1, set2)
      )
        
    let union g1 g2 = 
    let just1 = union_h(g1, g2) in
    let merge = union_h( g2, just1) in 
    merge
       
    let intersect = ...

    let from_list : ('a -> 'a -> int) -> 'a list -> 'a set
    let to_list : 'a set -> 'a list
    let map : ('a -> 'a) -> 'a set -> 'a set
    let fold : ('a -> 'b -> 'a) -> 'a -> 'b set -> 'a 
    
      


end



