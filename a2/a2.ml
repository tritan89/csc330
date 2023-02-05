
module type FSET = sig 
  type 'a set
  exception EmptySet
   val empty_set : ('a -> 'a -> int) -> 'a set
 val is_empty : 'a set -> bool
  val size : 'a set -> int 
  val insert : 'a set -> 'a -> 'a set
  val remove : 'a set -> 'a -> 'a set
  val union : 'a set -> 'a set -> 'a set
  val intersect : 'a set -> 'a set -> 'a set
  val from_list : ('a -> 'a -> int) -> 'a list -> 'a set
  val to_list : 'a set -> 'a list
  (* val map : ('a -> 'a) -> 'a set -> 'a set *)
  (* val fold : ('a -> 'b -> 'a) -> 'a -> 'b set -> 'a   *)
end

module FuncSet : FSET = struct
  type 'a set =  {cmp:('a -> 'a -> int); list_:'a list}
  exception EmptySet
  let empty_set compare = {cmp = compare; list_ = [] }
  let is_empty group  = group.list_ = []
  
  let size group = List.length group.list_

  let insert group element = 
    if List.mem element group.list_ then
      group
    else
      let added = element:: group.list_ in
      {cmp = group.cmp; list_= List.sort_uniq group.cmp added}
      
      
     
      
       
      

      
  let remove group element =
    if List.mem element group.list_ =false then
      raise EmptySet
    else


    let rec loop(g, e) =  
      match g with 
      | [] -> []
      | h :: tail -> 
        (if h = element then
          loop(tail, e)
        else
          h:: loop(tail, e)
        )in
    let rmv = loop(group.list_,element)in
      {cmp = group.cmp; list_= rmv} 
        

  
    let union set1 set2 = 
      let added = set1.list_ @ set2.list_ in 
      let sorted = List.sort_uniq  set1.cmp added in 
      {cmp=set1.cmp; list_= sorted}
   
       
    let intersect set1 set2 = 
      let rec loop(list1, list2 ) =
        match list1 with 
        | [] -> [] 
        | h::t -> 
          (if List.mem h list2 then
              h::loop(t,list2)
          else
            loop(t,list2)) in
      let intersected = loop(set1.list_, set2.list_) in 
        {cmp=set1.cmp; list_= List.sort_uniq  set1.cmp intersected} 

    let from_list compare group = {cmp = compare; list_= List.sort_uniq compare group}
    let to_list group = List.sort_uniq group.cmp group.list_
    let map fn set =
      let rec loop(fn, group) =
        match group with
        | [] -> []
        | h::t -> fn(h)::loop(fn, t) in 
      let mapped = loop(fn, set.list_) in 
      {cmp = set.cmp; list_= List.sort_uniq set.cmp mapped}

    let fold fn set =
      let rec fold_left fn list acc = 
        match list with 
        | [] -> acc
        | h::t -> fold_left fn (fn acc h) t 
      in fold_left fn set.list_ 
      


end



