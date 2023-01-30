(*#utop_prompt_dummy
  let _ = UTop.set_show_box false *)

(************************************************************************)
(** Records **)

(* Records have the same "expressive power" as tuples, just with
 * user-defined field names and different syntax for building and using
 * but our first time making our own new type (!) *)

type lava_lamp = { height : float
                 ; color_liquid : string
                 ; color_lava : string }

let my_lamp1 = { height = 13.5 +. 1.0
               ; color_liquid = "bl" ^ "ue"
               ; color_lava = "" ^ "green" ^ "" }

let my_lamp2 =
  { height = 14.4; color_liquid = my_lamp1.color_liquid; color_lava = "x" }

let a = my_lamp1.height
let b = my_lamp1.color_liquid
let c = my_lamp1.color_lava

;;


let concat_liquid_colors ((lamp1 : lava_lamp), (lamp2 : lava_lamp)) =
  lamp1.color_liquid ^ " " ^ lamp2.color_liquid

let epsilon = 0.0001

let same_height (lamp1, lamp2) =
  Float.abs (lamp1.height -. lamp2.height) < epsilon

let is_same = same_height (my_lamp1, my_lamp2)

;;






(************************************************************************)
(** Variants **)

(* Enumerations *)
type si_unit = Second | Meter | Kilogram | Ampere | Kelvin | Mole | Candela

let ss = [ Second; Meter; Second ]

let string_of_si_unit (u : si_unit) : string =
  match u with
  | Second -> "second"
  | Meter -> "meter"
  | Kilogram -> "kilogram"
  | Ampere -> "ampere"
  | Kelvin -> "kelvin"
  | Mole -> "mole"
  | Candela -> "candela"

let sa = string_of_si_unit Ampere

type si_prefix = Giga | Mega | Kilo | Milli | Micro | Nano

let scale p =
  match p with
  | Giga -> 1e9
  | Mega -> 1e6
  | Kilo -> 1e3
  | Milli -> 1e-3
  | Micro -> 1e-6
  | Nano -> 1e-9

let sg = scale Giga

;;


(* Now variant types where one or more constructors carry [typed] data,
   which is much more interesting and powerful
*)
type silly = A of int * bool * string list | Foo of string | Pizza

let silly_over_silly s =
  match s with
  | A (x, y, z) -> List.hd z
  | Foo s2 -> s2 ^ s2
  | Pizza -> "ham and pineapple"

type shape =
  | Circle of float * float * float (* center-x, center-y, radius *)
  | Rectangle of
      float * float * float * float (* x1,y1,x2,y2 (opposite corners) *)
  | Triangle of
      float * float * float * float * float * float (* x1,y1,x2,y2,x3,y3 *)

let area s =
  match s with
  | Circle (x, y, radius) -> Float.pi *. radius *. radius
  | Rectangle (x1, y1, x2, y2) -> Float.abs ((x2 -. x1) *. (y2 -. y1))
  | Triangle (x1, y1, x2, y2, x3, y3) ->
      let a = x1 *. (y2 -. y3) in
      let b = x2 *. (y3 -. y1) in
      let c = x3 *. (y1 -. y2) in
      Float.abs ((a +. b +. c) /. 2.0)

let well_formed s = area s > epsilon

let num_straight_sides s =
  (* will soon learn better style than these variable names *)
  match s with
  | Circle (x, y, r) -> 0
  | Rectangle (a, b, c, d) -> 4
  | Triangle (x1, x2, x3, x4, x5, x6) -> 3

let max_point s =
  let rec highest ps =
    (* local function assumes non-empty list *)
    if List.tl ps = [] then List.hd ps
    else
      let tl_ans = highest (List.tl ps) in
      if snd tl_ans > snd (List.hd ps) then tl_ans else List.hd ps
  in
  match s with
  | Circle (x, y, radius) -> (x, y +. radius)
  | Rectangle (x1, y1, x2, y2) ->
      highest [ (x1, y1); (x2, y2) ] (* any pt on top edge ok *)
  | Triangle (x1, y1, x2, y2, x3, y3) ->
      highest [ (x1, y1); (x2, y2); (x3, y3) ]

let a = area (Rectangle (1., 2., 3., 4.))
let nss = num_straight_sides (Triangle (5., 6., 3., 1., 2., 4.))
let mp = max_point (Circle (5., 6., 3.))

;;


(* Recursive variants: next time! *)
