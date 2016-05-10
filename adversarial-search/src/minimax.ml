(*
  Simple implementation of the minimax algorithm for DLC Course
  Jordan Thayer, May 2016
*)

(* Nodes come in two flavors, depending on who's turn it is to move.
   Either it's a min move, or a max move.  The 'a just says we aren't
   committing to the type of the remaining part of the node. *)
type 'a node =
| Min of 'a
| Max of 'a

(* the game will provide functions on game states, it doesn't know about minimax's internal
   representation! *)
let wrap fn = function
  | Max s
  | Min s -> fn s

let minimax getChildren hasChildren getValue initState =
  failwith "stub"
