(**
  Simple implementation of the minimax algorithm for DLC Course
  Jordan Thayer, May 2016
*)

(** Nodes come in two flavors, depending on who's turn it is to move.
    Either it's a min move, or a max move *)
type player =
| Min
| Max

(* Utility for figuring out what the next player is.
   Min follows Max, Max follows min *)
let next = function
  | Min -> Max
  | Max -> Min

(**  The 'a just says we aren't
     committing to the type of the remaining part of the node. *)
type 'a node = {
  data : 'a;         (* However the game wants to represent the game's state *)
  player : player;   (* Who's turn is it to move? *)
  score : int;       (* What's the utility of this move? *)
  next : 'a node;    (* What's the next move to make in an optimal strategy? *)
  }
(** Note, I only need next if I want to play the game.
    If I just want to know who wins in the optimal case, I can
    save myself a bunch of memory by dropping this field. *)

(** print out the solution minimax found for playing the game *)
let rec visSolution toString node =
  Printf.printf "%s\n\n" (toString node.data);
  if node.next != node (* common trick for terminating trees without using Some / None *)
  then visSolution toString node.next

(** the types of the functions I'm relying on for minimax search **)
type 'a succFun = 'a -> 'a list
type 'a valueFun = 'a -> int

(** simple implementation of minimax. Note that we don't
    need to know anything about the game specifically. Just how to get the next move. *)
let minimax (getChildren : 'a succFun) (getValue : 'a valueFun) (initState : 'a) =
  (* a local helper function for adding nodes to the minimax tree *)
  let makeNode cState player =
    (* recursive structure initially. I am my own next node. *)
    let rec ret = { data = cState;
                    player = player;
                    score = 0; (* we set the score to be neutral initially*)
                    next = ret; } in
    ret in
  (* a local helper function for deciding what node is best in minimax tree search. *)
  let rec bestChild node accum cState =
    let nextPlayer = next node.player in
    let nextNode = search (makeNode cState nextPlayer) in
    match accum with
    | None -> Some nextNode
    | Some accum ->
      begin
        (* here's where we have the two modes of operation, one for min, one for max *)
        match node.player with
        | Max -> if accum.score >= nextNode.score then Some accum else Some nextNode
        | Min -> if accum.score <= nextNode.score then Some accum else Some nextNode
      end
  and search node =
    match getChildren node.data with
      (* a terminal state, supply a score, and push it up *)
    | [] -> { node with score = (getValue node.data) }
    | kids -> (* non-terminal state, search continues *)
      begin
        match List.fold_left (bestChild node) None kids with
        | None -> failwith "?"
        | Some best -> { node with score = best.score; next = best; }
      end
  in
  search (makeNode initState Max)

(** example invocation assuming that we're going to play tic-tac-toe *)
let main () =
  let istate = Tic_tac_toe.initState in
  let getValue s = Tic_tac_toe.toInt (Tic_tac_toe.getWinner s.Tic_tac_toe.board) in
  let all_legal_moves (board : Tic_tac_toe.state) =
    if getValue board = 0
    then List.map (fun i -> { Tic_tac_toe.player = board.Tic_tac_toe.toPlay; Tic_tac_toe.ind = i; }) board.Tic_tac_toe.remaining
    else [] in
  let getChildren state =
    let nexts = all_legal_moves state in
    List.map (Tic_tac_toe.applyMove state) nexts in
  let solution = minimax getChildren getValue istate in
  visSolution (fun s -> Tic_tac_toe.boardToString s.Tic_tac_toe.board) solution

let _ = main ()
