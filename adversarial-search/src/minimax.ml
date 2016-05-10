(*
  Simple implementation of the minimax algorithm for DLC Course
  Jordan Thayer, May 2016
*)

(* Nodes come in two flavors, depending on who's turn it is to move.
   Either it's a min move, or a max move.  The 'a just says we aren't
   committing to the type of the remaining part of the node. *)
type player =
| Min
| Max

let next = function
  | Min -> Max
  | Max -> Min

type 'a node =
  { data : 'a;
    player : player;
    score : int;
    next : 'a node; }

(* print out the solution minimax found for playing the game *)
let rec visSolution toString node =
  Printf.printf "%s\n\n" (toString node.data);
  if node.next != node then visSolution toString node.next

(* simple implementation of minimax. Note that we don't
   need to know anything about the game specifically. Just how to get the next move. *)
let minimax getChildren getValue initState =
  let makeNode cState player =
    let rec ret = { data = cState;
                    player = player;
                    score = 0;
                    next = ret; } in
    ret in
  let rec search node =
    match getChildren node.data with
      (* a terminal state, supply a score, and push it up *)
    | [] ->   { node with score = (getValue node.data) }
    | kids ->
      begin
        match List.fold_left (fun accum cState ->
          let nextNode = makeNode cState (next node.player) in
          let nextNode = search nextNode in
          match accum with
          | None -> Some nextNode
          | Some accum ->
            begin
              match node.player with
              | Max -> if accum.score >= nextNode.score then Some accum else Some nextNode
              | Min -> if accum.score <= nextNode.score then Some accum else Some nextNode
            end) None kids with
        | None -> failwith "?"
        | Some best -> { node with score = best.score; next = best; }
      end in
  search (makeNode initState Max)

(* example invocation assuming that we're going to play tic-tac-toe *)
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
