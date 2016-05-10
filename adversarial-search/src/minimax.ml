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
          let nextNode = makeNode cState node.player in
          match accum with
          | None -> Some nextNode
          | Some accum ->
            if node.player = Max && accum.score > nextNode.score
            then Some accum
            else if node.player = Max
            then Some nextNode
            else if node.player = Min && accum.score < nextNode.score
            then Some accum
            else Some nextNode) None kids with
        | None -> failwith "?"
        | Some best -> { node with score = best.score; next = best; }
      end in
  search (makeNode initState Max)

let main () =
  let istate = Tic_tac_toe.initState in
  let getValue s = Tic_tac_toe.toInt (Tic_tac_toe.getWinner s.Tic_tac_toe.board) in
  let all_legal_moves (board : Tic_tac_toe.state) =
    List.map (fun i -> { Tic_tac_toe.player = board.Tic_tac_toe.toPlay; Tic_tac_toe.ind = i; }) board.Tic_tac_toe.remaining in
  let getChildren state =
    let nexts = all_legal_moves state in
    List.map (Tic_tac_toe.applyMove state) nexts in
  minimax getChildren getValue istate

let _ = main
