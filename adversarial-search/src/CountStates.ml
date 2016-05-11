(** Counts states in Tic-tac-toe game in various ways **)
open Tic_tac_toe

let all_legal_moves (board : state) =
  match getWinner board.board with
  | Cross
  | Naught -> []
  | Blank -> List.map (fun i -> { player = board.toPlay; ind = i; }) board.remaining

let printState i s = Printf.sprintf "Move %i:\n%s\n" i (boardToString s.board)

let dfs_no_closed (init : state) =
  let rec loop (state : state) (ind : int) (stack : move list list) =
    (* Printf.eprintf "Loop:%s\n%!" (printState ind state); *)
    match getWinner state.board with
    | Cross
    | Naught -> undoMv state ind stack
    | Blank -> nextMove state ind stack
  and undoMv (state  : state) (ind : int) (stack : move list list) =
    (* Printf.eprintf "undomv:%s\n%!" (printState ind state); *)
    match stack with
    | [] -> ind
    | hd::tl ->
      begin
        match hd with
        | [] -> undoMv state ind tl
        | [toHere] -> undoMv (undoMove state toHere) ind tl
        | toHere::next::rem -> nextMove (applyMove (undoMove state toHere) next) ind ((next::rem)::tl)
      end
  and nextMove state ind (stack : move list list) =
    (* Printf.eprintf "nextMove:%s\n%!" (printState ind state); *)
    match all_legal_moves state with
    | [] -> undoMv state ind stack
    | hd::tl as moves -> loop (applyMove state hd) (ind + 1) (moves::stack)
  in
  loop init 0 []

let dfs_closed (init : state) =
  let seen = Hashtbl.create 10_000 in
  let key s = s.board in
  let dups = ref 0 in
  let incf r = r := !r + 1 in
  let rec loop (state : state) (ind : int) (stack : move list list) =
    let kval = key state in
    if Hashtbl.mem seen kval
    then begin
      incf dups;
      undoMv state ind stack
    end
    else begin
      Hashtbl.add seen kval ind;
      match getWinner state.board with
      | Cross
      | Naught -> undoMv state ind stack
      | Blank -> nextMove state ind stack
    end
  and undoMv (state  : state) (ind : int) (stack : move list list) =
    match stack with
    | [] -> (ind, !dups)
    | hd::tl ->
      begin
        match hd with
        | [] -> undoMv state ind tl
        | [toHere] -> undoMv (undoMove state toHere) ind tl
        | toHere::next::rem -> nextMove (applyMove (undoMove state toHere) next) ind ((next::rem)::tl)
      end
  and nextMove state ind (stack : move list list) =
    match all_legal_moves state with
    | [] -> undoMv state ind stack
    | hd::tl as moves -> loop (applyMove state hd) (ind + 1) (moves::stack)
  in
  loop init 0 []

let dfs_closed_canon (init : state) =
  let seen = Hashtbl.create 10_000 in
  let key s = canonicalBoard s.board in
  let dups = ref 0 in
  let incf r = r := !r + 1 in
  let rec loop (state : state) (ind : int) (stack : move list list) =
    let kval = key state in
    if Hashtbl.mem seen kval
    then begin
      incf dups;
      undoMv state ind stack
    end
    else begin
      Hashtbl.add seen kval ind;
      match getWinner state.board with
      | Cross
      | Naught -> undoMv state ind stack
      | Blank -> nextMove state ind stack
    end
  and undoMv (state  : state) (ind : int) (stack : move list list) =
    match stack with
    | [] -> (ind, !dups)
    | hd::tl ->
      begin
        match hd with
        | [] -> undoMv state ind tl
        | [toHere] -> undoMv (undoMove state toHere) ind tl
        | toHere::next::rem -> nextMove (applyMove (undoMove state toHere) next) ind ((next::rem)::tl)
      end
  and nextMove state ind (stack : move list list) =
    match all_legal_moves state with
    | [] -> undoMv state ind stack
    | hd::tl as moves -> loop (applyMove state hd) (ind + 1) (moves::stack)
  in
  loop init 0 []

let main () =
  let dfs_nodes = dfs_no_closed initState in
  let dup_dfs_nodes, dup_dfs_dups = dfs_closed initState in
  let cdup_dfs_nodes, cdup_dfs_dups = dfs_closed_canon initState in
  Printf.printf "Depth First Search:       %i moves.\n" dfs_nodes;
  Printf.printf "Depth First Search (DD):  %i moves, %i dups.\n" dup_dfs_nodes dup_dfs_dups;
  Printf.printf "Depth First Search (DDC): %i moves, %i dups.\n" cdup_dfs_nodes cdup_dfs_dups

let _ = main ()
