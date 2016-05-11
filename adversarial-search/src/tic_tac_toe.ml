(** Simple implementation of Tic Tac Toe for Draper DLC
    Jordan Thayer, May 2016 *)

type space = | Blank | Cross | Naught
type board = space array array

(* Representing the game state, rather than the board state *)
type remaining = int list

type state = {
  toPlay : space;
  board : board;
  remaining : remaining;
}

type move = {
  player : space;
  ind : int;
}

(* Some useful constants *)
let maxBoardInd = 2
let winCross = 3
let winNaught = ~-3

let initState = {
  toPlay = Cross;
  board = Array.make_matrix 3 3 Blank;
  remaining = [0; 1; 2; 3; 4; 5; 6; 7; 8;]
}

let nextPlayer = function
  | Blank -> Blank
  | Cross -> Naught
  | Naught -> Cross

(* Display *)
let toChar = function
  | Blank -> ' '
  | Cross -> 'X'
  | Naught -> 'O'

let toInt = function
  | Blank -> 0
  | Cross -> 1
  | Naught -> ~-1

let rowToString r =
  Printf.sprintf " %c | %c | %c " (toChar r.(0)) (toChar r.(1)) (toChar r.(2))

let boardToString b =
  Printf.sprintf "%s\n-----------\n%s\n-----------\n%s\n"
    (rowToString b.(0))
    (rowToString b.(1))
    (rowToString b.(2))

(* Win or loss *)
let computeWin res =
  if res = winCross then
    Cross
  else if res = winNaught then
    Naught
  else Blank

let getWinningRow b i =
  let row = b.(i) in
  let res = Array.fold_left (fun accum el -> accum + (toInt el)) 0 row in
  computeWin res

let getWinningCol b i =
  let res = ref 0 in
  for y = 0 to maxBoardInd do
    res := !res + (toInt b.(y).(i))
  done;
  computeWin !res

let getWinningUpDiag b =
  let res = toInt b.(2).(0) + toInt b.(1).(1) + toInt b.(0).(2) in
  computeWin res

let getWinningDownDiag b =
  let res = toInt b.(0).(0) + toInt b.(1).(1) + toInt b.(2).(2) in
  computeWin res


let getWinner b =
  match getWinningRow b 0 with
  | Blank ->
    begin
      match getWinningRow b 1 with
      | Blank ->
        begin
          match getWinningRow b 2 with
          | Blank ->
            begin
              match getWinningCol b 0 with
              | Blank ->
                begin
                  match getWinningCol b 1 with
                  | Blank ->
                    begin
                      match getWinningCol b 2 with
                      | Blank ->
                      begin
                        match getWinningUpDiag b with
                        | Blank -> getWinningDownDiag b
                        | x -> x
                      end
                      | x -> x
                    end
                  | x -> x
                end
            | x -> x
            end
          | x -> x
        end
    | x -> x
    end
  | x -> x

(* 1D -> 2D mapping for grid cells *)
let indToInt (x, y) =
  y * 3 + x

let intToInd i =
  let x = i mod 3
  and y = i / 3 in
  (x,y)

let applyMove b m =
  let (x,y) = intToInd m.ind in
  let rem = List.mem m.ind b.remaining
  and blank = b.board.(y).(x) = Blank in
  if rem && blank  then begin
    let next_board = Array.make_matrix 3 3 Blank in
    for x = 0 to maxBoardInd do
      for y = 0 to maxBoardInd do
        next_board.(y).(x) <- b.board.(y).(x)
      done
    done;
    next_board.(y).(x) <- m.player;
    {
      toPlay = nextPlayer m.player;
      remaining = List.filter (fun i -> i != m.ind) b.remaining;
      board = next_board;
    }
  end
  else failwith (Printf.sprintf "Illegal move to %i -> (%i, %i) by %c! rem: %b blank: %b" m.ind x y (toChar m.player) rem blank)

let undoMove b m =
  let (x,y) = intToInd m.ind in
  if not (List.mem m.ind b.remaining) && not (b.board.(y).(x) = Blank) then begin
    let next_board = Array.make_matrix 3 3 Blank in
    for x = 0 to maxBoardInd do
      for y = 0 to maxBoardInd do
        next_board.(y).(x) <- b.board.(y).(x)
      done
    done;
    next_board.(y).(x) <- Blank;
    {
      toPlay = nextPlayer m.player;
      remaining = m.ind :: b.remaining;
      board = next_board;
    }
  end
  else failwith (Printf.sprintf "Illegal undo move to %i %i by %c!" x y (toChar m.player))


let maxMove board =
  let sum = ref 0 in
  for x = 0 to maxBoardInd do
    for y = 0 to maxBoardInd do
      sum := toInt (board.(y).(x))
    done;
  done;
  if !sum > 0 then Cross
  else if !sum < 0 then Naught
  else Blank

let canonicalBoard board =
  let maxMover = maxMove board in
  match maxMover with
  | Cross
  | Blank -> board
  | Naught -> begin
    let toRet = Array.make_matrix 3 3 Blank in
    for y = 0 to maxBoardInd do
      for x = 0 to maxBoardInd do
        toRet.(y).(x) <- nextPlayer board.(y).(x)
      done
    done;
    toRet
  end
