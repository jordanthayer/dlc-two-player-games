type space = Blank | Cross | Naught
type board = space array array
type remaining = int list
type state = { toPlay : space; board : board; remaining : remaining; }
type move = { player : space; ind : int; }

val initState : state

val nextPlayer : space -> space

val toInt : space -> int

val boardToString : char array array -> string

val getWinner : space array array -> space

val indToInt : int * int -> int
val intToInd : int -> int * int

val applyMove : state -> move -> state
val undoMove : state -> move -> state
