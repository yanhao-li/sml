Control.Print.printDepth := 100;
Control.Print.printLength := 100;

(* 1. 

('a -> 'b) -> ('b list -> 'c) -> 'a -> 'c list

x => 'a
g x => 'a -> 'b
m [g x] => 'b list -> 'c
[m [g x]] => 'c list

*)

fun foo g m x = [m [g x]]

(* 2 *)
fun bar x = 
fn [] => []
| (y::ys) => x * y :: bar x ys

(* 3 *)
fun part x [] = ([], [])
| part x (y::ys) = 
  let val (a, b) = part x ys
  in if y < x then (y::a, b) else (a, y::b)
  end

(* 4 *)
fun partSort [] = []
|   partSort [y] = [y]
|   partSort (x::xs) = 
  let val (a, b) = part x xs
  in partSort a @ [x] @ partSort b
  end

(* 5 *)
fun pSort (op <) [] = []
|   pSort (op <) [y] = [y]
|   pSort (op <) (x::xs) =
  let
    fun part x [] = ([], [])
    | part x (y::ys) =
      let
        val (a, b) = part x ys
      in
        if y < x then (y::a, b) else (a, y::b)
      end
    val (a, b) = part x xs
  in
    pSort (op <) a @ [x] @ pSort (op <) b
  end

(* 6 *)
exception reduce_error
fun reduce f [] = raise reduce_error
|   reduce f [y] = y
|   reduce f (x::xs) = f x (reduce f xs)

(* 7 *)
datatype 'a tree = leaf of 'a | node of 'a tree list

(* 8 *)
fun fringe (leaf x) = [x]
|   fringe (node []) = []
|   fringe (node L) = reduce (fn x => fn y => x @ y) (map (fn x => fringe x) L)

(* 9 *)
fun sortTree (op <) (leaf L) = (leaf (pSort (op <) L))
|   sortTree (op <) (node []) = (node [])
|   sortTree (op <) (node L) = (node (map (fn n => sortTree (op <) n) L))

(* 10 *)
fun powerSet [] = [[]]
|   powerSet (x::xs) = 
let val p = powerSet xs
in map (fn y => x::y) p @ p
end