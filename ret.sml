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
  let
    val (a, b) = part x ys
  in 
    if y < x then (y::a, b) else (a, y::b)
  end

