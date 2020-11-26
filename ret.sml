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

