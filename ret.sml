Control.Print.printDepth := 100;
Control.Print.printLength := 100;

fun sum [] = 0
 | sum (x::xs) = x + sum xs ;