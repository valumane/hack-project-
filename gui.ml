#use "decodehach.ml";;

#require "graphics";;
open Graphics;;

open_graph("800x600");;

let draw_on_line(l)=
  for i = 0 to List.length l -1
  do 
    draw_string( (List.nth l i)^" | ")
  done;
;;
test( t.(0) );;

let y : int ref = ref 550;;
moveto 5 !y;;

let draw(t)=
  clear_graph();
  y := 550;
  moveto 5 550;

  for i=0 to Array.length t -1 
  do
    test( t.(i) );
    y := !y - 10;
    moveto 5 !y
  done;
  
;;
draw( print_result_by_login("ksarbelw") );;