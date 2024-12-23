#use "decodehach.ml";;

#require "graphics";;
open Graphics;;

open_graph("800x600");;

clear_graph();;
let draw_on_line(l)=
  for i = 0 to List.length l -1
  do 
    draw_string( (List.nth l i)^" | ")
  done;
;;
draw_on_line( t.(0) );;
t.(0);;
let y : int ref = ref 550;;
moveto 5 !y;;

let draw(t)=
  clear_graph();
  y := 550;
  moveto 5 550;

  let l = t.(0) in
  for i = 0 to List.length l -1
  do 
    draw_string( (List.nth l i) )
  done;

  y := !y - 10;
  moveto 5 !y;

  for i=1 to Array.length t -1 
  do
    draw_on_line( t.(i) );
    y := !y - 10;
    moveto 5 !y
  done;
  
;;
draw( print_result_by_login("oqdsihfnh") );;
