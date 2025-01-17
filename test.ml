open_graph("1000x600");;

clear_graph();;
moveto 100 100;;


clear_graph();;
draw_poly([| (200,100);(200,200);(100,200);(100,100) |]);;

type t_dimension = {text:string; length_x:int; length_y:int; ind_x:int; ind_y:int};;
type t_button = {origin_x:int; origin_y:int; width:int; height:int};;

let init_dim(t)=
  let str_len = String.length t in
  let tmp_ind_x = 15 in (* length by pixel of one characters*)
  let tmp_ind_y = 15 in (* height by pixel of one characters*)
  let result : t_dimension = {text=t; 
                              length_x=str_len*tmp_ind_x; 
                              length_y=tmp_ind_y; 
                              ind_x =tmp_ind_x;
                              ind_y=tmp_ind_y } in
  result
;;
init_dim("bonjour");;

let init_button(o_x,o_y,dim)=
  let result : t_button = {origin_x=o_x;origin_y=o_y;width=o_x+dim.length_x;height=o_y+dim.length_y} in
  result
;;
init_button(100,100,init_dim("previous"));;
100/4;;
let create_button(origin_x, origin_y, text)=
  moveto origin_x origin_y;
  
  let dim_text = init_dim(text) in
  let dim_button = init_button(origin_x,origin_y,dim_text) in
  
  draw_poly(
    [| 
      ( dim_button.width, origin_y );
      ( dim_button.width, dim_button.height );
      ( origin_x, dim_button.height );
      ( origin_x, origin_y ) 
    |]
  );
  moveto (origin_x+5)  (100+1);
  draw_string text;  
;;
clear_graph();;
create_button(100,100,"next");;
create_button(200,100,"previous");;

let create_button_with_clear(originX,originY,text)=
  clear_graph();
  create_button(originX,originY,text);
;;
create_button_with_clear(100,100,"previous");;

let testclickb(dimbutton)=
  let tmp =  wait_next_event( [Button_down] ) in
  if tmp.mouse_y > dimbutton.origin_y && 
     tmp.mouse_y < dimbutton.height &&
     tmp.mouse_x > dimbutton.origin_x &&
     tmp.mouse_x < dimbutton.width 
  then "ok"
  else "pas ok"
;;
testclickb( init_button(200,100,init_dim("previous") ));;
testclickb( init_button(100,100,init_dim("next") ));;



