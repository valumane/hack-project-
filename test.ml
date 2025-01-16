open Graphics;;

clear_graph();;
moveto 100 100;;

let create_Button_aux(originX,originY,tailleX,tailleY,text)=
  moveto originX originY;
  draw_poly([|
  ( (originX+tailleX), (originY) );
  ( (originX+tailleX), (originY+tailleY) );
  ( (originX), (originY+tailleY) );
  ( (originX), (originY) )
|]);
  
  moveto originX (originY+(tailleY/2)-4);
  draw_string text;


;;
draw_string "t";;
moveto 100 (100+20);;


let put_string(originX,originY,text)=
  String.length text
;;
put_string(0,0,"bonjour");;
(12-7)/2;;

let create_Button(originX,originY,tailleX,tailleY,text)=
  clear_graph();
  create_Button_aux(originX,originY,tailleX,tailleY,text);

;;


clear_graph();;
create_Button(100,100,100,50,"  bonjour");;

let testclickb()=
  let tmp =  wait_next_event( [Button_down] ) in
  if tmp.mouse_y > 100 && 
     tmp.mouse_y < 200 &&
     tmp.mouse_x > 100 &&
     tmp.mouse_x < 200 
  then "ok"
  else "pas ok"
;;
testclickb();;

