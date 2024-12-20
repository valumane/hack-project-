(*--- partie1 ---*)
#use "partie1.ml";;
(* init tout les fuites de donnee *) (*~7sec*)
let ( depensetout,slogram,tetedamis,depensetouthache ) = init_sheet();;


(List.length depensetout,List.length depensetouthache,
List.length slogram,
List.length tetedamis);;
      