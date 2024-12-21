(*--- partie1 ---*)
#use "merge.ml";;
(* init tout les fuites de donnee *) (*~7sec*)
let ( depensetout,slogram,tetedamis,depensetouthache ) = init_sheet();;
writemerge(depensetout,slogram,tetedamis);;
