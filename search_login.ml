#use "decodehach.ml";;
 
let extract_login(fuite)=
  let rec aux(acc,lst: 'a list * ('a *'a ) list) : 'a list=
    if lst=[]
    then List.rev acc
    else aux( fst(List.hd lst) :: acc, List.tl lst)
  in
  aux([],fuite)
;;

let remove_duplicates lst =
  let seen = Hashtbl.create (List.length lst) in
  let result = ref [] in
  List.iter (fun login ->
    if not (Hashtbl.mem seen login) then (
      Hashtbl.add seen login true;
      result := login :: !result
    )
  ) lst;
  List.rev !result
;;


let regroupe_all_login(f1,f2,f3)=
  remove_duplicates( extract_login(f1)@extract_login(f2)@extract_login(f3) )

;;
let list_of_login = regroupe_all_login(depensetout,slogram,tetedamis);;

