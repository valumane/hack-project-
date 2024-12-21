#use "merge.ml";;
let ( depensetout,slogram,tetedamis,depensetouthache ) = init_sheet();;

let findLoginInList(login,lst:string*(string*string)list ):'a list=
  let rec aux(acc,lst,index)=
    if lst =[]
    then acc
    else
      if fst( List.hd lst) = login
      then aux(index::acc,List.tl lst,index+1)
      else aux(acc,List.tl lst,index+1)
  in
  List.rev ( aux([],lst,0) ) 
;;

let find_info_by_login_in_db(login,db,namedb)=
  let indb = findLoginInList(login,db) in
  let rec aux(acc,lst)=
    if lst = []
    then acc
    else 
      let res = (namedb,login,snd(List.nth db (List.hd lst)) ) in
      aux(res :: acc, List.tl lst)
  in
  aux([],indb)
;;

find_info_by_login_in_db("wponfel4",depensetout,"depensetout");;
find_info_by_login_in_db("wponfel4",slogram,"slogram");;
find_info_by_login_in_db("wponfel4",tetedamis,"tetedamis");;

