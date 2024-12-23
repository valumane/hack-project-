
#use "search.ml";;

let mdpclair = mdp_from_file "././outils/french_passwords_top20000.txt";; 

let hashlistmdp(lst)=
  let rec aux(acc,lst)=
    if lst=[]
    then acc
    else aux( hash_password(List.hd lst)::acc,List.tl lst)
  in
  List.rev(aux([],lst))
;;
let hashmdpclair = hashlistmdp(mdpclair);;

let decodeMdp (mdphash, liste,mdpclair) =
  let rec aux(idx,lst)=
    if lst = [] 
    then -1 
    else 
      if List.hd lst = mdphash 
      then idx
      else aux(idx + 1, List.tl lst)
  in
  let res = aux(0,liste) in
  if res = -1
  then "mot de passe non trouver"
  else List.nth mdpclair res
;;


let print_all_info_of_login(login,db,indb)=
  let liste = find_info_by_login_in_db(login,db,indb) in
  let acc : 'a ref = ref [] in

  for i = 0 to List.length liste-1
  do
    let a,b,c = List.nth liste i in
    let res = [a; b; c; decodeMdp(c,hashmdpclair,mdpclair)] in
    acc := res :: !acc 
  done;

  !acc
;;

let print_result_by_login(login)=
  let a = print_all_info_of_login(login, depensetout, "depensetout") in
  let b = print_all_info_of_login(login, slogram, "slogram") in
  let c = print_all_info_of_login(login, tetedamis, "tetedamis") in
  
  let list = a@b@c in

  let res = Array.make (List.length list +1) ["site"; "login"; "mdp clair"; "mdp decode"] in
  
  let rec listToTab(lst, i)=
    if lst = [] 
    then ()
    else 
      (
      res.(i) <- List.hd lst;
      listToTab(List.tl lst, i + 1)
      )
  in

  listToTab(list, 1);
  res
;;

print_result_by_login("dsollong");;
