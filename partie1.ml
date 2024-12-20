#use "././outils/tools.ml";;

(*db = fuite de donnee = Data Base = db*)

(* Insère un élément dans une liste triée *)
let rec insert_sorted x lst =
  if List.length lst = 0 then [x]
  else if fst x <= fst (List.hd lst) then x :: lst
  else (List.hd lst) :: (insert_sorted x (List.tl lst))
;;

(* Tri une liste par insertion *)
let rec insertion_sort lst =
  if List.length lst = 0 then []
  else insert_sorted (List.hd lst) (insertion_sort (List.tl lst))
;;

(*remove les doublons*)
let remove_duplicate (lst : (string * string) list) : (string * string) list =
  let acc = ref [] in
  let rec check_duplicate (acc_lst : (string * string) list) (s : string) : bool =
    if List.length acc_lst = 0 then false
    else if String.get (fst (List.hd acc_lst)) 0 = String.get s 0 then true
    else check_duplicate (List.tl acc_lst) s
  in
  let rec add_unique lst =
    if List.length lst = 0 then ()
    else
      let s, v = List.hd lst in
      if not (check_duplicate !acc s) then acc := (s, v) :: !acc;
      add_unique (List.tl lst)
  in
  add_unique lst;
  !acc
;;

(* merge deux listes*)
let rec merge(db2,db1: 'a list * 'a list):'a list=
  if db1=[]
  then db2
  else merge(List.hd db1 :: db2, List.tl db1)
;;


(* merge et supprime les doublons de 2listes*)
let merge_two_list_no_duplicates(db1,db2:'a list*'a list):'a list=
  if db1=[] || db2=[]
  then failwith "erreur merge_no_duplicates : db1 ou db2 vide"
  else remove_duplicate(  merge(db1,db2)  )
;;



(*fonction pour hache une db*)
let hache_db (db : ('a * string) list) : ('a * string) list =
  let length = List.length db in
  let result : ('a * string) list ref = ref [] in
  for i = 0 to length - 1 do
    let (login, password) = List.nth db i in
    result := (login, hash_password password) :: !result
  done;
  List.rev !result
;;

(* initialise toute les bd *)
let init_sheet ():(string * string) list *
                  (string * string) list *
                  (string * string) list *
                  (string * string) list =
  (* Lecture des fichiers individuels *)
  let depensetout01 = read_data_from_file "./fuite/depensetout01.txt" and
      depensetout02 = read_data_from_file "./fuite/depensetout02.txt" in 

  let slogram01 = read_data_from_file "./fuite/slogram01.txt" and
      slogram02 = read_data_from_file "./fuite/slogram02.txt" in

  let tetedamis01 = read_data_from_file "./fuite/tetedamis01.txt" and
      tetedamis02 = read_data_from_file "./fuite/tetedamis02.txt" in

  (* Fusion des bases de donnees *)
  let depensetout = merge_two_list_no_duplicates(depensetout01,depensetout02) and
      slogram = merge_two_list_no_duplicates(slogram01,slogram02) and
      tetedamis = merge_two_list_no_duplicates(tetedamis01,tetedamis02) in

  (*hache tout les mdp de depensetout*)
  let depensetouthache = hache_db(depensetout) in
  
  (* Retourne un tuple contenant toutes les bases de donn es *)
  (depensetout, slogram, tetedamis, depensetouthache);;

let (depensetout,slogram,tetedamis,depensetouthache) = init_sheet();;

(* AUUUU SECOURS ECRITURE FONCTIONNELLLLE AAAAAAAAAAAAAH*)
let format_fuite () : string * string -> string =
  (fun (login, pwd) -> Printf.sprintf "(%s,%s)" login pwd)
;;
