#use "decodehach.ml";;
open_graph("800x600");;

clear_graph();;
let draw_on_line(l)=
  for i = 0 to List.length l -1
  do 
    draw_string( (List.nth l i)^"|")
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

  for i=0 to Array.length t -1 
  do
    draw_on_line( t.(i) );
    y := !y - 10;
    moveto 5 !y
  done;
;;
draw( print_result_by_login("oqdsihfnh") );;
draw(print_result_by_login("mmarvalo"));;

let valuetest = 
[|
["site";"login";"mdp coder";"mdp decode"];
["depensetout"; "mmarvalo"; "d0ka04"; "mot de passe non trouver"];
["slogram"; "mmarvalo"; "KJAoi+ifhWmihDf8ck066dM3lHd26nizVKgIUDV9p2Y="; "mot de passe non trouver"]; 
["slogram"; "mmarvalo"; "0znR14qf5qa71aec5iwbOxUWe9qrMjFw2YQZ2vSBGjE="; "mot de passe non trouver"];
["tetedamis"; "mmarvalo"; "fo35TfBLpvZB4cw3hRAh/awRnXvelTM8YGb570loyCI="; "mot de passe non trouver"];
["tetedamis"; "mmarvalo"; "KJAoi+ifhWmihDf8ck066dM3lHd26nizVKgIUDV9p2Y="; "mot de passe non trouver"];
["tetedamis"; "mmarvalo"; "C7Y6/NKAk6d6jt0hchIPKkI26rAxXMA39AbFIgG0YwI="; "mot de passe non trouver"];
["tetedamis"; "mmarvalo"; "sYKiGzuTaUsdLtp8R3T8sK22mWzU4GqlBSnb/oFix0k="; "mot de passe non trouver"]
|];;

draw(valuetest);;

List.nth (valuetest.(0)) 1;;
String.length ( List.nth (valuetest.(0)) 0 );;

let find_longest_string_of_nth_collumn(t,n)=
  let len : int ref = ref 0 in
  for i = 0 to Array.length t-1
  do
    let len_to_test = String.length ( List.nth (t.(i)) n ) in
    if !len < len_to_test
    then len := len_to_test
  done;
  !len
;;

find_longest_string_of_nth_collumn(valuetest,0);;


let replace_in_list lst index new_val =
  if index < 0 || index >= List.length lst 
  then failwith "Index hors limites."
  else
    let result = ref [] in
    let i = ref 0 in
    List.iter (fun x ->
      if !i = index 
      then result := !result @ [new_val]
      else result := !result @ [x];
      i := !i + 1
    ) lst;
    !result
;;
replace_in_list [1;2;3;4] 0 10;;

let add_character(t,n)=
  let final_length = find_longest_string_of_nth_collumn(t,n) in
  let rec add_character(p)=
    if String.length p < final_length
    then add_character( p^" " )
    else p
  in
  
  for i = 0 to Array.length t -1
  do
    t.(i) <- replace_in_list t.(i) n (add_character( List.nth (t.(i)) n ))
  done
;;
add_character(valuetest,3);;
draw(valuetest);;
Array.length valuetest;;
let adapt_all_collumn(t)=

  for i = 0 to Array.length t-1
  do
    add_character(t,i)
  done
;;
adapt_all_collumn(valuetest);;
draw(valuetest);;