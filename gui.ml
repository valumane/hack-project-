#use "search_login.ml";;

open_graph("1000x600");;

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

let add_character(t,n)=
  let final_length = find_longest_string_of_nth_collumn(t,n) in
  let rec add_character(p)=
    if String.length p < final_length
    then add_character( p^" " )
    else p
  in
  
  for i = 0 to Array.length t -1
  do
    t.(i) <- replace_in_list t.(i) n ( "|"^( add_character( List.nth (t.(i) ) n ) ) )
  done
;;

let adapt_all_collumn(t)=
  let len = List.length ( t.(0) ) in
  for i = 0 to len -1
  do
    add_character(t,i)
  done
;;

let draw_on_line(l)=
  for i = 0 to List.length l -1
  do 
    draw_string( List.nth l i )
  done;
;;

let y : int ref = ref 550;;

let draw(t)=
  adapt_all_collumn(t);
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
draw(print_result_by_login( List.nth list_of_login 6));;
clear_graph();;

let tmp : int ref = ref 0 ;;

let next()=
  tmp := !tmp + 1;
  draw(print_result_by_login( List.nth list_of_login !tmp) )
;;

let previous()=
  tmp := !tmp -1;
  draw(print_result_by_login( List.nth list_of_login !tmp) )
;;


next();;
previous();;



