let fu = function 
| `A -> "A"
| `B -> "B"

let _ = 
  fu `A |> Printf.printf "%s\n%!" ;
  fu `B |> Printf.printf "%s\n%!" (*; 
  fu `C |> Printf.printf "%s\n%!" *)

type old_expr = [ `Num of int | `Plus of old_expr * old_expr | `Minus of old_expr * old_expr]


let rec eval = function 
| `Plus (x, y) -> eval x + eval y
| `Minus (x, y) -> eval x - eval y
| `Num i -> i

let rec eval_mult = function 
| `Mult (x,y) -> eval_mult x * eval_mult y 
| #old_expr as x -> eval x

let rec expr_to_string = function 
| `Num x -> string_of_int x 
| `Plus (x, y) -> Printf.sprintf "(%s + %s)" (expr_to_string x) (expr_to_string y)
| `Minus (x, y) -> Printf.sprintf "(%s - %s)" (expr_to_string x) (expr_to_string y)

let rec expr_mult_to_string = function 
| `Mult (x, y) -> Printf.sprintf "(%s * %s)" (expr_mult_to_string x) (expr_mult_to_string y)
| #old_expr as x -> expr_to_string x 


let expr1 = `Plus (`Num 1, `Plus (`Num 2, `Num 3))  
let expr2 = `Plus (`Num 1, `Minus (`Num 2, `Num 3))  
let expr3 = `Mult (`Num 1, `Minus (`Num 2, `Num 3))  
(*let expr4 = `Plus (`Num 1, `Mult (`Num 2, `Num 3))  *)

let test e = 
  Printf.printf "%s: %s\n%!" (expr_to_string e) (string_of_int @@ eval e)

let test1 e =
  Printf.printf "%s: %s\n%!" (expr_mult_to_string e) (string_of_int @@ eval_mult e)

let _ = 
  test expr1;
  test expr2;
  test1 expr3 (*;
  test1 expr4 *)

(*
let f1 = function `A x -> x = 1 | `B -> true | `C -> false
let f2 = function `A x -> x = "a" | `B -> true 

let f x = f1 x && f2 x

let _ =
  string_of_bool (f @@ `A 1) |> Printf.printf "%s\n%!" ; 
  string_of_bool (f @@ `A "1") |> Printf.printf "%s\n%!" ; 
  string_of_bool (f @@ `C) |> Printf.printf "%s\n%!" ; 
  string_of_bool (f @@ `B) |> Printf.printf "%s\n%!" 
*)



class stack_of_ints =
    object (self)
      val mutable the_list = ( [] : int list ) (* instance variable *)
      method push x =                        (* push method *)
        the_list <- x :: the_list
      method pop =                           (* pop method *)
        let result = List.hd the_list in
        the_list <- List.tl the_list;
        result
      method peek =                          (* peek method *)
        List.hd the_list
      method size =                          (* size method *)
        List.length the_list
      method print = 
        Printf.sprintf "[%s]" @@
        String.concat ", " @@ List.map string_of_int the_list
    end

class queue_of_ints =
    object (self)
      val mutable the_list = ( [] : int list ) (* instance variable *)
      method push x =                        (* push method *)
        the_list <- x :: the_list
      method pop =                           (* pop method *)
        let reversed = List.rev the_list in 
        let result = List.hd reversed in
        the_list <- List.tl reversed;
        result
      method peek =                          (* peek method *)
        List.hd the_list
      method size =                          (* size method *)
        List.length the_list
      method print = 
        Printf.sprintf "<%s>" @@ String.concat ", " @@ List.map string_of_int the_list
    end

let test_struct obj = 
  let x = obj in 
  let y = x#push 1 ; x in
  let z = y#pop ; y in 
  let w = z#pop ; z in 
  Printf.printf "Obj: %s\nPeek: %s\nPush 1: %s\nPop Pop: %s\n%!"
    (x#print) 
    (string_of_int x#peek) 
    (y#print)
    (w#print)

let _ = 
  let stack = new stack_of_ints in 
  stack#push 13; 
  Printf.printf "%s\n" (stack#print);
  stack#push 42; 
  Printf.printf "%s\n" (stack#print);
  
  test_struct stack 
