type id = string

type binop = Plus | Minus | Times | Div

type stm = CompoundStm of stm * stm
	 | AssignStm of id * exp
	 | PrintStm of exp list
and exp = IdExp of id
	| NumExp of int
	| OpExp of exp * binop * exp
	| EseqExp of stm * exp

let prog = 
  CompoundStm(AssignStm("a", OpExp(NumExp 5, Plus, NumExp 3)),
	      CompoundStm(AssignStm("b",
				    EseqExp(PrintStm[IdExp "a"; OpExp(IdExp "a", Minus, NumExp 1)], 
					   OpExp(NumExp 10, Times, IdExp "a"))),
	      PrintStm[IdExp "b"]));;

	     


let rec maxargs (s:stm) =
  match s with 
    CompoundStm(s1, s2) -> max (maxargs s1) (maxargs s2)
  | AssignStm(_, e) -> maxargsexp e
  | PrintStm(explist) -> max (List.length explist) (maxargsexplist explist)
and maxargsexp (e:exp) =
  match e with
    EseqExp(s, e1) -> max (maxargs s) (maxargsexp e1)
  | OpExp(e2, _, e3) -> max (maxargsexp e2) (maxargsexp e3)
  | _ -> 0
and maxargsexplist (el: exp list)=
  match el with
    [] -> 0
  | (e::rest) -> max (maxargsexp e) (maxargsexplist rest);;

let printStmInProg = maxargs(prog);;

type linkList = (id*int)list;;

let interp s =

  let update t i n = (i, n)::t in
  
  let rec interpStm s t=
    match s with 
      CompoundStm(s1, s2) -> interpStm s2 (interpStm s1 t)
    | AssignStm(i, e) -> let (u, t1) = (interpExp e t) in
			 update t i u
    | _ -> t
  
  and lookup i t =
    match t with
      [] -> 0
     |((j,v)::rest) -> if i=j then v else lookup i rest

  and interpExp e t =
    match e with
      IdExp(a) -> ((lookup a t), t)
    | NumExp(n) -> (n, t)
    | OpExp(e1, binop, e2) ->
       (let (a, t1) = (interpExp e1 t) and
	   (b, t2) = (interpExp e2 t) in
       match binop with
	 Plus -> (a+b, t)
       | Minus -> (a-b, t)
       | Times -> (a*b, t)
       | Div -> (a / b, t))
    | EseqExp(s3, e3) -> (interpExp e3 (interpStm s3 t))

  and table:linkList = [] in
		  
  (interpStm s table);;

let result = (interp prog);;

let rec print_list = function
    [] -> ()
  | (s, i)::l -> print_string s ; print_string " " ; print_int i ; print_string " "; print_list l in
 
(print_list result)

