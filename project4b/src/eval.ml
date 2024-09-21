open MicroCamlTypes
open Utils

exception TypeError of string
exception DeclareError of string
exception DivByZeroError 

(* Provided functions - DO NOT MODIFY *)

(* Adds mapping [x:v] to environment [env] *)
let extend env x v = (x, ref v)::env

(* Returns [v] if [x:v] is a mapping in [env]; uses the
   most recent if multiple mappings for [x] are present *)
let rec lookup env x =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then !value else lookup t x

(* Creates a placeholder mapping for [x] in [env]; needed
   for handling recursive definitions *)
let extend_tmp env x = (x, ref (Int 0))::env

(* Updates the (most recent) mapping in [env] for [x] to [v] *)
let rec update env x v =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then (value := v) else update t x v
        
(* Part 1: Evaluating expressions *)

(* Evaluates MicroCaml expression [e] in environment [env],
   returning a value, or throwing an exception on error *)
let rec eval_expr env e =
  match e with
    | Value v -> v
    | ID var -> lookup env var
    | Fun (var, expr) -> Closure (env, var, expr)
    | Not (expr1) -> let (expr2) = (eval_expr env expr1) in (eval_not expr2)
    | Binop (op, expr1, expr2) -> let (expr3), (expr4) = (eval_expr env expr1), (eval_expr env expr2) in (eval_binop op expr3 expr4)
    | If (guard, expr1, expr2) -> eval_if env guard expr1 expr2
    | FunctionCall (expr1, expr2) -> eval_functioncall env expr1 expr2
    | Let (var, recur, expr1, expr2) -> if recur = true then eval_reclet env var expr1 expr2 else eval_let env var expr1 expr2
  and eval_not v =
    match v with
      | (Bool q) -> if q = true then (Bool false) else (Bool true)
      | _ -> raise (TypeError "type error")
  and eval_if env guard expr1 expr2 =
    let v1 = eval_expr env guard in
      match v1 with
        | (Bool q) -> if q = true then (eval_expr env expr1) else (eval_expr env expr2)
        | _ -> raise (TypeError "type error")
  and eval_let env var expr1 expr2 =
    let v1 = eval_expr env expr1 in
      let env2 = extend env var v1 in
        eval_expr env2 expr2
  and eval_binop op e1 e2 =
    match op, e1, e2 with
      | Add, (Int v1), (Int v2) -> (Int (v1 + v2))
      | Sub, (Int v1), (Int v2) -> (Int (v1 - v2))
      | Mult, (Int v1), (Int v2) ->  (Int (v1 * v2))
      | Div, (Int v1), (Int v2) -> if v2 = 0 then raise (DivByZeroError) else (Int (v1 / v2))
      | Greater, (Int v1), (Int v2) -> if v1 > v2 then  (Bool true) else  (Bool false)
      | Less, (Int v1), (Int v2) -> if v1 < v2 then  (Bool true) else  (Bool false)
      | GreaterEqual, (Int v1), (Int v2) -> if v1 >= v2 then  (Bool true) else  (Bool false)        
      | LessEqual, (Int v1), (Int v2) -> if v1 <= v2 then  (Bool true) else  (Bool false)
      | Concat, (String v1), (String v2) -> (String (v1 ^ v2))
      | Equal, (Int v1), (Int v2) -> if v1 = v2 then (Bool true) else (Bool false)
      | Equal, (Bool v1), (Bool v2) -> if v1 = v2 then (Bool true) else (Bool false)
      | Equal, (String v1), (String v2) -> if compare v1 v2 = 0 then (Bool true) else (Bool false)
      | NotEqual, (Int v1), (Int v2) -> if v1 <> v2 then (Bool true) else (Bool false)
      | NotEqual, (Bool v1), (Bool v2) -> if v1 <> v2 then (Bool true) else (Bool false)
      | NotEqual, (String v1), (String v2) -> if compare v1 v2 <> 0 then (Bool true) else (Bool false)
      | Or, (Bool v1), (Bool v2) -> if (v1 = true) || (v2 = true) then (Bool true) else (Bool false)
      | And, (Bool v1), (Bool v2) -> if (v1 = true) && (v2 = true) then (Bool true) else (Bool false)
      | _, _, _ -> raise (TypeError "type error")
  and eval_reclet env var expr1 expr2 =
    let env2 = extend_tmp env var in
      let v1 = eval_expr env2 expr1 in update env2 var v1;
        let v2 = eval_expr env2 expr2 in v2
  and eval_functioncall env expr1 expr2 =
    let v1 = eval_expr env expr1 in 
      let v2 = eval_expr env expr2 in
        match v1 with
          | Closure (env, var, expr) -> let env2 = (extend env var v2) in eval_expr env2 expr
          | _ -> raise (TypeError "functioncall error")

(* Part 2: Evaluating mutop directive *)
(* Evaluates MicroCaml mutop directive [m] in environment [env],
   returning a possibly updated environment paired with
   a value option; throws an exception on error *)

(* eval_mutop : environment -> mutop -> environment * (value option) *)

let eval_mutop env m = 
  match m with
    | Def (id, expr1) -> 
      let env2 = extend_tmp env id in
        let v = eval_expr env2 expr1 in (update env2 id v);
          (env2, Some v)
    | Expr (expr1) -> let v1 = eval_expr env expr1 in (env, Some v1)
    | NoOp -> (env, None)