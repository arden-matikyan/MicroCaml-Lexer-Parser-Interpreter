open MicroCamlTypes
open Utils
open TokenTypes

(* Provided functions - DO NOT MODIFY *)

(* Matches the next token in the list, throwing an error if it doesn't match the given token *)
let match_token (toks: token list) (tok: token) =
  match toks with
  | [] -> raise (InvalidInputException(string_of_token tok))
  | h::t when h = tok -> t
  | h::_ -> raise (InvalidInputException(
      Printf.sprintf "Expected %s from input %s, got %s"
        (string_of_token tok)
        (string_of_list string_of_token toks)
        (string_of_token h)))

(* Matches a sequence of tokens given as the second list in the order in which they appear, throwing an error if they don't match *)
let match_many (toks: token list) (to_match: token list) =
  List.fold_left match_token toks to_match

(* Return the next token in the token list as an option *)
let lookahead (toks: token list) = 
  match toks with
  | [] -> None
  | h::t -> Some h

(* Return the token at the nth index in the token list as an option*)
let rec lookahead_many (toks: token list) (n: int) = 
  match toks, n with
  | h::_, 0 -> Some h
  | _::t, n when n > 0 -> lookahead_many t (n-1)
  | _ -> None

(* Part 2: Parsing expressions *)

let rec parse_expr toks = 
  let (t, exp) = parse_Expr toks in (t, exp)

  (*Parses the Expr rule *)
  and parse_Expr toks = 
    match (lookahead toks) with 
      | Some Tok_Let -> parse_LetExpr toks 
      | Some Tok_If -> parse_IfExpr toks 
      | Some Tok_Fun -> parse_FunctionExpr toks
      | _ -> parse_OrExpr toks 

  (* PrimaryExpr -> Tok_Int | Tok_Bool | Tok_String | Tok_ID | ( Expr ) *)
  and parse_PrimaryExpr toks =
    match lookahead toks with
      | Some Tok_Int v -> let t = match_token toks (Tok_Int v) in 
        (t, Value(Int v))
      | Some Tok_Bool b -> let t = match_token toks (Tok_Bool b) in
        (t, Value(Bool b))
      | Some Tok_String str -> let t = match_token toks (Tok_String str) in 
        (t, Value(String str))
      | Some Tok_ID id -> let t = match_token toks (Tok_ID id) in
        (t, ID(id))
      | Some Tok_LParen -> let t = match_token toks Tok_LParen in
        let (t', expr) = parse_expr t in
          let t'' = match_token t' Tok_RParen in
            (t'', expr)
      | _ -> raise (InvalidInputException "Invalid")

  (* FunctionCallExpr -> PrimaryExpr PrimaryExpr | PrimaryExpr *)
  and parse_FunctionCallExpr toks = 
    let (t, expr) = parse_PrimaryExpr (toks) in
      match lookahead t with
        | Some Tok_Int v -> let (t1, expr1) = parse_PrimaryExpr t in 
          (t1, FunctionCall(expr, expr1))
        | Some Tok_Bool b -> let (t1, expr1) = parse_PrimaryExpr t in
          (t1, FunctionCall(expr, expr1))
        | Some Tok_String s -> let (t1, expr1) = parse_PrimaryExpr t in
          (t1, FunctionCall(expr, expr1))
        | Some Tok_ID id -> let (t1, expr1) = parse_PrimaryExpr t in 
          (t1, FunctionCall(expr, expr1))
        | Some Tok_LParen -> let (t1, expr1) = parse_PrimaryExpr t in
          (t1, FunctionCall(expr, expr1))
        | _ -> (t, expr)

  (* UnaryExpr -> not UnaryExpr | FunctionCallExpr *)
  and parse_UnaryExpr toks =
    match lookahead toks with
      | Some Tok_Not -> let t1 = match_token toks Tok_Not in 
          let (t2, expr) = parse_UnaryExpr (t1) in
            (t2, Not (expr))
      | _ -> let (t1, expr) = parse_FunctionCallExpr (toks) in 
        (t1, expr)   
  
  (* ConcatExpr -> UnaryExpr ^ ConcatExpr | UnaryExpr *) 
  and parse_ConcatExpr toks =
    let (t, expr) = parse_UnaryExpr (toks) in
      match lookahead t with
        | Some Tok_Concat -> let t1 = match_token t Tok_Concat in
          let (t2, expr1) = parse_ConcatExpr (t1) in 
            (t2, Binop(Concat, expr, expr1))
        | _ -> (t, expr)

  (* MultiplicativeExpr -> ConcatExpr MultiplicativeOperator MultiplicativeExpr | ConcatExpr *)
  and parse_MultiplicativeExpr toks =
    let (t, expr) = parse_ConcatExpr (toks) in 
      match lookahead t with
        | Some Tok_Mult -> let t1 = match_token t Tok_Mult in 
            let (t2, expr1) = parse_MultiplicativeExpr (t1) in
              (t2, Binop (Mult, expr, expr1))
        | Some Tok_Div -> let t1 = match_token t Tok_Div in 
            let (t2, expr1) = parse_MultiplicativeExpr (t1) in
              (t2, Binop (Div, expr, expr1))
        | _ -> (t, expr)

  (* AdditiveExpr -> MultiplicativeExpr AdditiveOperator AdditiveExpr | MultiplicativeExpr *)
  and parse_AdditiveExpr toks = 
    let (t, expr) = parse_MultiplicativeExpr (toks) in 
      match lookahead t with 
      | Some Tok_Add -> let t1 = match_token t Tok_Add in 
        let (t2, expr1) = parse_AdditiveExpr (t1) in 
          (t2, Binop(Add, expr, expr1))
      | Some Tok_Sub -> let t1 = match_token t Tok_Sub in 
        let (t2, expr1) = parse_AdditiveExpr (t1) in 
          (t2, Binop(Sub, expr, expr1))
      | _ -> (t, expr)

  (* RelationalExpr -> AdditiveExpr RelationalOperator RelationalExpr | AdditiveExpr *)
  and parse_RelationalExpr toks = 
    let (t, expr) = parse_AdditiveExpr (toks) in 
      match lookahead t with 
        | Some Tok_Less -> let t1 = match_token t Tok_Less in 
          let(t2, expr1) = parse_RelationalExpr (t1) in 
            (t2, Binop(Less, expr, expr1))
        | Some Tok_Greater -> let t1 = match_token t Tok_Greater in 
          let(t2, expr1) = parse_RelationalExpr (t1) in 
            (t2, Binop(Greater, expr, expr1))
        | Some Tok_LessEqual -> let t1 = match_token t Tok_LessEqual in 
          let(t2, expr1) = parse_RelationalExpr (t1) in 
            (t2, Binop(LessEqual, expr, expr1))
        | Some Tok_GreaterEqual -> let t1 = match_token t Tok_GreaterEqual in 
          let(t2, expr1) = parse_RelationalExpr (t1) in 
            (t2, Binop(GreaterEqual, expr, expr1))
        | _ -> (t, expr)

  (* EqualityExpr -> RelationalExpr EqualityOperator EqualityExpr | RelationalExpr *)
  and parse_EqualityExpr toks =
    let (t, expr) = parse_RelationalExpr (toks) in
      match lookahead t with
        | Some Tok_Equal -> let t1 = match_token t Tok_Equal in 
            let (t2, expr1) = parse_EqualityExpr (t1) in
              (t2, Binop (Equal, expr, expr1))
        | Some Tok_NotEqual -> let t1 = match_token t Tok_NotEqual in 
          let (t2, expr1) = parse_EqualityExpr (t1) in
            (t2, Binop (NotEqual, expr, expr1))
        | _ -> (t, expr)

  (* LetExpr -> let Recursion Tok_ID = Expr in Expr *)
  and parse_LetExpr toks =
    let t = match_token toks Tok_Let in
      match lookahead t with
      | Some Tok_ID id -> let t1 = match_token t (Tok_ID id) in
        let t2 = match_token t1 Tok_Equal in 
          let (t3, expr1) = parse_expr (t2) in
            let t4 = match_token t3 Tok_In in 
              let (t5, expr2) = parse_expr (t4) in 
                (t5, Let(id, false, expr1, expr2))
      | Some Tok_Rec -> let t1 = match_token t Tok_Rec in
        (match lookahead t1 with
          | Some Tok_ID id -> let t2 = match_token t1 (Tok_ID id) in
            let t3 = match_token t2 Tok_Equal in
              let (t4, expr1) = parse_expr (t3) in
                let t5 = match_token t4 Tok_In in
                  let (t6, expr2) = parse_expr (t5) in 
                    (t6, Let(id, true, expr1, expr2))
          | _ -> raise(InvalidInputException "no id"))              
      | _ -> raise(InvalidInputException "no id")

  (* FunctionExpr -> fun Tok_ID -> Expr *)
  and parse_FunctionExpr toks =
    let t = match_token toks Tok_Fun in
      match lookahead t with
        | Some Tok_ID id -> let t1 = match_token t (Tok_ID id) in
          let t2 = match_token t1 Tok_Arrow in
            let (t3, expr1) = parse_expr (t2) in
              (t3, Fun(id, expr1))
        |_ -> raise (InvalidInputException "no id")

  (* IfExpr -> if Expr then Expr else Expr *)
  and parse_IfExpr toks =
    let t = match_token toks Tok_If in
      let (t1, expr1) = parse_expr (t) in
        let t2 = match_token t1 Tok_Then in 
          let (t3, expr2) = parse_expr (t2) in
            let t4 = match_token t3 Tok_Else in 
              let (t5, expr3) = parse_expr (t4) in
                (t5, If(expr1, expr2, expr3))

  (* OrExpr -> AndExpr || OrExpr | AndExpr *)
  and parse_OrExpr toks =
    let (t, expr) = parse_AndExpr (toks) in
      match lookahead t with
        | Some Tok_Or -> let t1 = match_token t Tok_Or in
          let (t2, expr1) = parse_OrExpr (t1) in
            (t2, Binop (Or, expr, expr1))
        |_ -> (t, expr)          

  (* AndExpr -> EqualityExpr && AndExpr | EqualityExpr *)
  and parse_AndExpr toks =
    let (t, expr) = parse_EqualityExpr (toks) in 
      match lookahead t with
        | Some Tok_And -> let t1 = match_token t Tok_And in 
          let (t2, expr1) = parse_AndExpr (t1) in
            (t2, Binop (And, expr, expr1))
        | _ -> (t, expr)
 
(* Part 3: Parsing mutop *)
let rec parse_mutop toks = 
  match (lookahead toks) with
    | Some Tok_DoubleSemi -> (match_token toks Tok_DoubleSemi, NoOp)
    | Some Tok_Def -> parse_DefMutop toks
    | _ -> parse_ExprMutop toks

  and parse_ExprMutop toks = 
  let (t1, expr_after_Expr) = parse_expr toks in
    (match_token t1 Tok_DoubleSemi, Expr(expr_after_Expr))
  
  and parse_DefMutop toks = 
    let t1 = match_token toks Tok_Def in
      let (t2, id) = 
        match (lookahead t1) with
        | Some Tok_ID a -> (match_token t1 (Tok_ID a), a)
        | _ ->  raise (InvalidInputException ("no id"))
      in 
        let t3 = match_token t2 Tok_Equal in
          let (t4, expr_after_Expr) = parse_expr t3 in
            let t5 = match_token t4 Tok_DoubleSemi in
              (t5, Def(id , expr_after_Expr)) 
    