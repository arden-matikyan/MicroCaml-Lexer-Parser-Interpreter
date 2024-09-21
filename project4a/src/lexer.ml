open TokenTypes

(* Part 1: Lexer - IMPLEMENT YOUR CODE BELOW *)

let tokenize input = 
    let rec lexer pos = 
        if pos >= String.length input then [] 
        
        (* ======== double char =========== *)
        else if (Str.string_match (Str.regexp "<>") input pos) then
            Tok_NotEqual::(lexer (pos + 2))
        else if (Str.string_match (Str.regexp ">=") input pos) then
            Tok_GreaterEqual::(lexer (pos + 2))
        else if (Str.string_match (Str.regexp "<=") input pos) then
            Tok_LessEqual::(lexer (pos + 2))
        else if (Str.string_match (Str.regexp "||") input pos) then
            Tok_Or::(lexer (pos + 2))
        else if (Str.string_match (Str.regexp "&&") input pos) then
            Tok_And::(lexer (pos + 2))
        else if (Str.string_match (Str.regexp "->") input pos) then
            Tok_Arrow::(lexer (pos + 2))
        else if (Str.string_match (Str.regexp ";;") input pos) then
            Tok_DoubleSemi::(lexer (pos + 2))

        (* ========== single char =============== *)   
        else if (Str.string_match (Str.regexp "=") input pos) then
            Tok_Equal::(lexer (pos + 1))
        else if (Str.string_match (Str.regexp ">") input pos) then
            Tok_Greater::(lexer (pos + 1))
        else if (Str.string_match (Str.regexp "<") input pos) then
            Tok_Less::(lexer (pos + 1))
        else if (Str.string_match (Str.regexp "\\+") input pos) then (* special char *)
            Tok_Add::(lexer (pos + 1))
        else if (Str.string_match (Str.regexp "-") input pos) then
            Tok_Sub::(lexer (pos + 1))
        else if (Str.string_match (Str.regexp "\\*") input pos) then (* special char *)
            Tok_Mult::(lexer (pos + 1))
        else if (Str.string_match (Str.regexp "/") input pos) then
            Tok_Div::(lexer (pos + 1))
        else if (Str.string_match (Str.regexp "\\^") input pos) then (* special char *)
            Tok_Concat::(lexer (pos + 1))


        (* ========= negative number (-xxxx)  ==== *)
        else if (Str.string_match (Str.regexp "(-[0-9]+)") input pos) then
            let neg_int_token = Str.matched_string input in                         
            let new_token = int_of_string (String.sub neg_int_token 1 (String.length neg_int_token - 2)) in
                Tok_Int new_token::(lexer (pos + (String.length neg_int_token)))

        (* ===== positive number =========== *)
        else if (Str.string_match (Str.regexp "[0-9]+") input pos)  then
            let int_token = Str.matched_string input in 
                Tok_Int (int_of_string int_token)::(lexer (pos + (String.length int_token)))

        else if (Str.string_match (Str.regexp "(") input pos) then
            Tok_LParen::(lexer (pos + 1))
        else if (Str.string_match (Str.regexp ")") input pos) then
            Tok_RParen::(lexer (pos + 1))

        (* ===== Tok string =======*)
        else if (Str.string_match (Str.regexp "\"[^\"]*\"") input pos) then
            let string_token = Str.matched_string input in
            let sanitzed = String.sub string_token 1 ((String.length string_token) - 2) in
                Tok_String sanitzed::(lexer (pos + (String.length string_token)))

        (* ===== Tok ID ======== *)
        else if (Str.string_match (Str.regexp "[a-zA-Z][a-zA-Z0-9]*") input pos) then
            let token_id = Str.matched_string input in
                if (token_id = "true") then
                    Tok_Bool true::(lexer (pos + 4))
                else if (token_id = "false") then
                    Tok_Bool false::(lexer (pos + 5)) 
                else if (token_id = "not") then
                    Tok_Not::(lexer (pos + 3))
                else if (token_id = "if") then
                    Tok_If::(lexer (pos + 2))
                else if (token_id = "then") then
                    Tok_Then::(lexer (pos + 4))
                else if (token_id = "else") then
                    Tok_Else::(lexer (pos + 4))
                else if (token_id = "let") then
                    Tok_Let::(lexer (pos + 3))
                else if (token_id = "def") then
                    Tok_Def::(lexer (pos + 3))
                else if (token_id = "in") then
                    Tok_In::(lexer (pos + 2))
                else if (token_id = "rec") then
                    Tok_Rec::(lexer (pos + 3))
                else if (token_id = "fun") then
                    Tok_Fun::(lexer (pos + 3))
                else
                    Tok_ID token_id::(lexer (pos + (String.length token_id)))

        else if (Str.string_match (Str.regexp " ") input pos) then
            lexer (pos + 1)
        else if (Str.string_match (Str.regexp "\t") input pos) then
            lexer (pos + 1)
        else if (Str.string_match (Str.regexp "\n") input pos) then
            lexer (pos + 1)

        else 
            raise (InvalidInputException "Invalid Token")
        in lexer 0 
