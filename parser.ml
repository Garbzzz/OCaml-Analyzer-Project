open Types
open Utils

(* Provided functions - DO NOT MODIFY *)

(* Matches the next token in the list, throwing an error if it doesn't match the given token *)
let match_token (toks : token list) (tok : token) =
  match toks with
  | [] -> raise (InvalidInputException (string_of_token tok))
  | h :: t when h = tok -> t
  | h :: _ ->
      raise
        (InvalidInputException
           (Printf.sprintf "Expected %s from input %s, got %s"
              (string_of_token tok)
              (string_of_list string_of_token toks)
              (string_of_token h)))

(* Matches a sequence of tokens given as the second list in the order in which they appear, throwing an error if they don't match *)
let match_many (toks : token list) (to_match : token list) =
  List.fold_left match_token toks to_match

(* Return the next token in the token list as an option *)
let lookahead (toks : token list) =
  match toks with [] -> None | h :: t -> Some h

(* Return the token at the nth index in the token list as an option*)
let rec lookahead_many (toks : token list) (n : int) =
  match (toks, n) with
  | h :: _, 0 -> Some h
  | _ :: t, n when n > 0 -> lookahead_many t (n - 1)
  | _ -> None

(* Part 2: Parsing expressions *)

let rec parse_primary toks =
  match lookahead toks with
  | Some Tok_Int n -> 
      let toks' = match_token toks (Tok_Int n) in
      (toks', Int n)
  | Some Tok_Bool b ->
      let toks' = match_token toks (Tok_Bool b) in
      (toks', Bool b)
  | Some Tok_String s ->
      let toks' =  match_token toks (Tok_String s) in
      (toks', String s)
  | Some Tok_ID id ->
      let toks' = match_token toks (Tok_ID id) in 
      (toks', ID id)
  | Some Tok_LParen ->
      let toks' = match_token toks Tok_LParen in
      let toks'', e = parse_expr toks' in
      let toks''' = match_token toks'' Tok_RParen in
      (toks''', e)
       
  | Some Tok_LCurly ->
      parse_record toks
  | _ -> raise (InvalidInputException "parse_primary")

  and parse_record toks =
    let toks1 = match_token toks Tok_LCurly in match lookahead toks1 with 
      | Some Tok_RCurly ->
        let toks2 = match_token toks1 Tok_RCurly in 
        (toks2, Record [])
      |_ -> 
        let toks2, fields = parse_record_body toks1 in 
        let toks3  = match_token toks2 Tok_RCurly in 
        (toks3, Record fields)

  and parse_record_body toks =
    match lookahead toks with 
    | Some (Tok_ID id) ->
      let toks1 = match_token toks (Tok_ID id) in
      let toks2 = match_token toks1 Tok_Equal in
      let toks3, e = parse_expr toks2 in
      let lab = Lab id in
      begin match lookahead toks3 with
      | Some Tok_Semi ->
          let toks4 = match_token toks3 Tok_Semi in
          let toks5, rest = parse_record_body toks4 in
          (toks5, (lab, e) :: rest)
      | _ ->
          (toks3, [(lab, e)])
      end
  | _ ->
      raise (InvalidInputException "record field expected")
  and parse_select toks =
  let toks1, base = parse_primary toks in
  let rec loop toks_acc expr_acc =
    match lookahead toks_acc with
    | Some Tok_Dot ->
        let toks2 = match_token toks_acc Tok_Dot in
        begin match lookahead toks2 with
        | Some (Tok_ID id) ->
            let toks3 = match_token toks2 (Tok_ID id) in
            loop toks3 (Select (Lab id, expr_acc))
        | _ ->
            raise (InvalidInputException "field name after '.' expected")
        end
    | _ -> (toks_acc, expr_acc)
  in
  loop toks1 base
  and begins_primary = function
  | Some (Tok_Int _) | Some (Tok_Bool _) | Some (Tok_String _)
  | Some (Tok_ID _) | Some Tok_LParen | Some Tok_LCurly -> true
  | _ -> false

  and parse_app toks =
    let toks1, f = parse_select toks in
    let rec loop toks_acc acc =
      if begins_primary (lookahead toks_acc) then
        let toks2, arg = parse_select toks_acc in
        loop toks2 (App (acc, arg))
      else
        (toks_acc, acc)
    in
    loop toks1 f


  and parse_unary toks =
    match lookahead toks with
    | Some Tok_Not ->
        let toks1 = match_token toks Tok_Not in
        let toks2, e = parse_unary toks1 in
        (toks2, Not e)
    | _ ->
        parse_app toks

  and parse_right_chain parse_atom next_of_op toks =
    (* parse_atom parses the LHS; next_of_op maps a token to (op_ctor, token) or None *)
    let toks1, lhs = parse_atom toks in
    match lookahead toks1 with
    | Some tok ->
        begin match next_of_op tok with
        | None -> (toks1, lhs)
        | Some (op_ctor, tok_to_match) ->
            let toks2 = match_token toks1 tok_to_match in
            let toks3, rhs = parse_right_chain parse_atom next_of_op toks2 in
            (toks3, Binop (op_ctor, lhs, rhs))
        end
    | None -> (toks1, lhs)
  
  
  and parse_concat toks =
    let next_of_op = function
      | Tok_Concat -> Some (Concat, Tok_Concat)
      | _ -> None
    in
    parse_right_chain parse_unary next_of_op toks

  and parse_multiplicative toks =
    let next_of_op = function
      | Tok_Mult -> Some (Mult, Tok_Mult)
      | Tok_Div  -> Some (Div,  Tok_Div)
      | _ -> None
    in
    parse_right_chain parse_concat next_of_op toks

  and parse_additive toks =
    let next_of_op = function
      | Tok_Add -> Some (Add, Tok_Add)
      | Tok_Sub -> Some (Sub, Tok_Sub)
      | _ -> None
    in
    parse_right_chain parse_multiplicative next_of_op toks

  and parse_relational toks =
    let next_of_op = function
      | Tok_Less         -> Some (Less,         Tok_Less)
      | Tok_Greater      -> Some (Greater,      Tok_Greater)
      | Tok_LessEqual    -> Some (LessEqual,    Tok_LessEqual)
      | Tok_GreaterEqual -> Some (GreaterEqual, Tok_GreaterEqual)
      | _ -> None
    in
    parse_right_chain parse_additive next_of_op toks

  and parse_equality toks =
    let next_of_op = function
      | Tok_Equal    -> Some (Equal,    Tok_Equal)
      | Tok_NotEqual -> Some (NotEqual, Tok_NotEqual)
      | _ -> None
    in
    parse_right_chain parse_relational next_of_op toks

  and parse_and toks =
    let next_of_op = function
      | Tok_And -> Some (And, Tok_And)
      | _ -> None
    in
    parse_right_chain parse_equality next_of_op toks

  and parse_or toks =
    let next_of_op = function
      | Tok_Or -> Some (Or, Tok_Or)
      | _ -> None
    in
    parse_right_chain parse_and next_of_op toks

  and parse_expr toks =
    match lookahead toks with
    | Some Tok_Let ->
        let toks1 = match_token toks Tok_Let in
        let is_rec, toks2 =
          match lookahead toks1 with
          | Some Tok_Rec -> (true, match_token toks1 Tok_Rec)
          | _ -> (false, toks1)
        in
        let id =
          match lookahead toks2 with
          | Some (Tok_ID x) -> x
          | _ -> raise (InvalidInputException "identifier expected after let")
        in
        let toks3 = match_token toks2 (Tok_ID id) in
        let toks4 = match_token toks3 Tok_Equal in
        let toks5, e1 = parse_expr toks4 in
        let toks6 = match_token toks5 Tok_In in
        let toks7, e2 = parse_expr toks6 in
        (toks7, Let (id, is_rec, e1, e2))

    | Some Tok_If ->
        let toks1 = match_token toks Tok_If in
        let toks2, e1 = parse_expr toks1 in
        let toks3 = match_token toks2 Tok_Then in
        let toks4, e2 = parse_expr toks3 in
        let toks5 = match_token toks4 Tok_Else in
        let toks6, e3 = parse_expr toks5 in
        (toks6, If (e1, e2, e3))

    | Some Tok_Fun ->
        let toks1 = match_token toks Tok_Fun in
        let x =
          match lookahead toks1 with
          | Some (Tok_ID v) -> v
          | _ -> raise (InvalidInputException "parameter name expected after fun")
        in
        let toks2 = match_token toks1 (Tok_ID x) in
        let toks3 = match_token toks2 Tok_Arrow in
        let toks4, body = parse_expr toks3 in
        (toks4, Fun (x, body))

    | _ ->
        parse_or toks

(* Part 3: Parsing mutop *)

(* parse_mutop : token list -> (token list * mutop)

   Grammar:
   Mutop       -> DefMutop | ExprMutop | ";;"
   DefMutop    -> def Tok_ID = Expr ";;"
   ExprMutop   -> Expr ";;"

   Returned AST:
   NoOp                      if input is just ";;"
   Def (id, expr)            if input is def ID = expr;;
   Expr expr                 if input is just expr;;

   Important notes:
   - MUST always consume Tok_DoubleSemi at the end.
   - DEF bindings are implicitly recursive:
        def f = fun x -> ...
     should behave as Let(f, true, Fun(...), ???) in the interpreter later.
     But for THIS PART, parse_mutop returns:
        Def (f, parsed_expression)
     (You do NOT wrap inside Let here.)
*)
let rec parse_mutop toks =
  match lookahead toks with

  (* Case 1: NoOp *)
  | Some Tok_DoubleSemi ->
      let toks = match_token toks Tok_DoubleSemi in
      (toks, NoOp)

  (* Case 2: Global Definition (DefMutop) *)
  | Some Tok_Def ->
      let toks = match_token toks Tok_Def in
      (match lookahead toks with
       | Some (Tok_ID name) ->
           let toks = match_token toks (Tok_ID name) in
           let toks = match_token toks Tok_Equal in
           let (toks, expr) = parse_expr toks in
           let toks = match_token toks Tok_DoubleSemi in
           (toks, Def(name, expr))
       | _ -> raise (InvalidInputException "Expected identifier after 'def'"))

  (* Case 3: Expression directive (ExprMutop) *)
  | _ ->
      let (toks, expr) = parse_expr toks in
      let toks = match_token toks Tok_DoubleSemi in
      (toks, Expr expr)



