open Types

(* Provided functions - DO NOT MODIFY *)

(* Adds mapping [x:v] to environment [env] *)
let extend env x v = (x, ref v) :: env

(* Returns [v] if [x:v] is a mapping in [env]; uses the
   most recent if multiple mappings for [x] are present *)
let rec lookup env x =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value) :: t -> if x = var then !value else lookup t x

(* Creates a placeholder mapping for [x] in [env]; needed
   for handling recursive definitions *)
let extend_tmp env x = (x, ref (Int 0)) :: env

(* Updates the (most recent) mapping in [env] for [x] to [v] *)
let rec update env x v =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value) :: t -> if x = var then value := v else update t x v

(* Part 1: Evaluating expressions *)

(* Evaluates MicroCaml expression [e] in environment [env],
   returning an expression, or throwing an exception on error *)
let rec eval_expr env e =
  match e with
  | Int _ | Bool _ | String _ | Closure _ ->
    e

  (* ----- Identifier ----- *)
  | ID x ->
      lookup env x

  (* ----- Unary Not ----- *)
  | Not e1 ->
    (match eval_expr env e1 with 
      | Bool b -> Bool (not b)
      | _ -> raise (TypeError "Expected type bool"))

  (* ----- Binary Operations ----- *)
  | Binop (op, e1, e2) ->
    let left_side = eval_expr env e1 in
    (match op, left_side with
     | And, Bool false -> Bool false
     | Or,  Bool true  -> Bool true
     | And, Bool true ->
         let right_side = eval_expr env e2 in
         (match right_side with
          | Bool b -> Bool b
          | _ -> raise (TypeError "Expected type bool"))
     | Or, Bool false ->
         let right_side = eval_expr env e2 in
         (match right_side with
          | Bool b -> Bool b
          | _ -> raise (TypeError "Expected type bool"))
     | And, _ | Or, _ ->
         raise (TypeError "Expected type bool")
     | _ ->
        let right_side = eval_expr env e2 in
        match op, left_side, right_side with
        | Add, Int a, Int b -> Int (a+b)
        | Sub, Int a, Int b -> Int (a-b)
        | Mult, Int a, Int b -> Int(a*b)
        | Div, Int _, Int 0 -> raise(DivByZeroError)
        | Div, Int a, Int b -> Int(a/b)
        | Greater, Int a, Int b -> Bool(a > b)
        | Less, Int a, Int b -> Bool(a < b)
        | GreaterEqual, Int a, Int b -> Bool (a >= b)
        | LessEqual, Int a, Int b -> Bool(a <= b)
        | Concat, String a, String b -> String(a ^ b)
        | Equal, Int a, Int b -> Bool(a = b)
        | Equal, Bool a, Bool b -> Bool(a = b)
        | Equal, String a, String b -> Bool(a = b)
        | NotEqual, Int a, Int b -> Bool(a<>b)
        | NotEqual, Bool a, Bool b -> Bool(a<>b)
        | NotEqual, String a, String b -> Bool(a <> b)
        | _ -> raise (TypeError "Cannot compare types"))
  (* ----- If expressions ----- *)
  | If (e1, e2, e3) ->
    (match eval_expr env e1 with
    | Bool true  -> eval_expr env e2
    | Bool false -> eval_expr env e3
    | _ -> raise (TypeError "Expected type bool"))

  (* ----- Function / Closure ----- *)
  | Fun (x, body) ->
      Closure(env, x, body)

  (* ----- Function Application ----- *)
  | App (f, arg) ->
      let eval_f = eval_expr env f in 
      let eval_arg = eval_expr env arg in
      (match eval_f with
      | Closure (clos_env, x, body) ->
          eval_expr (extend clos_env x eval_arg) body
      |_-> raise (TypeError "Not a function"))
    
  (* ----- Let Bindings ----- *)
  | Let (x, is_rec, e1, e2) ->
      (match is_rec with 
      |false -> let v1 = eval_expr env e1 in eval_expr (extend env x v1) e2
      |true -> let env' = extend_tmp env x in 
               let v1 = eval_expr env' e1 in
               update env' x v1;
               eval_expr env' e2)
  (* ----- Record Construction ----- *)
  | Record fields ->
      Record (List.map (fun (lab, ex) -> (lab, eval_expr env ex)) fields)

  (* ----- Record Selection ----- *)
  | Select (Lab l, e2) ->
      (match eval_expr env e2 with
       | Record fields ->
           (match List.assoc_opt (Lab l) fields with
            | Some v -> v
            | None -> raise (SelectError "Label not found"))
       | _ -> raise (TypeError "Not a record"))



(* Part 2: Evaluating mutop directive *)

(* Evaluates MicroCaml mutop directive [m] in environment [env],
   returning a possibly updated environment paired with
   a value option; throws an exception on error *)
let eval_mutop env m =
  match m with
(*--- NoOp ---*)
  | NoOp ->
      (env, None)
(*---- Def ----*)
  | Def (x, e) ->
      let env' = extend_tmp env x in
      let v = eval_expr env' e in
      update env' x v;
      (env', Some v)
(*---- Expr ----*)
  | Expr e ->
      let v = eval_expr env e in
      (env, Some v)


