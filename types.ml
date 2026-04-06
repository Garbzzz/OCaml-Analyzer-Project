(* lexer types *)
exception InvalidInputException of string

type token =
  | Tok_RParen
  | Tok_LParen
  | Tok_RCurly
  | Tok_LCurly
  | Tok_Dot
  | Tok_Equal
  | Tok_NotEqual
  | Tok_Greater
  | Tok_Less
  | Tok_GreaterEqual
  | Tok_LessEqual
  | Tok_Or
  | Tok_And
  | Tok_Not
  | Tok_If
  | Tok_Then
  | Tok_Else
  | Tok_Add
  | Tok_Sub
  | Tok_Mult
  | Tok_Div
  | Tok_Concat
  | Tok_Let
  | Tok_Rec
  | Tok_In
  | Tok_Def
  | Tok_Fun
  | Tok_Arrow
  | Tok_Int of int
  | Tok_Bool of bool
  | Tok_String of string
  | Tok_ID of string
  | Tok_DoubleSemi
  | Tok_Semi
  [@@deriving show { with_path = false }]

(* parser & evaluator types *)
exception TypeError of string
exception DeclareError of string
exception SelectError of string
exception DivByZeroError

type op =
  | Add
  | Sub
  | Mult
  | Div
  | Concat
  | Greater
  | Less
  | GreaterEqual
  | LessEqual
  | Equal
  | NotEqual
  | Or
  | And
  [@@deriving show { with_path = false }]

type var = string 
[@@deriving show { with_path = false }]
type label = Lab of var 
[@@deriving show { with_path = false }]

type expr =
  | Int of int
  | Bool of bool
  | String of string
  | ID of var
  | Fun of var * expr
  | Not of expr
  | Binop of op * expr * expr
  | If of expr * expr * expr
  | App of expr * expr
  | Let of var * bool * expr * expr
  | Closure of environment * var * expr
  | Record of (label * expr) list
  | Select of label * expr
  [@@deriving show { with_path = false }]

and environment = (var * expr ref) list
  [@@deriving show { with_path = false }]

type mutop = 
  | Def of var * expr 
  | Expr of expr 
  | NoOp
  [@@deriving show { with_path = false }]

type mutop_parser_result = token list * mutop
  [@@deriving show { with_path = false }]

type expr_parser_result = token list * expr
  [@@deriving show { with_path = false }]

type mutop_eval_result = environment * expr option
  [@@deriving show { with_path = false }]

type option_eval_result = expr option
  [@@deriving show { with_path = false }]

type expr_eval_result = expr
  [@@deriving show { with_path = false }]