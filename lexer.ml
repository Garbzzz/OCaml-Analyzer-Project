open Types

(* get rid of all white space in the command *)
let rec delete_white_space str position =
  if position >= String.length str then position
  else
    let curr_char = String.get str position in
    if curr_char = ' ' || curr_char = '\t' || curr_char = '\n' then
      delete_white_space str (position + 1)
    else
      position

let tokenize input =
  let rec tokenize_helper input position result =
    if position >= String.length input then List.rev result
    else
      let new_position = delete_white_space input position in
      if new_position >= String.length input then List.rev result
      else
        let remaining = String.sub input new_position (String.length input - new_position) in
        (*We must deal with non-single char characters first because if we do single char, it may negate the true
          meaning of what was typed (ex. semi colon, greater than or equal to would just pull greater). Then 
        we need to match all keyword tokens (ex let, def, rec, etc.) then deal with rest later*)
        (* 1. Tok_DoubleSemi ;; *)
        if Str.string_match (Str.regexp ";;") remaining 0 then
          tokenize_helper input (new_position + 2) (Tok_DoubleSemi :: result)

        (* 2. Tok_GreaterEqual >= *)
        else if Str.string_match (Str.regexp ">=") remaining 0 then
          tokenize_helper input (new_position + 2) (Tok_GreaterEqual :: result)

        (* 3. Tok_LessEqual <= *)
        else if Str.string_match (Str.regexp "<=") remaining 0 then
          tokenize_helper input (new_position + 2) (Tok_LessEqual :: result)

        (* 4. Tok_NotEqual <> *)
        else if Str.string_match (Str.regexp "<>") remaining 0 then
          tokenize_helper input (new_position + 2) (Tok_NotEqual :: result)

        (* 5. Tok_Arrow -> *)
        else if Str.string_match (Str.regexp "->") remaining 0 then
          tokenize_helper input (new_position + 2) (Tok_Arrow :: result)

        (* 6. Tok_Or || *)
        else if Str.string_match (Str.regexp "||") remaining 0 then
          tokenize_helper input (new_position + 2) (Tok_Or :: result)

        (* 7. Tok_And && *)
        else if Str.string_match (Str.regexp "&&") remaining 0 then
          tokenize_helper input (new_position + 2) (Tok_And :: result)

        (* 8. Keyword matching (check before Tok_ID to ensure longest match) *)
        else if Str.string_match (Str.regexp "^let\\([^a-zA-Z0-9]\\|$\\)") remaining 0 then
          tokenize_helper input (new_position + 3) (Tok_Let :: result)

        else if Str.string_match (Str.regexp "^def\\([^a-zA-Z0-9]\\|$\\)") remaining 0 then
          tokenize_helper input (new_position + 3) (Tok_Def :: result)

        else if Str.string_match (Str.regexp "^rec\\([^a-zA-Z0-9]\\|$\\)") remaining 0 then
          tokenize_helper input (new_position + 3) (Tok_Rec :: result)

        else if Str.string_match (Str.regexp "^in\\([^a-zA-Z0-9]\\|$\\)") remaining 0 then
          tokenize_helper input (new_position + 2) (Tok_In :: result)

        else if Str.string_match (Str.regexp "^fun\\([^a-zA-Z0-9]\\|$\\)") remaining 0 then
          tokenize_helper input (new_position + 3) (Tok_Fun :: result)

        else if Str.string_match (Str.regexp "^if\\([^a-zA-Z0-9]\\|$\\)") remaining 0 then
          tokenize_helper input (new_position + 2) (Tok_If :: result)

        else if Str.string_match (Str.regexp "^then\\([^a-zA-Z0-9]\\|$\\)") remaining 0 then
          tokenize_helper input (new_position + 4) (Tok_Then :: result)

        else if Str.string_match (Str.regexp "^else\\([^a-zA-Z0-9]\\|$\\)") remaining 0 then
          tokenize_helper input (new_position + 4) (Tok_Else :: result)

        else if Str.string_match (Str.regexp "^not\\([^a-zA-Z0-9]\\|$\\)") remaining 0 then
          tokenize_helper input (new_position + 3) (Tok_Not :: result)

        (* 9. Tok_Bool true|false — check before Tok_ID *)
        else if Str.string_match (Str.regexp "^\\(true\\|false\\)\\([^a-zA-Z0-9]\\|$\\)") remaining 0 then
          let matcher = Str.matched_string remaining in
          let bool_str =
            if String.length matcher >= 4 && String.sub matcher 0 4 = "true" then "true"
            else "false"
          in
          let bool_val = (bool_str = "true") in
          tokenize_helper input (new_position + (if bool_val then 4 else 5)) (Tok_Bool bool_val :: result)

        (* 10. Tok_String "..." *)
        else if Str.string_match (Str.regexp "^\"[^\"]*\"") remaining 0 then
          let matcher = Str.matched_string remaining in
          let content = String.sub matcher 1 (String.length matcher - 2) in
          tokenize_helper input (new_position + String.length matcher) (Tok_String content :: result)

        (* 11. Negative integers WITH parentheses: (-123) *)
        else if Str.string_match (Str.regexp "(-\\([0-9]+\\))") remaining 0 then
          let matcher = Str.matched_string remaining in
          (* Extract number without parentheses: (-123) -> -123 *)
          let num_str = Str.matched_group 1 remaining in
          tokenize_helper input (new_position + String.length matcher) (Tok_Int (-(int_of_string num_str)) :: result)

        (* 12. pos integers *)
        else if Str.string_match (Str.regexp "^[0-9]+") remaining 0 then
          let matcher = Str.matched_string remaining in
          tokenize_helper input (new_position + String.length matcher) (Tok_Int (int_of_string matcher) :: result)

        (* 13. Tok_ID — [a-zA-Z][a-zA-Z0-9]* *)
        else if Str.string_match (Str.regexp "^[a-zA-Z][a-zA-Z0-9]*") remaining 0 then
          let matcher = Str.matched_string remaining in
          tokenize_helper input (new_position + String.length matcher) (Tok_ID matcher :: result)

        (* Match single-character tokens after checking longer ones *)
        else
          let first_char = String.get remaining 0 in
          if first_char = '(' then
            tokenize_helper input (new_position + 1) (Tok_LParen :: result)
          else if first_char = ')' then
            tokenize_helper input (new_position + 1) (Tok_RParen :: result)
          else if first_char = '{' then
            tokenize_helper input (new_position + 1) (Tok_LCurly :: result)
          else if first_char = '}' then
            tokenize_helper input (new_position + 1) (Tok_RCurly :: result)
          else if first_char = '.' then
            tokenize_helper input (new_position + 1) (Tok_Dot :: result)
          else if first_char = '=' then
            tokenize_helper input (new_position + 1) (Tok_Equal :: result)
          else if first_char = '>' then
            tokenize_helper input (new_position + 1) (Tok_Greater :: result)
          else if first_char = '<' then
            tokenize_helper input (new_position + 1) (Tok_Less :: result)
          else if first_char = ';' then
            tokenize_helper input (new_position + 1) (Tok_Semi :: result)
          else if first_char = '+' then
            tokenize_helper input (new_position + 1) (Tok_Add :: result)
          else if first_char = '-' then
            tokenize_helper input (new_position + 1) (Tok_Sub :: result)
          else if first_char = '*' then
            tokenize_helper input (new_position + 1) (Tok_Mult :: result)
          else if first_char = '/' then
            tokenize_helper input (new_position + 1) (Tok_Div :: result)
          else if first_char = '^' then
            tokenize_helper input (new_position + 1) (Tok_Concat :: result)
          else
            raise (InvalidInputException ("Unexpected character at position " ^ string_of_int new_position))
  in
  tokenize_helper input 0 []