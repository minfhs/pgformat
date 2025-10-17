open Ast

(* Token classification *)
module TokenClassifier = struct
  let is_simple_operator = function
    | EQ | LT | GT | GTE | LTE | NEQ | INTO | ASSIGN | LANGUAGE
    | INT _ | FLOAT _ | FUNC_DELIM | AS | PLUS | MINUS | STAR
    | PARAMETER _ | JSON_OP _ | TEXT_SEARCH_OP _ -> true
    | _ -> false

  let is_structural_keyword = function
    | SELECT | FROM | WHERE | CREATE | INSERT | VALUES | BEGIN | END
    | DECLARE | LOOP | END_LOOP -> true
    | _ -> false

  let is_join_keyword = function
    | LEFT | RIGHT | INNER | OUTER | FULL | JOIN | ON -> true
    | _ -> false

  let is_clause_keyword = function
    | AND | OR | RETURNS -> true
    | _ -> false

  let requires_space_after = function
    | SEMICOLON -> false
    | _ -> true

  let requires_newline_before = function
    | SELECT | FROM | WHERE | CREATE | INSERT | BEGIN | END 
    | DECLARE | AND | OR | LEFT | JOIN -> true
    | _ -> false
end
