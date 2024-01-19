module Syntax

extend lang::std::Layout;
extend lang::std::Id;

/*
 * Concrete syntax of QL
 */

start syntax Form 
  = "form" Id name "{" Question* questions "}"
  ; 

// TODO: question, computed question, block, if-then-else, if-then
syntax Question 
  = question: String label Id id ":" Type type
  | computedQuestion: String label Id id ":" Type type "=" Expr expr
  | @Foldable block: "{" Question* questions "}"
  | ifThenElse: "if" "(" Expr condition ")" Question passedCondition "else" Question failedCondition
  | ifThen: "if" "(" Expr condition ")" Question () !>> "else"
  ;

// TODO: +, -, *, /, &&, ||, !, >, <, <=, >=, ==, !=, literals (bool, int, str)
// Think about disambiguation using priorities and associativity
// and use C/Java style precedence rules (look it up on the internet)
syntax Expr 
  = Id \ "true" \ "false" // true/false are reserved keywords.
  | integer: Integer
  | boolean: Boolean
  | string: String

  | bracket "(" Expr ")"
  > right (not: "!" Expr)
  > left (
    multiply: Expr "*" Expr
    | divide: Expr "/" Expr
    | modulus: Expr "%" Expr
  )
  > left (
    addition: Expr "+" Expr
    | subtraction: Expr "-" Expr
  )
  > left (and: Expr "&&" Expr)
  > left (or: Expr "||" Expr)
  > right (
    greaterThan: Expr "\>" Expr
    | lessThan: Expr "\<" Expr
    | lessThanEqual: Expr "\<=" Expr
    | greaterThanEqual: Expr "\>=" Expr
    | equal: Expr "==" Expr
    | notEqual: Expr "!=" Expr
  )
  ;
  
syntax Type 
  = "string"
  | "integer"
  | "boolean"
  ;

lexical String 
  = [\"] (![\"\\]
  | ([\\][\\\"nfbtr]))* [\"] 
  ;

lexical Integer 
  = [\-]?[0-9]+ !>> [0-9]
  ;

lexical Boolean 
  = "true"
  | "false"
  ;



