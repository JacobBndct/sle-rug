module AST

/*
 * Define Abstract Syntax for QL
 *
 * - complete the following data types
 * - make sure there is an almost one-to-one correspondence with the grammar
 */

data AForm(loc src = |project://sle-rug/src/Syntax.rsc|)
  = form(str name, list[AQuestion] questions)
  ;

data AQuestion(loc src = |project://sle-rug/src/Syntax.rsc|)
  = question(str label, AId id, AType val)
  | computedQuestion(str label, AId id, AType val, AExpr expr)
  | block(list[AQuestion] questions)
  | ifThenElse(AExpr condition, AQuestion passedCondition, AQuestion failedCondition)
  | ifThen(AExpr condition, AQuestion question)
  ;

data AExpr(loc src = |project://sle-rug/src/Syntax.rsc|)
  = ref(AId id)
  | string(str s)
  | integer(int i)
  | boolean(bool b)
  | bracketExpr(AExpr expr)
  | not(AExpr expr)
  | multiply(AExpr left, AExpr right)
  | divide(AExpr left, AExpr right)
  | addition(AExpr left, AExpr right)
  | subtraction(AExpr left, AExpr right)
  | and(AExpr left, AExpr right)
  | or(AExpr left, AExpr right)
  | greaterThan(AExpr left, AExpr right)
  | lessThan(AExpr left, AExpr right)
  | lessThanEqual(AExpr left, AExpr right)
  | greaterThanEqual(AExpr left, AExpr right)
  | equal(AExpr left, AExpr right)
  | notEqual(AExpr left, AExpr right)
  ;

data AId(loc src = |std:///lang/std/Id.rsc|)
  = id(str name);

data AType(loc src = |project://sle-rug/src/Syntax.rsc|)
  = stringType()
  | integerType()
  | booleanType()
  ;