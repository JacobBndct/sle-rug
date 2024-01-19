module CST2AST

import Syntax;
import AST;

import ParseTree;
import String;
import Boolean;

/*
 * Implement a mapping from concrete syntax trees (CSTs) to abstract syntax trees (ASTs)
 *
 * - Use switch to do case distinction with concrete patterns (like in Hack your JS) 
 * - Map regular CST arguments (e.g., *, +, ?) to lists 
 *   (NB: you can iterate over * / + arguments using `<-` in comprehensions or for-loops).
 * - Map lexical nodes to Rascal primitive types (bool, int, str)
 * - See the ref example on how to obtain and propagate source locations.
 */

AForm cst2ast(start[Form] sf) {
  Form f = sf.top; // remove layout before and after form
  return form("f.name", [cst2ast(question) | Question question <- f.questions], src=f.src); 
}

default AQuestion cst2ast(Question q) {
    switch (q) {
      case (Question)`<String label> <Id x> : <Type val>`: return question("<label>", id("<x>", src=x.src), cst2ast(val), src=x.src);
      case (Question)`<String label> <Id x> : <Type val> = <Expr expr>`: return computedQuestion("<label>", id("<x>", src=x.src), cst2ast(val), cst2ast(expr), src=x.src);
      case (Question)`{<Question* qs>}`: return block([cst2ast(question) | question <- qs]);
      case (Question)`if (<Expr condition>) <Question passedCondition> else <Question failedCondition>`: return ifThenElse(cst2ast(condition), cst2ast(passedCondition), cst2ast(failedCondition));
      case (Question)`if (<Expr condition>) <Question question>`: return ifThen(cst2ast(condition), cst2ast(question));
    default: throw "Unhandled expression: <q>";
  }
}

AExpr cst2ast(Expr e) {
  switch (e) {
    case (Expr)`<Id x>`:  return ref(id("<x>", src=x.src), src=x.src);
    case (Expr)`<String s>`: return string("<s>", src=e.src);
    case (Expr)`<Integer i>`: return integer(toInt("<i>"), src=e.src);
    case (Expr)`<Boolean b>`: return boolean(fromString("<b>"), src=e.src);
    case (Expr)`(<Expr e1>)`: return bracketExpr(cst2ast(e1), src=e.src);
    case (Expr)`!<Expr e1>`: return not(cst2ast(e1), src=e.src);
    case (Expr)`<Expr e1> * <Expr e2>`: return multiply(cst2ast(e1), cst2ast(e2), src=e.src);
    case (Expr)`<Expr e1> / <Expr e2>`: return divide(cst2ast(e1), cst2ast(e2), src=e.src);
    case (Expr)`<Expr e1> + <Expr e2>`: return addition(cst2ast(e1), cst2ast(e2), src=e.src);
    case (Expr)`<Expr e1> - <Expr e2>`: return subtraction(cst2ast(e1), cst2ast(e2), src=e.src);
    case (Expr)`<Expr e1> && <Expr e2>`: return and(cst2ast(e1), cst2ast(e2), src=e.src);
    case (Expr)`<Expr e1> || <Expr e2>`: return or(cst2ast(e1), cst2ast(e2), src=e.src);
    case (Expr)`<Expr e1> \> <Expr e2>`: return greaterThan(cst2ast(e1), cst2ast(e2), src=e.src);
    case (Expr)`<Expr e1> \< <Expr e2>`: return lessThan(cst2ast(e1), cst2ast(e2), src=e.src);
    case (Expr)`<Expr e1> \<= <Expr e2>`: return lessThanEqual(cst2ast(e1), cst2ast(e2), src=e.src);
    case (Expr)`<Expr e1> \>= <Expr e2>`: return greaterThanEqual(cst2ast(e1), cst2ast(e2), src=e.src);
    case (Expr)`<Expr e1> == <Expr e2>`: return equal(cst2ast(e1), cst2ast(e2), src=e.src);
    case (Expr)`<Expr e1> != <Expr e2>`: return notEqual(cst2ast(e1), cst2ast(e2), src=e.src);
    
    default: throw "Unhandled expression: <e>";
  }
}

default AType cst2ast(Type t) {
    switch (t) {
      case (Type)`string`: return stringType(src=t.src);
      case (Type)`integer`: return integerType(src=t.src);
      case (Type)`boolean`: return booleanType(src=t.src);

    default: throw "Unhandled expression: <t>";
  }
}
