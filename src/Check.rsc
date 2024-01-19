module Check

import AST;
import Resolve;
import Message; // see standard library

data Type
  = tint()
  | tbool()
  | tstr()
  | tunknown()
  ;

// the type environment consisting of defined questions in the form 
alias TEnv = rel[loc def, str name, str label, Type \type];

// To avoid recursively traversing the form, use the `visit` construct
// or deep match (e.g., `for (/question(...) := f) {...}` ) 
TEnv collect(AForm f) {
  TEnv tenv = {};
  RefGraph rg = resolve(f);
  visit (f) {
    case question(label, id, val): 
      tenv += {<id.src, id.name, label, typeOf(val)>};
    case computedQuestion(label, id, _, AExpr expr): 
      tenv += {<id.src, id.name, label, typeOf(expr, tenv, rg.useDef)>};
  }
  return tenv; 
}

set[Message] check(AForm f, TEnv tenv, UseDef useDef) {
  set[Message] msgs = {};
  visit (f) {
    case question(str label, id, AType val):
      msgs += check(question(label, id, val), tenv, useDef);
    case computedQuestion(str label, id, AType val, AExpr expr):
      msgs += check(computedQuestion(label, id, val, expr), tenv, useDef);
    case ifThenElse(AExpr condition, AQuestion passedCondition, AQuestion failedCondition):
      msgs += check(ifThenElse(condition, passedCondition, failedCondition), tenv, useDef);
    case ifThen(AExpr condition, AQuestion question):
      msgs += check(ifThen(condition, question), tenv, useDef);
  }
  return msgs; 
}

// - produce an error if there are declared questions with the same name but different types.
// - duplicate labels should trigger a warning 
// - the declared type computed questions should match the type of the expression.
set[Message] check(AQuestion q, TEnv tenv, UseDef useDef) {
  set[Message] msgs = {};
  switch (q) {
    case question(str label, id, AType val): {
      if (<loc def, _, str l, _> <- tenv && l == label && def != id.src) {
        msgs += { warning("Declared questions with duplicate labels", id.src) };
      };
      if (<loc def, str name, _, Type t> <- tenv && t != typeOf(val) && def != id.src && name == id.name) {
        msgs += { error("Declared questions with the same name but different types", id.src) };
      };
    }
    case computedQuestion(_, id, AType val, AExpr expr): {
      msgs += check(expr, tenv, useDef);
      if (typeOf(expr, tenv, useDef) != typeOf(val)) {
        msgs += { error("The declared type computed questions should match the type of the expression", id.src) };
      }
    }
    case ifThenElse(AExpr condition, _, _): {
      msgs += check(condition, tenv, useDef);
      if (typeOf(condition,  tenv, useDef) != tbool()) {
        msgs += { error("Condition must be a boolean", condition.src) };
      };
    }
    case ifThen(AExpr condition, _): {
      msgs += check(condition, tenv, useDef);
      if (typeOf(condition,  tenv, useDef) != tbool()) {
        msgs += { error("Condition must be a boolean", condition.src) };
      }
    }
  }
  return msgs; 
}

// Check operand compatibility with operators.
// E.g. for an addition node add(lhs, rhs), 
//   the requirement is that typeOf(lhs) == typeOf(rhs) == tint()
set[Message] check(AExpr e, TEnv tenv, UseDef useDef) {
  set[Message] msgs = {};
  visit (e) {
    case ref(id):
      if (id.src notin useDef<0>) {
          msgs += { error("Question not defined", id.src) };
      } 
    case multiply(lhs, rhs): 
      if (typeOf(lhs, tenv, useDef) != tint() || typeOf(rhs, tenv, useDef) != tint()) {
        msgs += { error("Operand types do not match operator requirements", e.src) };
      } 
    case divide(lhs, rhs):
      if (typeOf(lhs, tenv, useDef) != tint() || typeOf(rhs, tenv, useDef) != tint()) {
        msgs += { error("Operand types do not match operator requirements", e.src) };
      }
    case addition(lhs, rhs):
      if (typeOf(lhs, tenv, useDef) != tint() || typeOf(rhs, tenv, useDef) != tint()) {
        msgs += { error("Operand types do not match operator requirements", e.src) };
      }
    case subtraction(lhs, rhs):
      if (typeOf(lhs, tenv, useDef) != tint() || typeOf(rhs, tenv, useDef) != tint()) {
        msgs += { error("Operand types do not match operator requirements", e.src) };
      }
    case greaterThan(lhs, rhs):
      if (typeOf(lhs, tenv, useDef) != tint() || typeOf(rhs, tenv, useDef) != tint()) {
        msgs += { error("Operand types do not match operator requirements", e.src) };
      }
    case lessThan(lhs, rhs):
      if (typeOf(lhs, tenv, useDef) != tint() || typeOf(rhs, tenv, useDef) != tint()) {
        msgs += { error("Operand types do not match operator requirements", e.src) };
      }
    case lessThanEqual(lhs, rhs):
      if (typeOf(lhs, tenv, useDef) != tint() || typeOf(rhs, tenv, useDef) != tint()) {
        msgs += { error("Operand types do not match operator requirements", e.src) };
      }
    case greaterThanEqual(lhs, rhs):
      if (typeOf(lhs, tenv, useDef) != tint() || typeOf(rhs, tenv, useDef) != tint()) {
        msgs += { error("Operand types do not match operator requirements", e.src) };
      }
  }
  return msgs; 
}

Type typeOf(AExpr e, TEnv tenv, UseDef useDef) {
  switch (e) {
    case ref(id(_, src = loc u)):  
      if (<u, loc d> <- useDef, <d, _, _, Type t> <- tenv) {
        return t;
      }
      else return tunknown();
    case string(_): return tstr();
    case integer(_): return tint();
    case boolean(_): return tbool(); 
    case bracketExpr(AExpr expr): return typeOf(expr, tenv, useDef);
    case not(AExpr expr): return typeOf(expr, tenv, useDef);
    case multiply(_, _): return tint();
    case divide(_, _): return tint();
    case addition(_, _): return tint();
    case subtraction(_, _): return tint();
    case and(_, _): return tbool();
    case or(_, _): return tbool();
    case greaterThan(_, _): return tbool();
    case lessThan(_, _): return tbool();
    case lessThanEqual(_, _): return tbool();
    case greaterThanEqual(_, _): return tbool();
    case equal(_, _): return tbool();
    case notEqual(_, _): return tbool();

  }
  return tunknown(); 
}

Type typeOf(AType t) {
  switch (t) {
    case stringType(): return tstr();
    case integerType(): return tint();
    case booleanType(): return tbool();
  }
  return tunknown(); 
}

/* 
 * Pattern-based dispatch style:
 * 
 * Type typeOf(ref(id(_, src = loc u)), TEnv tenv, UseDef useDef) = t
 *   when <u, loc d> <- useDef, <d, x, _, Type t> <- tenv
 *
 * ... etc.
 * 
 * default Type typeOf(AExpr _, TEnv _, UseDef _) = tunknown();
 *
 */