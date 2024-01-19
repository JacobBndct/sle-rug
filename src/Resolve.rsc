module Resolve

import AST;

/*
 * Name resolution for QL
 */ 


// modeling declaring occurrences of names
alias Def = rel[str name, loc def];

// modeling use occurrences of names
alias Use = rel[loc use, str name];

alias UseDef = rel[loc use, loc def];

// the reference graph
alias RefGraph = tuple[
  Use uses, 
  Def defs, 
  UseDef useDef
]; 

RefGraph resolve(AForm f) = <us, ds, us o ds>
  when Use us := uses(f), Def ds := defs(f);

Use uses(AForm f) {
  rel[loc, str] result = {};
  for (AQuestion q <- f.questions) {
    result += usesQuestion(q);
  }
  return result;
}

Use usesQuestion(AQuestion q) {
  rel[loc, str] result = {};
  switch(q) {
    case computedQuestion(_, _, _, expr): 
      result += usesExpr(expr);
    case block(questions): 
      for (AQuestion q <- questions) { 
        result += usesQuestion(q);
      }
    case ifThenElse(condition, passedCondition, failedCondition): 
      result += usesExpr(condition) + usesQuestion(passedCondition) + usesQuestion(failedCondition);
    case ifThen(condition, question): 
      result += usesExpr(condition) + usesQuestion(question);
  }
  return result;
}

Use usesExpr(AExpr expr) {
  rel[loc, str] result = {};
  visit (expr) {
    case ref(id): result += {<id.src, id.name>};
  }
  return result;
}

Def defs(AForm f) {
  rel[str, loc] result = {};
  for (AQuestion q <- f.questions) {
    result += defsQuestion(q);
  }
  return result;
}

Def defsQuestion(AQuestion q) {
  rel[str, loc] result = {};
  switch(q) {
    case question(_, id, _): result += {<id.name, id.src>};
    case computedQuestion(_, id, _, _): result += {<id.name, id.src>};
    case block(questions): 
      for (AQuestion q <- questions) { 
        result += defsQuestion(q);
      }
    case ifThenElse(_, passedCondition, failedCondition): 
      result += defsQuestion(passedCondition) + defsQuestion(failedCondition);
    case ifThen(_, question): 
      result += defsQuestion(question);
  }
  return result;
}