module Transform

import Syntax;
import Resolve;
import AST;
import ParseTree;

/* 
 * Transforming QL forms
 */
 
 
/* Normalization:
 *  wrt to the semantics of QL the following
 *     q0: "" int; 
 *     if (a) { 
 *        if (b) { 
 *          q1: "" int; 
 *        } 
 *        q2: "" int; 
 *      }
 *
 *  is equivalent to
 *     if (true) q0: "" int;
 *     if (true && a && b) q1: "" int;
 *     if (true && a) q2: "" int;
 *
 * Write a transformation that performs this flattening transformation.
 *
 */
 
AForm flatten(AForm f) {
  return form(f.name, flattenQuestions(f.questions, boolean(true)));
}

list[AQuestion] flattenQuestions(list[AQuestion] questions, AExpr condition) {
  list[AQuestion] result = [];
  for (AQuestion q <- questions) {
    switch(q) {
      case AST::question(_, _, _): 
        result += [ifThen(condition, q)];
      case AST::computedQuestion(_, _, _, _): 
        result += [ifThen(condition, q)];
      case AST::block(_questions): 
        result += flattenQuestions(_questions, condition);
      case AST::ifThenElse(cond, passedCondition, failedCondition): 
        result += flattenQuestions([passedCondition], AExpr::and(condition, cond)) + flattenQuestions([failedCondition], AExpr::and(condition, AExpr::not(cond)));
      case AST::ifThen(cond, question): 
        result += flattenQuestions([question], AExpr::and(condition, cond));
    }
  }
  return result;
}

/* Rename refactoring:
 *
 * Write a refactoring transformation that consistently renames all occurrences of the same name.
 * Use the results of name resolution to find the equivalence class of a name.
 *
 */
 
start[Form] rename(start[Form] f, loc location, str newVariableName, UseDef useDef) {   
  set[loc] equivalentClass = { location };
  bool isDefinition = location in useDef<1>;
  if(isDefinition) {
    equivalentClass += { u | <loc u, location> <- useDef };
  } 
  else {
    if( <location, loc definition> <- useDef) {
      equivalentClass += definition;
      equivalentClass += { u | <loc u, definition> <- useDef };
    }
  }
  updatedForm = visit(f) {
    case Id x => [Id] newVariableName
      when x.src in equivalentClass
  }
  return updatedForm; 
}