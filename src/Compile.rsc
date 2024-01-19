module Compile

import AST;
import Resolve;
import IO;
import List;
import lang::html::AST; // see standard library
import lang::html::IO;

void compile(AForm f) {
  writeFile(f.src[extension="js"].top, form2js(f));
  writeFile(f.src[extension="html"].top, writeHTMLString(form2html(f)));
}

HTMLElement form2html(AForm f) {
  return html([
    head([script([], src="https://code.jquery.com/jquery-3.5.1.min.js")]),
    body([form([question2html(q) | q <- f.questions]), script([], src="<f.src[extension="js"].uri>")])
  ]);
}

str form2js(AForm f) {
  return intercalate("\n", [question2js(q) | q <- f.questions]) + "\n" + updateVisibility() + "\n" + computeQuestion();
}

HTMLElement question2html(AQuestion q) {
  switch(q) {
    case question(str _label, AId id, AType val): {
      return div([label([text(_label)], \for = id.name), input(id=id.name, label=_label, \type=getType(val))]);
    }
    case computedQuestion(str _label, AId id, AType val, _): {
      return div([label([text(_label)], \for = id.name), input(id=id.name, label=_label, \type=getType(val), readonly="readonly")]);
    }
    case block(list[AQuestion] questions): {
      return div([question2html(question) | question <- questions]);
    }
    case ifThenElse(AExpr condition, AQuestion passedCondition, AQuestion failedCondition): {
      return div([question2html(passedCondition), question2html(failedCondition)], style="display: none", id="_<expr2js(condition)>");
    }
    case ifThen(AExpr condition, AQuestion question): {
      return div([question2html(question)], style="display: none", id="_<expr2js(condition)>");
    }
    default: throw "Error";
  }
}

str question2js(AQuestion q) {
  switch(q) {
    case question(_, AId id, AType val): {
      return "var " + id.name + " = <initType(val)>" + "; \n $(\"#<id.name>\").on(\'input\', function() { <id.name> = <getVal(val)> });";
    }
    case computedQuestion(_, AId id, AType val, AExpr expr): {
      return "$(window).on(\'click keydown keyup mousemove mousedown\', function() {computeQuestion(\"<id.name>\", <expr2js(expr)>)}); \n var " + id.name + " = <initType(val)>" + "; \n $(\"#<id.name>\").on(\'input\', function() { <id.name> = <getVal(val)> });";
    }
    case block(list[AQuestion] questions): {
      return intercalate("\n", ["\n <question2js(_q)>" | _q <- questions]);
    }
    case ifThenElse(AExpr condition, AQuestion passedCondition, AQuestion failedCondition): {
      return "$(window).on(\'click keydown keyup mousemove mousedown\', function() {updateVisibility(\"_<expr2js(condition)>\", <expr2js(condition)>)}); \n <question2js(passedCondition)> \n <question2js(failedCondition)>";
    }
    case ifThen(AExpr condition, AQuestion question): {
      return "$(window).on(\'click keydown keyup mousemove mousedown\', function() {updateVisibility(\"_<expr2js(condition)>\", <expr2js(condition)>)}); \n <question2js(question)>";
    }
    default: throw "Error";
  }
}

str updateVisibility() {
  return "function updateVisibility(id, condition) {
  $(\"#\" + id).each(function() {
    if (condition) {
      $(this).show();
    } else {
      $(this).hide();
    }
  });
}";
}

str computeQuestion() {
  return "function computeQuestion(id, condition) {
  $(\"#\" + id).each(function() {
    $(this).val(condition);
  });
}";
}

str expr2js(AExpr expr) {
  switch(expr) {
    case ref(AId id): return id.name;
    case string(str s): return "\"" + s + "\"";
    case integer(int i): return "<i>";
    case boolean(bool b): return "<b>";
    case bracketExpr(AExpr expr): return "(" + expr2js(expr) + ")";
    case not(AExpr expr): return "!" + expr2js(expr);
    case multiply(AExpr left, AExpr right): return expr2js(left) + " * " + expr2js(right);
    case divide(AExpr left, AExpr right): return expr2js(left) + " / " + expr2js(right);
    case addition(AExpr left, AExpr right): return expr2js(left) + " + " + expr2js(right);
    case subtraction(AExpr left, AExpr right): return expr2js(left) + " - " + expr2js(right);
    case and(AExpr left, AExpr right): return expr2js(left) + " && " + expr2js(right);
    case or(AExpr left, AExpr right): return expr2js(left) + " || " + expr2js(right);
    case greaterThan(AExpr left, AExpr right): return expr2js(left) + " \> " + expr2js(right);
    case lessThan(AExpr left, AExpr right): return expr2js(left) + " \< " + expr2js(right);
    case lessThanEqual(AExpr left, AExpr right): return expr2js(left) + " \<= " + expr2js(right);
    case greaterThanEqual(AExpr left, AExpr right): return expr2js(left) + " \>= " + expr2js(right);
    case equal(AExpr left, AExpr right): return expr2js(left) + " == " + expr2js(right);
    case notEqual(AExpr left, AExpr right): return expr2js(left) + " != " + expr2js(right);
    default: throw "Error";
  }
}

str getType(AType t) {
  switch(t) {
    case stringType(): return "text";
    case integerType(): return "number";
    case booleanType(): return "checkbox";
    default: throw "Unknown type: <t>";
  }
}

str initType(AType t) {
  switch(t) {
    case stringType(): return "\"\"";
    case integerType(): return "0";
    case booleanType(): return "false";
    default: throw "Unknown type: <t>";
  }
}


str getVal(AType t) {
  switch(t) {
    case stringType(): return "$(this).val();";
    case integerType(): return "$(this).val();";
    case booleanType(): return "$(this).prop(\"checked\");";
    default: throw "Unknown type: <t>";
  }
}