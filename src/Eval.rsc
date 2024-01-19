module Eval

import AST;
import Resolve;

/*
 * Implement big-step semantics for QL
 */
 
// NB: Eval may assume the form is type- and name-correct.


// Semantic domain for expressions (values)
data Value
  = vint(int n)
  | vbool(bool b)
  | vstr(str s)
  ;

// The value environment
alias VEnv = map[str name, Value \value];

// Modeling user input
data Input
  = input(str question, Value \value);
  
// produce an environment which for each question has a default value
// (e.g. 0 for int, "" for str etc.)
VEnv initialEnv(AForm f) {
  VEnv venv = ();
  for (AQuestion q <- f.questions) {
    visit(q) {
      case question(_, id, val): venv[id.name] = defaultValue(val);
      case computedQuestion(_, id, val, _): venv[id.name] = defaultValue(val);
    }
  }
  return venv;
}

Value defaultValue(AType t) {
  switch(t) {
    case integerType(): return vint(0);
    case booleanType(): return vbool(false);
    case stringType(): return vstr("");
    default: throw "Unknown type <t>";
  }
}

// Because of out-of-order use and declaration of questions
// we use the solve primitive in Rascal to find the fixpoint of venv.
VEnv eval(AForm f, Input inp, VEnv venv) {
  return solve (venv) {
    venv = evalOnce(f, inp, venv);
  }
}

VEnv evalOnce(AForm f, Input inp, VEnv venv) {
  for (AQuestion q <- f.questions) {
    venv = eval(q, inp, venv);
  }
  return venv;
}

// evaluate conditions for branching,
// evaluate inp and computed questions to return updated VEnv
VEnv eval(AQuestion q, Input inp, VEnv venv) {
  switch(q) {
    case question(_, id, _): 
      if (inp.question == id.name) {
        venv[id.name] = inp.\value;
      }
    case computedQuestion(_, id, _, expr): 
      venv[id.name] = eval(expr, venv);
    case block(questions): 
      for (AQuestion subQ <- questions) {
        venv = eval(subQ, inp, venv);
      }
    case ifThenElse(condition, passedCondition, failedCondition): 
      if (eval(condition, venv) == vbool(true)) {
        venv = eval(passedCondition, inp, venv);
      } else {
        venv = eval(failedCondition, inp, venv);
      }
    case ifThen(condition, question): 
      if (eval(condition, venv) == vbool(true)) {
        venv = eval(question, inp, venv);
      }
  }
  return venv;
}



Value eval(AExpr e, VEnv venv) {
  switch (e) {
    case ref(id(str x)): return venv[x];
    case integer(i): return vint(i);
    case boolean(b): return vbool(b);
    case string(s): return vstr(s);
    case bracketExpr(expr): return eval(expr, venv);
    case not(expr): 
      switch(eval(expr, venv)) {
        case vbool(b): return vbool(!b);
        default: throw "Unsupported operation <e>";
      }
    case multiply(left, right): {
      Value leftValue = eval(left, venv);
      Value rightValue = eval(right, venv);
      if (leftValue is vint && rightValue is vint) {
        return vint(leftValue.n * rightValue.n);
      } else {
        throw "Unsupported operation <e>";
      }
    }
    case divide(left, right): {
      Value leftValue = eval(left, venv);
      Value rightValue = eval(right, venv);
      if (leftValue is vint && rightValue is vint) {
        return vint(leftValue.n / rightValue.n);
      } else {
        throw "Unsupported operation <e>";
      }
    }
    case addition(left, right): {
      Value leftValue = eval(left, venv);
      Value rightValue = eval(right, venv);
      if (leftValue is vint && rightValue is vint) {
        return vint(leftValue.n + rightValue.n);
      } else {
        throw "Unsupported operation <e>";
      }
    }
    case subtraction(left, right): {
      Value leftValue = eval(left, venv);
      Value rightValue = eval(right, venv);
      if (leftValue is vint && rightValue is vint) {
        return vint(leftValue.n + rightValue.n);
      } else {
        throw "Unsupported operation <e>";
      }
    }
    case and(left, right): {
      Value leftValue = eval(left, venv);
      Value rightValue = eval(right, venv);
      if (leftValue is vbool && rightValue is vbool) {
        return vbool(leftValue.b && rightValue.b);
      } else {
        throw "Unsupported operation <e>";
      }
    }
    case or(left, right): {
      Value leftValue = eval(left, venv);
      Value rightValue = eval(right, venv);
      if (leftValue is vbool && rightValue is vbool) {
        return vbool(leftValue.b || rightValue.b);
      } else {
        throw "Unsupported operation <e>";
      }
    }
    case greaterThan(left, right): {
      Value leftValue = eval(left, venv);
      Value rightValue = eval(right, venv);
      if (leftValue is vint && rightValue is vint) {
        return vbool(leftValue.n > rightValue.n);
      } else {
        throw "Unsupported operation <e>";
      }
    }
    case lessThan(left, right): {
      Value leftValue = eval(left, venv);
      Value rightValue = eval(right, venv);
      if (leftValue is vint && rightValue is vint) {
        return vbool(leftValue.n < rightValue.n);
      } else {
        throw "Unsupported operation <e>";
      }
    }
    case lessThanEqual(left, right): {
      Value leftValue = eval(left, venv);
      Value rightValue = eval(right, venv);
      if (leftValue is vint && rightValue is vint) {
        return vbool(leftValue.n <= rightValue.n);
      } else {
        throw "Unsupported operation <e>";
      }
    }
    case greaterThanEqual(left, right): {
      Value leftValue = eval(left, venv);
      Value rightValue = eval(right, venv);
      if (leftValue is vint && rightValue is vint) {
        return vbool(leftValue.n >= rightValue.n);
      } else {
        throw "Unsupported operation <e>";
      }
    }
    case equal(left, right): {
      Value leftValue = eval(left, venv);
      Value rightValue = eval(right, venv);
      if (leftValue is vint && rightValue is vint) {
        return vbool(leftValue.n == rightValue.n);
      } else {
        throw "Unsupported operation <e>";
      }
    }
    case notEqual(left, right): {
      Value leftValue = eval(left, venv);
      Value rightValue = eval(right, venv);
      if (leftValue is vint && rightValue is vint) {
        return vbool(leftValue.n != rightValue.n);
      } else {
        throw "Unsupported operation <e>";
      }
    }
    default: throw "Unsupported expression <e>";
  }
}