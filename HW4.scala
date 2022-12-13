// HW4, CMSC 22100, Autumn 2022

import scala.io.Source

import scala.collection.mutable.ListBuffer

enum Token:
  case TLParen
  case TRParen
  case TLBrack  
  case TRBrack
  case TDot
  case KW_tru
  case KW_fls
  case TNat(n:Int)
  case TVar(varName:String)   // one or more lowercase letters, but not "tru" or "fls"
  case TIdent(varName:String) // one or more uppercase letters

enum SurfaceTerm:
  case SVar(varName:String)  
  case SAbs(varNames:List[String],body:SurfaceTerm)
  case SApp(t1:SurfaceTerm,t2:SurfaceTerm)
  case STru
  case SFls
  case SNat(n:Int)
  case SIdent(varName:String)

enum ULCTerm:
  case UVar(varName:String)
  case UAbs(varName:String,body:ULCTerm)
  case UApp(t1:ULCTerm,t2:ULCTerm)

import Token.*
import SurfaceTerm.*
import ULCTerm.*

def isV(t: ULCTerm): Boolean = t match {
  case UAbs(_,_) => true
  case _ => false
}

// ===== scanner

def nextToken(cs:List[Char]): Option[(Token,List[Char])] = cs match {
  case Nil => None
  case '('::tl => Some(TLParen,tl)
  case ')'::tl => Some(TRParen,tl)
  case '['::tl => Some(TLBrack,tl)
  case ']'::tl => Some(TRBrack,tl)
  case '.'::tl => Some(TDot,tl)
  case 't'::'r'::'u'::tl => Some(KW_tru,tl)
  case 'f'::'l'::'s'::tl => Some(KW_fls,tl)
  case c::tl if c.isWhitespace => nextToken(tl)
  //if they are variables lower case, upper case, and then a number case
  //want to group together the variables and input the string
  case c::tl if c.isLower => Some(TVar(lower(cs.mkString)(0)), tl.drop(lower(cs.mkString)(1).toInt))
  case c::tl if c.isUpper => Some(TIdent(upper(cs.mkString)(0)), tl.drop(upper(cs.mkString)(1).toInt))
  case c::tl if c.isDigit => Some(TIdent(digit(cs.mkString)(0)), tl.drop(digit(cs.mkString)(1).toInt))
  case _ => throw new Exception("nextToken error: inputted value not acceptable")
}

def lower(term:String): List[String] = 
  var vars = term.toList
  var ans = ""
  var i = 0
  while(i < vars.length && (vars(i).isLower))
    ans += vars(i).toString
    i += 1

  return List(ans, (i - 1).toString)

def upper(term:String): List[String] = 
  var vars = term.toList
  var ans = ""
  var i = 0
  while(i < vars.length && (vars(i).isUpper))
    ans += vars(i).toString
    i += 1

  return List(ans, (i - 1).toString)

def digit(term:String): List[String] = 
  var vars = term.toList
  var ans = ""
  var i = 0
  while(i < vars.length && (vars(i).isDigit))
    ans += vars(i).toString
    i += 1

  return List(ans, (i - 1).toString)


//when it hits a lower case token, take all of the lower case letters and make them a token
//when it hits an upper case toke, take all of the uppercase letters and make them a token
def scan(code:String): List[Token] =
  def lp(cs:List[Char]): List[Token] = nextToken(cs) match {
    case None => Nil
    case Some(tok,cs) => tok::lp(cs)
  }
  return lp(code.toList)



// ===== parser

def nextTerm(toks:List[Token]): Option[(SurfaceTerm,List[Token])] = toks match {
  case Nil => None
  case TVar(x)::tl => Some(SVar(x),tl)
  case KW_tru::tl => Some(STru,tl)
  case KW_fls::tl => Some(SFls,tl)
  case TNat(n)::tl => Some(SNat(n),tl)
  case TIdent(x)::tl => Some(SIdent(x),tl)
  //case TLParen::tl => throw new Exception("todo: parse app")
  case TLParen:: tl => nextTerm(tl) match
    case None => throw new Exception("nextTerm Error: application ended unexpectedly")
    case Some(token1, tl2) => nextTerm(tl2) match 
      case Some(token2, token3::tl3) => token3 match
        case Token.TRParen => Some(SApp(token1, token2), tl3)
        case _ => throw new Exception("nextTerm Error: application missing necessary terms")
      case _ => throw new Exception("nextTerm Error: application missing necessary terms")
  //[t1 t2]
  case TLBrack::TVar(x)::tl => 
    var head = abs(TVar(x)::tl)
    var i = head(head.length - 1).toInt
    head.remove(head.length - 1)
    var body = tl.drop(i) //removing the TDot as well as the head variables
    body.drop(i)
    nextTerm(body) match
      case Some(surfaceTerm, t1 :: tail) => t1 match
        case Token.TRBrack => Some(SAbs(head.toList, surfaceTerm), tail)
        case _ => throw new Exception(s"parse error: abstraction")
      case _ => throw new Exception(s"parse error: abstraction")
  case _ => throw new Exception(s"parse error: ${toks.mkString}")
}


//function that returns the varNames for the head of the abstraction and the number of
//tokens that are included in the head of the abstraction
def abs(toks:List[Token]): ListBuffer[String] =
  var ans = new ListBuffer[String]()
  var i = 0
  while(i < toks.length && (toks(i) != Token.TDot))
    //print(toks(i))
    toks(i) match
      case TVar(x) => 
        ans += x
        i += 1
      case _ => i += 1

  ans += (i).toString
  return ans

def parse(toks:List[Token]): SurfaceTerm = nextTerm(toks) match {
  case None => throw new Exception("not enough program")
  case Some(st,Nil) => st
  case Some(_,_) => throw new Exception("too much program")
}

// ===== rewriter

//def rewrite(t: SurfaceTerm): ULCTerm = throw new Exception("todo: rewrite a surface term to ULC term")
def rewrite(t: SurfaceTerm): ULCTerm = 
  t match 
    case SVar(varName) => UVar(varName)
    case SAbs(varName, body) => varName match
      case head :: Nil => UAbs(varName.mkString, rewrite(body))
      case head :: tail => UAbs(head, rewrite(SAbs(tail, body)))
      case _ => throw new Exception("Rewrite Error: SAbs ended unexpectedly")
    case SApp(t1, t2) => UApp(rewrite(t1), rewrite(t2))
    case SIdent(varName) => UAbs(varName, UVar(varName))//UVar(varName)
    case STru => UAbs("t", UAbs("f", rewrite(SVar("t"))))
    case SFls => UAbs("t", UAbs("f", rewrite(SVar("f"))))
    case SNat(n) => UVar(natToName(n))

def natToName(n: Int): String =
  n match
    case 0 => "Succ(0)"
    case _ => ("Succ(").concat(natToName(n-1)).concat(")")

// ===== evaluator

//def fv(t: ULCTerm): Set[String] = throw new Exception("todo: fv (compute set of free variables)")
def fv(t: ULCTerm): Set[String] = 
  t match
    case UAbs(head, body) => 
      body match  
        case UVar(x) => Set(x) ++ Set(head)
        case _ => Set(head) ++ fv(body)
    case UApp(t1, t2) => 
      t1 match
        case UVar(t1) => 
          t2 match
            case UVar(t2) => Set(t1) ++ Set(t2)
            case _ => Set(t1) ++ fv(t2)
        case _ => 
          t2 match
            case UVar(t2) => fv(t1) ++ Set(t2)
            case _ => fv(t1) ++ fv(t2)
    case UVar(name) => Set(name)


var freshVarSeed = 0
def freshVarName(): String =
  val name:String = s"_v$freshVarSeed"
  freshVarSeed += 1
  return name

// subst(x,s,t1) means "rewrite x to s in t1"
//def subst(x: String, s: ULCTerm, t1: ULCTerm): ULCTerm = throw new Exception("todo: subst")
def subst(x: String, s:ULCTerm, t1:ULCTerm): ULCTerm = 
  t1 match
    case UVar(variable) =>
      if(variable == x)
        return s
      else
        return t1
    
    case UApp(ulc1, ulc2) =>
      return UApp(subst(x, s, ulc1), subst(x, s, ulc2))

    case UAbs(strhead, ulcbody) =>
      if(x == strhead)
        return t1
      else
        //print("here\n")
        if(fv(s).contains(strhead))
          var strheadprime = freshVarName()
          //print("here\n")
          return subst(x, s, UAbs(strheadprime, subst(strhead, UVar(strheadprime), ulcbody)))
        else
          //print("head is not in free val list\n")
          return UAbs(strhead, subst(x, s, ulcbody))
      

def stepCBV(t: ULCTerm): Option[ULCTerm] = 
  t match
    case UApp(t1, t2) => 
      t1 match
        case UAbs(varName, body) => //when M is an abstraction
          t2 match  
            case UAbs(varName2, body2) => stepCBV(subst(varName, t2, body))
            case _ => stepCBV(t2)
        case UApp(t3, t4) => 
          t2 match
            case UAbs(varName3, body) => stepCBV(t1)
            case _ => None
        case UVar(varName) => Some(t)
      t2 match
        case UVar(varName) => Some(t)
        case _ => None
    case _ => None


def stepLazy(t: ULCTerm): Option[ULCTerm] = 
  t match
      case UApp(t1, t2) => t1 match
        case UAbs(varName, ulcbody) => Some(subst(varName, t2, ulcbody)) //no reductions inside of abstractions
        case UApp(t3, t4) => stepLazy(t3)
        case _ => None
      case _ => None

def stepBeta(t: ULCTerm): Option[ULCTerm] = 
  t match
    case UApp(UVar(var1), UVar(t2)) => Some(t)
      case UApp(t1, t2) => t1 match 
        case UAbs(varName, body) => stepBeta(subst(varName, t2, body))
        case UApp(t3, t4) => stepBeta(t1) match
          case Some(t5) => Some(UApp(t5, t2))
          case _ => None
        case _ => None
    case UAbs(varName, body) => body match
      case UApp(t6, t7) => stepBeta(body) match
        case Some(t8) => Some(UAbs(varName, t8))
        case _ => None
      case _ => None
    case _ => None


// inverse parsing for ULCTerms
def unparse(t: ULCTerm): String = //throw new Exception("todo: unparse ULC terms")
  t match
    case UApp(t1, t2) => 
      ("(").concat(unparse(t1)).concat(" ").concat(unparse(t2)).concat(")")
    case UAbs(varName, body) =>
      ("[").concat(varName).concat(" . ").concat(unparse(body)).concat("]")
    case UVar(varName) if (varName.toList(0)).isUpper =>
      ("[").concat(varName).concat(" . ").concat(varName).concat("]")
    case UVar(varName) if (varName.toList(0)).isLower =>
      varName
    case _ => ""



// ===== no tasks past this point

def interpret(evalSystem:Char,code:String): Unit =
  val tokens = scan(code)
  val s = parse(tokens)
  val u = rewrite(s)
  val step: ULCTerm => Option[ULCTerm] = evalSystem match {
    case 'v' => stepCBV
    case 'z' => stepLazy
    case 'b' => stepBeta
    case _ => throw new Exception(s"unrecognized eval system $evalSystem")
  }
  def lp(t1:ULCTerm): List[ULCTerm] = step(t1) match {
    case None => t1::Nil
    case Some(t2) => t1::lp(t2)
  }
  val steps = lp(u)
  println(s"code:\n$code")
  println()
  println(s"rewritten:\n${unparse(u)}")
  println()
  println("evaluation:")
  for t <- steps do {
    println(unparse(t))
  }

@main def ulc(evalSystem:String, codeOrFilename:String): Unit =
  var code = codeOrFilename
  try {
    code = Source.fromFile(codeOrFilename).getLines.mkString
  } catch {
    case e: java.io.FileNotFoundException => "pass" // do nothing
  }
  interpret(evalSystem(0),code)

@main def test_lower() =
  print(s"testing lower helper aa bbb: ${lower("aa bbb")}\n")
  print(s"testing lower helper AA bbb: ${lower("AA bbb")}\n")
  print(s"testing lower helper aa(bbb): ${lower("aa(bbb)")}\n")

@main def test_nextToken() = 
  print(s"testing nextToken aa: ${nextToken(("a").toList)}\n")
  print(s"testing nextToken aaa: ${nextToken(("aaa").toList)}\n")
  print(s"testing nextToken a bbb: ${nextToken(("a bbb").toList)}\n")

  print("\n")

  print(s"testing nextToken A: ${nextToken(("A").toList)}\n")
  print(s"testing nextToken AA b: ${nextToken(("AA b").toList)}\n")

  print("\n")

  print(s"testing nextToken 123 {a}: ${nextToken(("123 {a}").toList)}\n")
  print(s"testing nextToken 1111: ${nextToken(("1111").toList)}\n")

@main def test_scan() = 
  print(s"testing scan [a( b.(C (b a))] x]: ${scan("[a( b.(C (b a))] x)")}\n")
  print(s"testing scan: ${scan("((tru A) B)")}\n")

@main def test_abs() = 
  print(s"testing abs b, a, aa: ${abs(List(TVar("b"), TVar("a"), TVar("aa")))}\n") 
  print(s"testing abs b, (, ., a: ${abs(List(TVar("b"), TLParen, TDot, TVar("a")))}\n")

@main def test_nextTerm() = 
  print(s"testing nextTerm: ${nextTerm(List(TLBrack, TVar("a"), TLParen, TVar("b"), TDot, TLParen, TIdent("C"), TLParen, TVar("b"), TVar("a"), TRParen, TRParen, TRBrack, TVar("x"), TRParen))}\n")
  print(s"testing nextTerm: ${nextTerm(List(TLParen, TLParen, KW_tru, TIdent("A"), TRParen, TIdent("B"), TRParen))}\n")

@main def test_parse() = 
  print(s"testing parse: ${parse(List(TLParen,TLBrack, TVar("a"), TLParen, TVar("b"), TDot, TLParen, TIdent("C"), TLParen, TVar("b"), TVar("a"), TRParen, TRParen, TRBrack, TVar("x"), TRParen))}\n")
  print(s"testing parse: ${parse(List(TLParen, TLParen, KW_tru, TIdent("A"), TRParen, TIdent("B"), TRParen))}\n")

@main def test_natToName() =
  print(s"testing natToName of 3: ${natToName(3)}\n")

@main def test_rewrite() = 
  print(s"testing rewrite: ${rewrite(SApp(SAbs(List("a", "b"),SApp(SIdent("C"),SApp(SVar("b"),SVar("a")))),SVar("x")))}\n")
  print(s"testing rewrite: ${rewrite(SApp(SApp(STru,SIdent("A")),SIdent("B")))}\n")

@main def test_fv() = 
  print(s"testing fv: ${fv(UApp(UApp(UAbs("t",UAbs("f",UVar("t"))),UVar("A")),UVar("B")))}\n")
  print(s"testing fv: ${fv(UApp(UAbs("a",UAbs("b",UApp(UAbs("C",UVar("C")),UApp(UVar("b"),UVar("a"))))),UVar("x")))}\n")
  print(s"testing fv: ${fv(UAbs("y", UApp(UVar("y"), UVar("y"))))}\n")

@main def test_subst() = 
  //print(s"testing subst: ${subst("a", UVar("x"), UAbs("a",UAbs("b",UApp(UVar("C"),UApp(UVar("b"),UVar("a"))))))}\n")
  
  print(s"testing subst: ${subst("a", UVar("x"), UVar("a"))}\n")
  print(s"testing subst: ${subst("a", UVar("x"), UApp(UVar("a"), UVar("a")))}\n")
  print(s"testing subst: ${subst("b", UVar("x"), UApp(UVar("a"), UVar("a")))}\n")
  print(s"testing subst: ${subst("x", UVar("s"), UAbs("x", UVar("y")))}\n")
  print(s"testing subst: ${subst("x", UVar("s"), UAbs("y", UApp(UVar("y"), UVar("y"))))}\n")
  print(s"testing subst: ${subst("x", UVar("y"), UAbs("y", UApp(UVar("x"), UVar("y"))))}\n")
  print(s"testing subst: ${subst("x", UVar("z"), UAbs("y", UVar("z")))}\n")

@main def test_lazy() = 
  print(s"testing step lazy: ${stepLazy(UApp(UAbs("a",UAbs("b",UApp(UAbs("C",UVar("C")),UApp(UVar("b"),UVar("a"))))),UVar("x")))}")

@main def test_CBV() = 
  print(s"testing step CBV: ${stepCBV(UApp(UAbs("a",UAbs("b",UApp(UAbs("C",UVar("C")),UApp(UVar("b"),UVar("a"))))),UVar("x")))}")

@main def test_beta() = 
  print(s"testing step beta: ${stepBeta(UApp(UAbs("a",UAbs("b",UApp(UAbs("C",UVar("C")),UApp(UVar("b"),UVar("a"))))),UVar("x")))}")t

@main def test_unparse() = 
  print(s"testing unparse lazy example: ${unparse(UAbs("b",UApp(UAbs("C",UVar("C")),UApp(UVar("b"),UVar("x")))))}\n")
  print(s"testing unparse call-by-value example: ${unparse(UAbs("b",UApp(UAbs("C",UVar("C")),UApp(UVar("b"),UVar("x")))))}\n")
  print(s"testing unparse beta example: ${unparse(UAbs("b",UApp(UAbs("C",UVar("C")),UApp(UVar("b"),UVar("x")))))}\n")

// to run this code, supply either 'v', 'z', or 'b' to choose eval system
// after that, supply either filename or code directly

// ex: $ scala ulc v "(A 0)"
// ...this will evaluate (A 0) under CBV

// ex: $ scala ulc z "(A x)"
// ...this will evaluate (A x) under lazy evaluation

// ex: $ scala ulc b "(A x)"
// ...this will evaluate (A x) under full-beta reduction

// ex: $ scala ulc v foobar.sulc
// ...this assumes you have a file "foobar.sulc" containing a surface-ULC program
// ...if you do, it reads in the code from the file and interprets it under CBV
// ...if you don't, the interpreter assumes "foobar.sulc" is code (and cannot parse it)
