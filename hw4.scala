import scala.io.StdIn.readLine
import scala.io.StdIn.readBoolean
import scala.io.StdIn.readInt
import scala.io.StdIn.readFloat
object hw4 extends eecs.cs478:
  def userName = "ADD YOUR NAME(S) HERE"

  // DOCUMENTATION SIGNATURE BLOCK GOES HERE
  // I DID NOT USE ANY SOURCES OR ASSISTANCE REQUIRING DOCUMENTATION
  // IN COMPLETING THIS ASSIGNMENT.
  /*
  If working by yourself, delete this comment block; otherwise, fill it out.
    Contribution of Micah Weiss: Fixed the error in the float regex caused by referncing a subgroup. Refactored the parser by reducing redundencies in binary operators.  
    Contribution of Justin Suess: Designed and implemented the overall structure of the AST and the iterative left-to-right method of parsing it. Created function to parse variable length arguments for functions and their declarations. Linked the tokenizer and the AST generator together and printed it on the screen. 
    Contribution of Bryce Valverde: Organized group meetings. Added boolean and assocaited binary operators in regex, created the tokens and implimented in parser. 
  */


  

  /***************** Language Creation, Phase 4 ***********************/
  enum Keyword:
    case FN
    case IF
    case MULTIPLY
    case DIVIDE
    case ADD
    case SUBTRACT
    case EQUALS
    case LESS
    case GREATER
    case GREATEREQUAL
    case LESSEQUAL
    case NOTEQUAL
    case AND
    case OR
  enum Punc:
    case LPAREN
    case RPAREN
    case COMMA
  enum Lit:
    case FLT(f: Float)
    case INT(i: Int)
    case STR(s: String)
    case BOOL(b: Boolean)
    case VOID
  enum Token:
    case KEYW(word : Keyword)
    case PUNC(punc : Punc)
    case ID(id : String)
    case COM(comment : String)
    case LIT(lit : Lit)


  //Language AST elements
  enum BinaryOperations:
    case MULTIPLY
    case DIVIDE
    case ADD
    case SUBTRACT
    case EQUALS
    case LESS
    case GREATER
    case GREATEREQUAL
    case LESSEQUAL
    case NOTEQUAL
    case AND
    case OR
  enum ASTNode:
    case FN_CALL(name : String, args : List[ASTNode])
    case LITERAL(lit : Lit)
    case IF_STATEMENT(cond : ASTNode, ifTrue : ASTNode, ifFalse : ASTNode)
    case FN_DECLARATION(name : String, args : List[String], body : ASTNode)
    case VARIABLE_REFERENCE(name : String)
    case LIT(lit : Lit)
    case BINARY_OPERATION(operation : BinaryOperations, left : ASTNode, right : ASTNode)


  val rest = "(.*)".r

  val FN_r = s"(@fn)$rest".r
  val STRING_r = s"(\'[a-zA-Z0-9 \t]+\')$rest".r
  val BOOL_r = s"(true|false)$rest".r
  val COMMA_r = s"(,)$rest".r
  val FLT_r = s"(-?[1-9]+[0-9]*[.][0-9]+|-?0[.][0-9]+)$rest".r 
  val INT_r = s"(-?[0-9]+)$rest".r
  val AND_r = s"(AND)$rest".r
  val OR_r = s"(OR)$rest".r
  val ID_r = s"([a-zA-Z_]+[a-zA-Z0-9_]*)$rest".r
  val RPAREN_r = s"(\\))$rest".r
  val LPAREN_r = s"(\\()$rest".r
  val WHITESPACE = s"(\t| |\n)+$rest".r
  val MUL_r = s"(\\*)$rest".r
  val ADD_r = s"(\\+)$rest".r
  val SUB_r = s"(\\-)$rest".r
  val DIV_r = s"(\\/)$rest".r
  val IF_r = s"(@if)$rest".r
  val G_r = s"(>)$rest".r
  val GE_r = s"(>=)$rest".r
  val L_r = s"(<)$rest".r
  val LE_r = s"(<=)$rest".r
  val E_r = s"(=)$rest".r
  val NE_r = s"(!=)$rest".r
  val COMMENT = s"(\\{[a-zA-Z0-9 ]*\\})$rest".r


  import scala.util.matching.Regex
  def readFile(filename: String): String =
    val fl = scala.io.Source.fromFile(filename)
    fl.mkString.replace("\n", " ").replace("\r", " ")

  import scala.util.Try
  def parse(s: String) : List[Token] =
    // println(s"Currently at $s")
    s match
      case FN_r(_, b)     => Token.KEYW(Keyword.FN) :: parse(b)
      case STRING_r(a, b) => Token.LIT(Lit.STR(a)) :: parse(b)
      case BOOL_r(a, b)     => Token.LIT(Lit.BOOL(a.toBoolean)) :: parse(b)
      case AND_r(_, b)    => Token.KEYW(Keyword.AND) :: parse(b)
      case OR_r(_, b)     => Token.KEYW(Keyword.OR) :: parse(b)
      case COMMA_r(_, b)  => Token.PUNC(Punc.COMMA) :: parse(b)
      case FLT_r(a, b)    => Token.LIT(Lit.FLT(a.toFloat)) :: parse(b)
      case INT_r(a, b)    => Token.LIT(Lit.INT(a.toInt)) :: parse(b)
      case ID_r(a, b)     => Token.ID(a) :: parse(b)
      case RPAREN_r(_, b) => Token.PUNC(Punc.RPAREN) :: parse(b)
      case LPAREN_r(_, b) => Token.PUNC(Punc.LPAREN) :: parse(b)
      case ADD_r(_, b)    => Token.KEYW(Keyword.ADD) :: parse(b)
      case SUB_r(_, b)    => Token.KEYW(Keyword.SUBTRACT) :: parse(b)
      case MUL_r(_, b)    => Token.KEYW(Keyword.MULTIPLY) :: parse(b)
      case DIV_r(_, b)    => Token.KEYW(Keyword.DIVIDE) :: parse(b)
      case IF_r(_, b)     => Token.KEYW(Keyword.IF) :: parse(b)
      case G_r(_, b)      => Token.KEYW(Keyword.GREATER) :: parse(b)
      case GE_r(_, b)     => Token.KEYW(Keyword.GREATEREQUAL) :: parse(b)
      case L_r(_, b)      => Token.KEYW(Keyword.LESS) :: parse(b)
      case LE_r(_, b)     => Token.KEYW(Keyword.LESSEQUAL) :: parse(b)
      case E_r(_, b)      => Token.KEYW(Keyword.EQUALS) :: parse(b)
      case NE_r(_, b)     => Token.KEYW(Keyword.NOTEQUAL) :: parse(b)
      // comments get ignored
      case COMMENT(_, b)  => parse(b)
      case WHITESPACE(_, b) => parse(b)
      case ""             => Nil
      case _              => println(s"Invalid token $s"); Nil



  // lexical analysis
  def wrapperFunction(f : String) : List[Token] =
    parse(readFile(f))
  

  def parseArgDecl(s : List[Token], variables : List[String]) : (List[Token], List[String]) =
    s match
      case Token.ID(a) :: Token.PUNC(Punc.COMMA) :: tail => parseArgDecl(tail, a :: variables)
      case Token.ID(a) :: Token.PUNC(Punc.RPAREN) :: tail => (tail, a :: variables)
      case _ => (s, Nil)
  def getSingleExpression(s : List[Token]) : (List[Token], ASTNode) =
    s match
      //VARIABLE REFERENCE
      case Token.ID(a) :: tail => (tail, ASTNode.VARIABLE_REFERENCE(a))
      //INT LITERAL
      case Token.LIT(Lit.INT(x)) :: tail => (tail, ASTNode.LIT(Lit.INT(x)))
      //FLOAT LITERAL 
      case Token.LIT(Lit.FLT(x)) :: tail => (tail, ASTNode.LIT(Lit.FLT(x)))
      //STRING LITERAL
      case Token.LIT(Lit.STR(x)) :: tail => (tail, ASTNode.LIT(Lit.STR(x)))
      //BOOL LITERAL
      case Token.LIT(Lit.BOOL(x)) :: tail => (tail, ASTNode.LIT(Lit.BOOL(x)))
      //FUNCTION CALL
      case Token.PUNC(Punc.LPAREN)
            :: Token.ID(name)
            :: Token.PUNC(Punc.COMMA) ::  rest => {
                var exprlist = List[ASTNode]();
                var tail2 = rest;
                var expr = ASTNode.LIT(Lit.VOID);
                while tail2.nonEmpty && tail2.head != Token.PUNC(Punc.RPAREN) do
                  val x = getSingleExpression(tail2)
                  tail2 = x._1
                  expr = x._2
                  if expr != ASTNode.LIT(Lit.VOID) then
                    exprlist = expr :: exprlist
                  //exprlist = expr :: exprlist
                  if tail2.nonEmpty && tail2.head == Token.PUNC(Punc.COMMA) then //skip comma
                    tail2 = tail2.tail
                if tail2.nonEmpty then
                  tail2 = tail2.tail //skip RPAREN
                (tail2, ASTNode.FN_CALL(name, exprlist))
            }
      //FUNCTION DECLARATION
      case Token.PUNC(Punc.LPAREN)
            :: Token.KEYW(Keyword.FN)
            :: Token.PUNC(Punc.COMMA)
            :: Token.ID(name) 
            :: Token.PUNC(Punc.COMMA) 
            :: Token.PUNC(Punc.LPAREN) 
            :: rest => {
                //println(s"rest is $rest")
                val x = parseArgDecl(rest, List[String]())
                val tail2 = x._1
                val variables = x._2
                val expr = getSingleExpression(tail2.tail) // skip COMMA after arg decl
                (expr._1.tail , ASTNode.FN_DECLARATION(name, variables, expr._2)) // skip RPAREN
            }
      //IF STATEMENT
      case Token.PUNC(Punc.LPAREN)
            :: Token.KEYW(Keyword.IF)
            :: Token.PUNC(Punc.COMMA)
            :: rest => {
                val cond = getSingleExpression(rest)
                val thenExpr = getSingleExpression(cond._1.tail) // skip COMMA
                val elseExpr = getSingleExpression(thenExpr._1.tail) // skip COMMA
                (elseExpr._1.tail, ASTNode.IF_STATEMENT(cond._2, thenExpr._2, elseExpr._2)) // skip RPAREN
            }
      //BINARY OPERATOR
      case Token.PUNC(Punc.LPAREN)
            :: Token.KEYW(a)
            :: Token.PUNC(Punc.COMMA)
            :: rest => {
                val left = getSingleExpression(rest)
                val right = getSingleExpression(left._1.tail) // skip COMMA
                a match
                  case Keyword.ADD       => (right._1.tail, ASTNode.BINARY_OPERATION(BinaryOperations.ADD, left._2, right._2)) 
                  case Keyword.SUBTRACT  => (right._1.tail, ASTNode.BINARY_OPERATION(BinaryOperations.SUBTRACT, left._2, right._2)) 
                  case Keyword.MULTIPLY  => (right._1.tail, ASTNode.BINARY_OPERATION(BinaryOperations.MULTIPLY, left._2, right._2)) 
                  case Keyword.DIVIDE    => (right._1.tail, ASTNode.BINARY_OPERATION(BinaryOperations.DIVIDE, left._2, right._2)) 
                  case Keyword.LESS      => (right._1.tail, ASTNode.BINARY_OPERATION(BinaryOperations.LESS, left._2, right._2)) 
                  case Keyword.GREATER   => (right._1.tail, ASTNode.BINARY_OPERATION(BinaryOperations.GREATER, left._2, right._2)) 
                  case Keyword.LESSEQUAL => (right._1.tail, ASTNode.BINARY_OPERATION(BinaryOperations.LESSEQUAL, left._2, right._2)) 
                  case Keyword.GREATEREQUAL => (right._1.tail, ASTNode.BINARY_OPERATION(BinaryOperations.GREATEREQUAL, left._2, right._2)) 
                  case Keyword.EQUALS    => (right._1.tail, ASTNode.BINARY_OPERATION(BinaryOperations.EQUALS, left._2, right._2)) 
                  case Keyword.NOTEQUAL  => (right._1.tail, ASTNode.BINARY_OPERATION(BinaryOperations.NOTEQUAL, left._2, right._2)) 
                  case Keyword.AND       => (right._1.tail, ASTNode.BINARY_OPERATION(BinaryOperations.AND, left._2, right._2)) 
                  case Keyword.OR        => (right._1.tail, ASTNode.BINARY_OPERATION(BinaryOperations.OR, left._2, right._2))
                  case _                 => (s, ASTNode.LIT(Lit.VOID))
            }
      case _ => (s, ASTNode.LIT(Lit.VOID))
  def generateAst(tokens : List[Token]): List[ASTNode] =
    // create a list of ASTNodes
    getSingleExpression(tokens) match
      case (_,ASTNode.LIT(Lit.VOID))  => Nil // end of AST
      // getSingleExpression(tokens) returns a tuple of (remaining tokens, ASTNode)
      case (remainder, expr) => expr :: generateAst(remainder)
  enum Function:
    case Write
    case ReadStr
    case ReadBool
    case ReadFloat
    case ReadInt
    case Custom(code : ASTNode, args : List[String])
  class  Environment(val functions : Map[String, Function], val variables : Map[String, Lit]):
    def ::(other : Environment) : Environment = Environment(functions ++ other.functions, variables ++ other.variables)
    def ::(other : Map[String,Lit]) : Environment = Environment(functions, variables ++ other.toMap)
    //def ::(other : Map[String,Function]) : Environment = Environment(functions ++ other.toMap, variables)
    def ::(other : (String, Function)) : Environment = Environment(functions + other, variables)
  def default_env : Environment =
    return Environment(Map("write" -> Function.Write, "readStr" -> Function.ReadStr, "readBool" -> Function.ReadBool, "readFloat" -> Function.ReadFloat, "readInt" -> Function.ReadInt), Map())
  def execute(nodes : List[ASTNode], env : Environment) : Unit =
    nodes match
      case head :: tail => execute(tail, execute_single_astnode(head, env)._1)
      case _ => ()
    
  def execute_single_astnode(node : ASTNode, env : Environment) : (Environment, Lit) = 
    node match
      case ASTNode.FN_CALL(name, args) => env.functions(name) match
        case Function.Write => 
          for item <- args do
            execute_single_astnode(item, env)._2 match
              case Lit.FLT(f)  => print(f)
              case Lit.INT(i)  => print(i)
              case Lit.STR(s)  => print(s)
              case Lit.BOOL(b) => print(b)
              case Lit.VOID    => print("You just printed nothing lol")
          (env, Lit.VOID)
        case Function.ReadStr      => (env, Lit.STR(readLine()))
        case Function.ReadBool     => (env, Lit.BOOL(readBoolean()))
        case Function.ReadFloat    => (env, Lit.FLT(readFloat()))
        //Add more of these read functions, for boolean, float, int, etc.
        case Function.Custom(code,argnames) => {
           var argsiter = args.iterator
           val newvars = argnames.map(arg => (arg, execute_single_astnode(argsiter.next, env)._2)).toMap;
           (newvars :: env, execute_single_astnode(code, env)._2) 
        }
      
      case ASTNode.LITERAL(lit) => (env, lit)
      case ASTNode.IF_STATEMENT(cond, ifTrue, ifFalse) => 
        if execute_single_astnode(cond, env)._2 match //various things that can be used inside of conditionals
          case Lit.FLT(f)  => !f.isNaN()
          case Lit.INT(i)  => i != 0
          case Lit.STR(s)  => s != ""
          case Lit.BOOL(b) => b
          case Lit.VOID    => false
        then
          execute_single_astnode(ifTrue, env)
        else
          execute_single_astnode(ifFalse, env)
      case ASTNode.FN_DECLARATION(name, args, body) => 
        ((name, Function.Custom(body, args)) :: env, Lit.VOID)
      case ASTNode.VARIABLE_REFERENCE(name) => ???
      case ASTNode.LIT(lit) => (env, lit)
      case ASTNode.BINARY_OPERATION(operation, left, right) => 
        val leftEval = execute_single_astnode(left, env)
        val rightEval = execute_single_astnode(right, leftEval._1)
        (leftEval._2, rightEval._2) match
          case (Lit.FLT(l), Lit.FLT(r)) => 
            operation match
              case BinaryOperations.ADD => (rightEval._1, Lit.FLT(l + r))
              case BinaryOperations.SUBTRACT => (rightEval._1, Lit.FLT(l - r))
              case BinaryOperations.DIVIDE => (rightEval._1, Lit.FLT(l / r))
              case BinaryOperations.MULTIPLY => (rightEval._1, Lit.FLT(l * r))
              case BinaryOperations.NOTEQUAL => if l != r then (rightEval._1, Lit.BOOL(true)) else (rightEval._1, Lit.BOOL(false))
              case BinaryOperations.EQUALS => if l == r then (rightEval._1, Lit.BOOL(true)) else (rightEval._1, Lit.BOOL(false))
              case BinaryOperations.LESS => if l < r then (rightEval._1, Lit.BOOL(true)) else (rightEval._1, Lit.BOOL(false))
              case BinaryOperations.LESSEQUAL => if l <= r then (rightEval._1, Lit.BOOL(true)) else (rightEval._1, Lit.BOOL(false))
              case BinaryOperations.GREATER => if l > r then (rightEval._1, Lit.BOOL(true)) else (rightEval._1, Lit.BOOL(false))
              case BinaryOperations.GREATEREQUAL => if l >= r then (rightEval._1, Lit.BOOL(true)) else (rightEval._1, Lit.BOOL(false))
              case BinaryOperations.AND => if !l.isNaN() && !r.isNaN() then (rightEval._1, Lit.BOOL(true)) else (rightEval._1, Lit.BOOL(false))
              case BinaryOperations.OR => if !l.isNaN() || !r.isNaN() then (rightEval._1, Lit.BOOL(true)) else (rightEval._1, Lit.BOOL(false))
          case (Lit.INT(l), Lit.INT(r)) =>
            operation match
              case BinaryOperations.ADD => (rightEval._1, Lit.INT(l + r))
              case BinaryOperations.SUBTRACT => (rightEval._1, Lit.INT(l - r))
              case BinaryOperations.DIVIDE => (rightEval._1, Lit.INT(l / r))
              case BinaryOperations.MULTIPLY => (rightEval._1, Lit.INT(l * r))
              case BinaryOperations.NOTEQUAL => if l != r then (rightEval._1, Lit.BOOL(true)) else (rightEval._1, Lit.BOOL(false))
              case BinaryOperations.EQUALS => if l == r then (rightEval._1, Lit.BOOL(true)) else (rightEval._1, Lit.BOOL(false))
              case BinaryOperations.LESS => if l < r then (rightEval._1, Lit.BOOL(true)) else (rightEval._1, Lit.BOOL(false))
              case BinaryOperations.LESSEQUAL => if l <= r then (rightEval._1, Lit.BOOL(true)) else (rightEval._1, Lit.BOOL(false))
              case BinaryOperations.GREATER => if l > r then (rightEval._1, Lit.BOOL(true)) else (rightEval._1, Lit.BOOL(false))
              case BinaryOperations.GREATEREQUAL => if l >= r then (rightEval._1, Lit.BOOL(true)) else (rightEval._1, Lit.BOOL(false))
              case BinaryOperations.AND => if l != 0 && r != 0 then (rightEval._1, Lit.BOOL(true)) else (rightEval._1, Lit.BOOL(false))
              case BinaryOperations.OR => if l != 0 || r != 0 then (rightEval._1, Lit.BOOL(true)) else (rightEval._1, Lit.BOOL(false))
          case (Lit.STR(l), Lit.STR(r)) =>
            operation match
              case BinaryOperations.ADD => (rightEval._1, Lit.STR(l + r))
              case BinaryOperations.NOTEQUAL => if l != r then (rightEval._1, Lit.BOOL(true)) else (rightEval._1, Lit.BOOL(false))
              case BinaryOperations.EQUALS => if l == r then (rightEval._1, Lit.BOOL(true)) else (rightEval._1, Lit.BOOL(false))
              case BinaryOperations.LESS => if l < r then (rightEval._1, Lit.BOOL(true)) else (rightEval._1, Lit.BOOL(false))
              case BinaryOperations.LESSEQUAL => if l <= r then (rightEval._1, Lit.BOOL(true)) else (rightEval._1, Lit.BOOL(false))
              case BinaryOperations.GREATER => if l > r then (rightEval._1, Lit.BOOL(true)) else (rightEval._1, Lit.BOOL(false))
              case BinaryOperations.GREATEREQUAL => if l >= r then (rightEval._1, Lit.BOOL(true)) else (rightEval._1, Lit.BOOL(false))
              case BinaryOperations.AND => if l != "" && r != "" then (rightEval._1, Lit.BOOL(true)) else (rightEval._1, Lit.BOOL(false))
              case BinaryOperations.OR => if l != "" || r != "" then (rightEval._1, Lit.BOOL(true)) else (rightEval._1, Lit.BOOL(false))
          case (Lit.BOOL(l), Lit.BOOL(r)) =>
            operation match
              case BinaryOperations.NOTEQUAL => if l != r then (rightEval._1, Lit.BOOL(true)) else (rightEval._1, Lit.BOOL(false))
              case BinaryOperations.EQUALS => if l == r then (rightEval._1, Lit.BOOL(true)) else (rightEval._1, Lit.BOOL(false))
              case BinaryOperations.AND => if l && r then (rightEval._1, Lit.BOOL(true)) else (rightEval._1, Lit.BOOL(false))
              case BinaryOperations.OR => if l || r then (rightEval._1, Lit.BOOL(true)) else (rightEval._1, Lit.BOOL(false))
          case (Lit.STR(l), Lit.BOOL(r)) => 
            operation match
              case BinaryOperations.NOTEQUAL => if l != r.toString() then (rightEval._1, Lit.BOOL(true)) else (rightEval._1, Lit.BOOL(false))
              case BinaryOperations.EQUALS => if l == r.toString() then (rightEval._1, Lit.BOOL(true)) else (rightEval._1, Lit.BOOL(false))
              case BinaryOperations.AND => if l != "" && r then (rightEval._1, Lit.BOOL(true)) else (rightEval._1, Lit.BOOL(false))
              case BinaryOperations.OR => if l != "" || r then (rightEval._1, Lit.BOOL(true)) else (rightEval._1, Lit.BOOL(false))
          case (Lit.STR(l), Lit.INT(r)) => 
            operation match
              case BinaryOperations.NOTEQUAL => if l != r.toString() then (rightEval._1, Lit.BOOL(true)) else (rightEval._1, Lit.BOOL(false))
              case BinaryOperations.EQUALS => if l == r.toString() then (rightEval._1, Lit.BOOL(true)) else (rightEval._1, Lit.BOOL(false))
              case BinaryOperations.AND => if l != "" && r != 0 then (rightEval._1, Lit.BOOL(true)) else (rightEval._1, Lit.BOOL(false))
              case BinaryOperations.OR => if l != "" || r != 0 then (rightEval._1, Lit.BOOL(true)) else (rightEval._1, Lit.BOOL(false))
          case (Lit.STR(l), Lit.FLT(r)) =>
            operation match
              case BinaryOperations.NOTEQUAL => if l != r.toString() then (rightEval._1, Lit.BOOL(true)) else (rightEval._1, Lit.BOOL(false))
              case BinaryOperations.EQUALS => if l == r.toString() then (rightEval._1, Lit.BOOL(true)) else (rightEval._1, Lit.BOOL(false))
              case BinaryOperations.AND => if l != "" && r != 0.0 then (rightEval._1, Lit.BOOL(true)) else (rightEval._1, Lit.BOOL(false))
              case BinaryOperations.OR => if l != "" || r != 0.0 then (rightEval._1, Lit.BOOL(true)) else (rightEval._1, Lit.BOOL(false))
          case (Lit.INT(l), Lit.FLT(r)) =>
            operation match
              case BinaryOperations.ADD => (rightEval._1, Lit.FLT(l + r.toInt))
              case BinaryOperations.SUBTRACT => (rightEval._1, Lit.FLT(l - r.toInt))
              case BinaryOperations.MULTIPLY => (rightEval._1, Lit.FLT(l * r.toInt))
              case BinaryOperations.DIVIDE => (rightEval._1, Lit.FLT(l / r.toInt))
              case BinaryOperations.NOTEQUAL => if l != r.toInt then (rightEval._1, Lit.BOOL(true)) else (rightEval._1, Lit.BOOL(false))
              case BinaryOperations.EQUALS => if l == r.toInt then (rightEval._1, Lit.BOOL(true)) else (rightEval._1, Lit.BOOL(false))
              case BinaryOperations.LESS => if l < r.toInt then (rightEval._1, Lit.BOOL(true)) else (rightEval._1, Lit.BOOL(false))
              case BinaryOperations.LESSEQUAL => if l <= r.toInt then (rightEval._1, Lit.BOOL(true)) else (rightEval._1, Lit.BOOL(false))
              case BinaryOperations.GREATER => if l > r.toInt then (rightEval._1, Lit.BOOL(true)) else (rightEval._1, Lit.BOOL(false))
              case BinaryOperations.GREATEREQUAL => if l >= r.toInt then (rightEval._1, Lit.BOOL(true)) else (rightEval._1, Lit.BOOL(false))
              case BinaryOperations.AND => if l != 0 && r.toInt != 0 then (rightEval._1, Lit.BOOL(true)) else (rightEval._1, Lit.BOOL(false))
              case BinaryOperations.OR => if l != 0 || r.toInt != 0 then (rightEval._1, Lit.BOOL(true)) else (rightEval._1, Lit.BOOL(false))

          case _ => ???

  @main def rum =
    val tokens = wrapperFunction("hello_world.lang")
    println(s"tokens are $tokens")
    val ast = generateAst(tokens)
    println(ast)
    execute(ast, default_env)
