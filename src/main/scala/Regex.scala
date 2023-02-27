import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Regex {

  /**
   * Return the precedence (priority) for operation
   * @param operation for which we search the precedence
   * @return 0, 1, 2, 3 (higher number is the higher precedence)
   */
  def getPrecedence(operation: String): Int = {
    val samePrecedence = List("*", "+", "?")
    if(operation.equals('('.toString) || operation.equals(')'.toString)) {
      0
    } else if(samePrecedence.contains(operation)) {
      3
    } else if(operation.equals('\u2322'.toString)) {
      2
    } else {
      1
    }
  }


  /**
   * Transform from symbol to token
   * @param symbol symbol which we want to transform
   * @return Token representation of this symbol
   */
  def fromSymbolToToken(symbol: String): String = {
    if(symbol.equals("+")) return "PLUS"
    if(symbol.equals("*")) return "STAR"
    if(symbol.equals("?")) return "MAYBE"
    if(symbol.equals("|")) return "UNION"
    if(symbol.equals('\u2322'.toString)) return "CONCAT"
    if(symbol.equals('\u03B5'.toString)) return "eps"
    symbol
  }

  /**
   * Function to preprocess a regex string. Adds the concat character, epsilon character and getting rid of syntactic sugar
   * to default and trivial operations
   * @param s Regex string
   * @return ListBuffer of strings which each one is a token of prenex
   */
  def preprocess(s:String): ListBuffer[String] = {
    val listBuffer = new ListBuffer[Char]
    listBuffer.addAll(s)

    val operations = List('+', '?', '*')
    val processedBuffer = new ListBuffer[String]
    var firstInContext = true
    while(listBuffer.nonEmpty) {
      val char = listBuffer.head
      listBuffer.remove(0)
      // char is +*? -> we need to put concat after this
      if(operations.contains(char)) {
        firstInContext = false
        processedBuffer += char.toString
      } else if(char.equals('|')){
        // char is |, dont put concat after this
        firstInContext = true
        processedBuffer += char.toString
      } else if(char.equals('(')) {
        if(!firstInContext) {
          // a CONCAT (
          processedBuffer += '\u2322'.toString // concatenation symbol
        }
        processedBuffer += char.toString
        // dont put concat after (
        firstInContext = true
      } else if(char.equals(')')) {
        // )
        processedBuffer += char.toString
        firstInContext = false
        // for next char -> put concat ) CONCAT a
      } else if (char.equals('[')){
        if(firstInContext) {
          // first [
          firstInContext = false
        } else {
          // a CONCAT [
          processedBuffer += '\u2322'.toString // concatenation symbol
        }
        val syntacticSugarBuffer = new ListBuffer[Char]
        syntacticSugarBuffer += '('
        val startChar = listBuffer.head
        listBuffer.remove(0, 2)
        val endChar = listBuffer.head
        listBuffer.remove(0, 2)
        for(symbol <- startChar to endChar) {
          syntacticSugarBuffer += symbol
          if(!symbol.equals(endChar)) syntacticSugarBuffer += '|'
        }
        syntacticSugarBuffer += ')'
        // ( A | B | C | D...)
        processedBuffer.addAll(syntacticSugarBuffer.map(_.toString))
      } else if (char.equals('\'')){
        // ''' or ' ' or '\n'
        val charBuilder: ListBuffer[Char] = new ListBuffer[Char]
        if(firstInContext) {
          // first in context
          firstInContext = false
        } else {
          // a CONCAT ' '
          processedBuffer += '\u2322'.toString // concatenation symbol
        }
        // save first ' symbol in the middle and last '
        charBuilder += char
        charBuilder += listBuffer.head
        listBuffer.remove(0, 2)
        charBuilder += char
        processedBuffer += (new mutable.StringBuilder()).addAll(charBuilder).toString()
      } else {
        // this is for default char
        if(!firstInContext) {
          // a CONCAT b
          processedBuffer += '\u2322'.toString
        } else {
          // first char in context
          firstInContext = false
        }
        processedBuffer += char.toString
      }
    }

    processedBuffer
  }

  /**
   * Construct a prenex from a regex form
   * @param str string representation of a regex
   * @return prenex representation of initial regex
   */
  def toPrenex(str: String): String = {
    val operation = List("+", "?", "*", "|", '\u2322'.toString)
    val parantheses = List("(", ")")
    val newString = str.replace("eps", '\u03B5'.toString).replace("\\n", "\n").replace("\\t", "\t")
    val processed = preprocess(newString)
    // reverse it
    val reverseString = processed.reverse
    // stack for operations
    val stack:mutable.Stack[String] = new mutable.Stack[String]()
    val prenex = new ListBuffer[String]
    for(infix <- reverseString) {
      // if default char -> add to prenex
      if (!operation.contains(infix) && !parantheses.contains(infix)) {
        prenex += infix
      } else if(infix.equals(")")) {
        // for ) push it to stack (its a opening of a new context, but string is reverset so ) first
        stack.push(infix)
      } else if(infix.equals("(")) {
        // pop till )
        while(!stack.top.equals(")")) {
          prenex += stack.pop
        }
        // pop )
        stack.pop()
      } else if(operation.contains(infix)) {
        // if stack is empty push operation
        if(stack.isEmpty) {
          stack.push(infix)
        } else {
          // calculate precedence
          val infixPrecedence: Int = getPrecedence(infix)
          var stackTopPrecedence: Int = getPrecedence(stack.top)
          // compare with stack top
          if(infixPrecedence > stackTopPrecedence) {
            stack.push(infix)
          } else if(infixPrecedence == stackTopPrecedence) {
            stack.push(infix)
          } else if(infixPrecedence < stackTopPrecedence) {
            // extract while infix precedence < stackTopPrecedence
            while(stack.nonEmpty && infixPrecedence < stackTopPrecedence) {
              prenex += stack.pop
              if(stack.nonEmpty) stackTopPrecedence = getPrecedence(stack.top)
            }
            stack.push(infix)
          }
        }
      }
    }
    // add to prenex remaining elements from stack
    while(stack.nonEmpty) {
      prenex += stack.pop
    }

    // from concat symbol, eps, +, ?, * and | go to CONCAT, eps, PLUS, MAYBE, STAR, UNION
    prenex.map(fromSymbolToToken).toList.reverse.mkString(" ")
  }
}
