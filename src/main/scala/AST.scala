import scala.collection.mutable

class AST(var value: String, var right: AST, var left: AST) {
}

object AST {
  /**
   * Check if token is an atom (not an operation)
   * @param token token to check
   * @return true -> atom, false otherwise
   */
  def checkIfAtom(token: String): Boolean = {
    if (token.equals("UNION")) return false
    if (token.equals("STAR")) return false
    if (token.equals("CONCAT")) return false
    if (token.equals("PLUS")) return false
    if (token.equals("MAYBE")) return false
    true
  }

  /**
   * Build an abstract syntax tree from given prenex(which is a list of tokens)
   * @param prenex list of tokens which form a prenex
   * @return constructed AST
   */
  def getASTFromPrenex(prenex: mutable.Buffer[String]) : AST = {
    // if root value is void -> return ast
    if(prenex.head.equals("void")) return new AST(prenex.head, null, null)

    // root node to build
    val root: AST = new AST("", null, null)
    if(checkIfAtom(prenex.head)) {
      val value:String = prenex.head
      prenex -= value
      return new AST(value, null, null)
    } else {
      val operation: String = prenex.head
      prenex -= operation
      root.value = operation
      if(operation.equals("STAR")) {
        root.left = getASTFromPrenex(prenex)
      } else if(operation.equals("PLUS")) {
        // plus is a concat of operand and operandStar
        root.value = "CONCAT"
        val prenex_clone = new mutable.ListBuffer[String]()
        prenex_clone.addAll(prenex)
        root.left = getASTFromPrenex(prenex)
        root.right = new AST("STAR",null, getASTFromPrenex(prenex_clone))
      } else if(operation.equals("MAYBE")) {
        // Maybe is a union of operand and eps
        root.value = "UNION"
        root.left = getASTFromPrenex(prenex)
        root.right = new AST("eps", null, null)
      } else {
        // Else for other operations (which could be CONCAT, UNION)
        root.left = getASTFromPrenex(prenex)
        root.right = getASTFromPrenex(prenex)
      }
    }

    root
  }
}
