import scala.collection.mutable.ListBuffer

case class Lexer (spec: String) {

  /*
    This is the main function of the lexer, it splits the given word into a list of lexems
    in the format (LEXEM, TOKEN)
  */

  /**
   * Init lexer NFA from tokenRegexPairs
   * @param tokenRegexPairs tokenRegexPairs list buffer
   * @param nfaList (handle) ListBuffer where are NFAs will be stored
   * @return lexer NFA
   */
  def initLexerNfa(tokenRegexPairs:ListBuffer[(String, String)], nfaList:ListBuffer[Nfa[Int]]) : Nfa[Int] = {
    var transitionsMap: Map[Int, Map[Char, List[Int]]] = Map.empty
    val finalStates: ListBuffer[Int] = ListBuffer.empty
    val initState = 1
    var totalStates = 1
    for (tokenRegexPair <- tokenRegexPairs) {
      // build nfa for regex of current token
      var nfa = Nfa.fromPrenex(Regex.toPrenex(tokenRegexPair._2))
      // map current nfa states to lexerNfa states
      nfa = nfa.map(x => x + totalStates)
      nfaList += nfa
      totalStates += nfa.getStates.size
      // add final states of current nfa to lexerNFA
      finalStates.addAll(nfa.finalStates)
      // add nfa transitions to lexerNFA transitions
      transitionsMap = transitionsMap ++ nfa.transitions
      // add transition for lexerInit state to current nfa on epsilon
      val initStateToNfas = transitionsMap.getOrElse(initState, Map(0.toChar -> List.empty))
      val nfaInitStatesList = initStateToNfas.getOrElse(0.toChar, List.empty)
      val transList: ListBuffer[Int] = ListBuffer.empty
      transList.addAll(nfaInitStatesList)
      transList += nfa.initState
      transitionsMap += initState -> Map(0.toChar -> transList.toList)
    }
    // build lexer nfa
    new Nfa[Int](initState, finalStates.toSet, transitionsMap)
  }

  def lex(word: String): Either[String,List[(String,String)]] = {
    // Specification split
    val specSplit:Array[String] = spec.split(";\n")
    val tokenRegexPairs:ListBuffer[(String, String)] = ListBuffer.empty
    // Create pairs (TOKEN,REGEX)
    for(lineSpec <- specSplit) {
      val splittedLine = lineSpec.split(": ")
      val pair = (splittedLine.head, splittedLine.last)
      tokenRegexPairs += pair
    }
    // Build lexer NFA
    val nfaList:ListBuffer[Nfa[Int]] = ListBuffer.empty
    val lexerNfa:Nfa[Int] = initLexerNfa(tokenRegexPairs, nfaList)
    // build lexer DFA
    val lexerDfa:Dfa[Int] = Dfa.fromNfa(lexerNfa)

    // result list of pairs (lexem, token)
    val resList:ListBuffer[(String, String)] = ListBuffer.empty
    // current position in str
    var currentPosition = 0
    // where current lexing begins in word
    var lexingBegin = 0
    // last position of a new line in word
    var lastNewLinePos = -1
    // line number in word
    var lineCounter = 0
    // current state of traversing
    var state:Int = lexerDfa.initStates.head
    // flag if lexing occurs
    var lexing = true
    // last visited final state
    var lastVisitedFinalState = -1
    // where in word last final state was found
    var lastTokenFoundAt = -1

    /**
     * Reseting the lexer and dfa traversal and saves the result to resultingList
     */
    def resetLexerAndDfaTraversal: Unit = {
      resList += getPairForResult
      lexingBegin = lastTokenFoundAt + 1
      currentPosition = lexingBegin
      lastTokenFoundAt = -1
      lastVisitedFinalState = -1
      state = lexerDfa.initStates.head
    }

    /**
     * Get pair (lexem,token) for a final state
     *
     * @return a pair of (lexem, token)
     */
    def getPairForResult: (String, String) = {
      // get nfa group state associated with last visited final state in dfa
      val groupState = lexerDfa.dfaStatesAsNfaStates(lastVisitedFinalState)
      var nfaIndex = -1
      // for each state
      for (nfaState <- groupState) {
        // if is final in lexerNFA
        if (lexerNfa.isFinal(nfaState)) {
          // check for each nfa in nfa list
          for (tokenNfa <- nfaList) {
            // if state is final in this nfa
            if (tokenNfa.isFinal(nfaState)) {
              // get the index of which token is associated
              val newIndex = nfaList.indexOf(tokenNfa)
              // get the lower token index (higher priority)
              if (nfaIndex == -1) {
                nfaIndex = newIndex
              } else if (nfaIndex > newIndex) {
                nfaIndex = newIndex
              }
            }
          }
        }
      }
      val lexem = word.slice(lexingBegin, lastTokenFoundAt + 1)
      val token = tokenRegexPairs(nfaIndex)._1
      (lexem, token)
    }

    while(lexing) {
      // if it is end of word
      if(currentPosition >= word.length) {
        // check if current state is final
        if (lexerDfa.isFinal(state)) {
          // mark as found final state at previous position and end lexing
          lastVisitedFinalState = state
          lastTokenFoundAt = currentPosition - 1
          resetLexerAndDfaTraversal
          lexing = false
        } else {
          // if is not final, check if a final state was found before, if no error, otherwise save result
          // and reset lexer traversal from where final state was found
          if (lastVisitedFinalState == -1) {
            val message = "No viable alternative at character EOF, line " + lineCounter.toString
            return Left(message)
          } else {
            resetLexerAndDfaTraversal
          }
        }
      } else {
        // if not end of word, get current char
        val currentChar = word(currentPosition)
        // check if its not EOL
        if(currentChar.equals('\n')) {
          // check if new line already met
          if(lastNewLinePos == -1) {
            lineCounter += 1
            lastNewLinePos = currentPosition
            // if met, check if it was met on a position before current (to not count same EOL twice)
          } else if(lastNewLinePos < currentPosition) {
            lineCounter += 1
            lastNewLinePos = currentPosition
          }
        }
        // get next state from state on current char
        val nextState = lexerDfa.next(state, currentChar)
        // check if current state is final and update occurence
        if (lexerDfa.isFinal(state)) {
          lastVisitedFinalState = state
          lastTokenFoundAt = currentPosition - 1
        }
        // if next state is sink
        if (lexerDfa.checkIfSink(nextState)) {
          // check if a final met before
          if (lastVisitedFinalState == -1) {
            val message = "No viable alternative at character " + currentPosition.toString + ", line " + lineCounter.toString
            return Left(message)
          } else {
            resetLexerAndDfaTraversal
          }
        } else {
          // otherwise, go to next state and consume word
          state = nextState
          currentPosition += 1
        }
      }
    }
    Right(resList.toList)
  }
}