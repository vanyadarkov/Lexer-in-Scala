import scala.collection.mutable.ListBuffer

class Nfa[A](var initState:A, var finalStates:Set[A], var transitions:Map[A, Map[Char, List[A]]]) {
  /**
   * Map a NFA of A to NFA of B
   * @param f function for A to B
   * @tparam B new type
   * @return new NFA[B]
   */
  def map[B](f: A => B) : Nfa[B] = {
    var newTransitions:Map[B, Map[Char, List[B]]] = Map.empty[B, Map[Char, List[B]]]
    newTransitions = transitions map {case (state, nextTransitions) => f(state) -> (nextTransitions map {
      case (char, nextStates) => char -> nextStates.map(f)
    })}
    new Nfa[B](f(initState), finalStates.map(f), newTransitions)
  }

  /**
   * Get all next states from state with Epsilon Closure
   * @param state state
   * @return set of next states (including itself)
   */
  def epsilonClosuresForState(state:A, visited:ListBuffer[A]) : Set[A] = {
    val states:ListBuffer[A] = new ListBuffer[A]()
    if(!visited.contains(state)) {
      visited += state
      states += state
      // check if current state have transitions
      if (transitions.contains(state)) {
        // check if current state have epsilon transition
        if (transitions(state).contains(0.toChar)) {
          // get epsilon closures from all next epsilon closed state
          for (epsilonClosedState <- transitions(state)(0.toChar)) {
            val epsilonClosures = epsilonClosuresForState(epsilonClosedState, visited)
            for (newState <- epsilonClosures) {
              states += newState
            }
          }
        }
      }
    }
    states.toSet
  }

  /**
   * Get next states from state with c char transition
   * @param state current state
   * @param c transition char
   * @return
   */
  def next(state:A, c: Char): Set[A] = {
    if(transitions.contains(state)) {
      if(transitions(state).contains(c)) {
        return transitions(state)(c).toSet
      }
    }
    Set.empty[A]
  }

  /**
   * Consume a string
   * @param str string to consume
   * @param currentState current state
   * @return true if word is consumed and final state reached, false otherwise
   */
  def consume(str: String, currentState: A) : Boolean = {
    if(str.isEmpty && isFinal(currentState)) return true
    var currentChar:Char = 0.toChar
    if(str.nonEmpty) currentChar = str.head
    // if we have transitions for current state
    if(transitions.contains(currentState)) {
      // all transitions
      val allNexts = transitions(currentState)
      // if from current state we can go with current char
      if (allNexts.contains(currentChar)) {
        for (next <- allNexts(currentChar)) {
          val ret = consume(str.tail, next)
          if (ret) return true
        }
      } else if (allNexts.contains(0.toChar)) {
        // if we have epsilon transition
        for (next <- allNexts(0.toChar)) {
          val ret = consume(str, next)
          if (ret) return true
        }
      }
    }
    false
  }

  /**
   * Check if this nfa accepts str
   * @param str string to check for acceptance
   * @return true if accepted, false otherwise
   */
  def accepts(str: String): Boolean = {
    if(transitions.isEmpty) {
      return false
    }
    consume(str, initState)
  }

  /**
   * Return all states of NFA
   * @return set of all states
   */
  def getStates : Set[A] = {
    var states:Set[A] = Set.empty[A]
    for((state, adjacency) <- transitions) {
      states += state
      for((_, nextStates) <- adjacency) {
        for(nextState <- nextStates) {
          states += nextState
        }
      }
    }

    states
  }

  /**
   * Check if state is final
   * @param state state
   * @return true if final, false otherwise
   */
  def isFinal(state: A): Boolean = {
    if(finalStates.contains(state)) return true
    false
  }

  /**
   * Alphabet of current nfa
   * @return set of chars which represents alphabet of this nfa
   */
  def getAlphabet: Set[Char] = {
    var alphabet:Set[Char] = Set.empty
    for((_, adjacency) <- transitions) {
      for(key <- adjacency.keySet)
        if(!key.equals(0.toChar)) alphabet += key
    }
    alphabet
  }
}

object Nfa {
  /**
   * Split a string by spaces, considering space token, quote token, weird characters tokens and others
   * @param prenex - prenex
   * @return
   */
  def splitPrenex(prenex: String): List[String] = {
    prenex.split(" ").toList.foldLeft(List.empty[String])((acc, x) =>
      if (x.equals("\'")) {
        if(acc.isEmpty) {
          acc :+ "\'"
        }else {
          if (acc.last.equals("\'")) acc.init :+ " "
          else acc :+ "\'"
        }
      } else {
        if (x.contains("\'")) acc :+ x.tail.init
        else acc :+ x
      })
  }

  /**
   * Build a nfa from abstract syntax tree with initial state
   * @note Recursive function
   * @param node root from which we build current nfa
   * @param initialState initial state of this NFA
   * @return NFA with Int represented states
   */
  def nfaFromAST(node:AST, initialState:Int) : Nfa[Int] = {
    // Transitions map
    var transitions: Map[Int, Map[Char, List[Int]]] = Map.empty[Int, Map[Char, List[Int]]]
    // Transition char
    var transitionChar: Char = node.value.head
    // Transition map (Char -> nextStates)
    var transitionMap: Map[Char, List[Int]] = Map.empty[Char, List[Int]]
    // Invalid nfa in case of an error or void
    val invalidNfa:Nfa[Int] = new Nfa[Int](-1, Set(-2), Map.empty[Int, Map[Char, List[Int]]])
    if(node.value.equals("void")) invalidNfa
    else {
      if(AST.checkIfAtom(node.value)) {
        // if atom is eps -> transition char from init state is eps (eps represented as \0 char)
        if(node.value.equals("eps")) transitionChar = 0.toChar
        // \0 -> (final state)
        transitionMap = Map(transitionChar -> List(initialState + 1))
        // initState with \0 -> finalState
        transitions += (initialState -> transitionMap)
        new Nfa[Int](initialState, Set(initialState + 1), transitions)
      } else if(node.value.equals("CONCAT")){
        // For concat, right nfa initState is leftNfa finalState (Thompson construction wikipedia)
        val leftNfa:Nfa[Int] = nfaFromAST(node.left, initialState)
        val rightNfa:Nfa[Int] = nfaFromAST(node.right, leftNfa.finalStates.head)
        // the new NFA hast leftNfa.initState and rightNfa.finalState with merged transitions maps of both
        new Nfa[Int](leftNfa.initState, rightNfa.finalStates, leftNfa.transitions.++(rightNfa.transitions))
      } else if(node.value.equals("UNION")) {
        // Union
        val upperNfa:Nfa[Int] = nfaFromAST(node.left, initialState + 1)
        val lowerNfa:Nfa[Int] = nfaFromAST(node.right, upperNfa.finalStates.head + 1)
        // from initial state to UpperInitState and lowerInitState with epsilon transition
        transitionMap = Map(0.toChar -> List(upperNfa.initState, lowerNfa.initState))
        transitions += (initialState -> transitionMap)
        // from upper and lower nfa add epsilon transition to final state (which is lowerNfa.finalState + 1)
        transitionMap = Map(0.toChar -> List(lowerNfa.finalStates.head + 1))
        transitions += (upperNfa.finalStates.head -> transitionMap)
        transitions += (lowerNfa.finalStates.head -> transitionMap)
        // nfa with initialState, lowerNfa.finalState + 1 and merge transitions maps of both
        new Nfa[Int](initialState, Set(lowerNfa.finalStates.head + 1), transitions.++(upperNfa.transitions).++(lowerNfa.transitions))
      } else if(node.value.equals("STAR")) {
        // Star
        val starNfa:Nfa[Int] = nfaFromAST(node.left, initialState + 1)
        // from initialState to starNfa.initState and starNfa.finalState + 1 with epsilon transition
        transitionMap = Map(0.toChar -> List(starNfa.initState, starNfa.finalStates.head + 1))
        transitions += (initialState -> transitionMap)
        // from starNfa.finalState to starNfa.initState and starNfa.finalState + 1 with epsilon transition
        transitionMap = Map(0.toChar -> List(starNfa.finalStates.head + 1, starNfa.initState))
        transitions += (starNfa.finalStates.head -> transitionMap)
        // full StarNfa with initialState, starNfa.finalState + 1 and merged transitions maps of both
        new Nfa[Int](initialState, Set(starNfa.finalStates.head + 1), transitions.++(starNfa.transitions))
      } else invalidNfa
    }
  }

  /**
   * Get NFA[Int] from prenext Str
   * @param str string from which we build NFA
   * @return NFA[Int]
   */
  def fromPrenex(str: String): Nfa[Int] = {
    val tokenized_prenex:List[String] = splitPrenex(str)
    val prenex_ast:AST = AST.getASTFromPrenex(tokenized_prenex.toBuffer)
    nfaFromAST(prenex_ast, 1)
  }
}


