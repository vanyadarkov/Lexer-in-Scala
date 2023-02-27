import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class Dfa[A] (var initStates:Set[A], var finalStates:Set[A], var transitions:Map[A, Map[Char, A]], var dfaStatesAsNfaStates:Map[A, Set[A]]){

  /**
   * Map current DFA of A to DFA of B
   * @param f function to map A to B
   * @tparam B new type
   * @return new DFA[B]
   */
  def map[B](f: A => B) : Dfa[B] = {
    var newTransitions: Map[B, Map[Char, B]] = Map.empty[B, Map[Char, B]]
    newTransitions = transitions map { case (state, adjacency) => f(state) -> (adjacency map {
      case (nextChar, nextState) => nextChar -> f(nextState)
    })}
    val newDfaStatesAsNfaStates:Map[B, Set[B]] = dfaStatesAsNfaStates map {case (state, group) => f(state) -> group.map(f)}
    new Dfa[B](initStates.map(f), finalStates.map(f), newTransitions, newDfaStatesAsNfaStates)
  }

  /**
   * Check if state is sink state
   * @param state state to check
   * @return true if sink false otherwise
   */
  def checkIfSink(state: A): Boolean = {
    if(transitions.contains(state)) {
      val fromState = transitions(state)
      for((_ -> nextState) <- fromState) {
        if(nextState != state) return false
      }
      true
    } else false
  }

  /**
   * Get the next state from state with c char transition
   * @param state state
   * @param c transition char
   * @return next state
   */
  def next(state:A, c: Char): A = {
    if(transitions(state).contains(c)) {
      transitions(state)(c)
    } else {
      val states = getStates
      for(state <- states) {
        if(checkIfSink(state)) return state
      }
      initStates.head
    }
  }

  /**
   * Check if dfa accepts str
   * @param str string to check
   * @return true if accepted, false otherwise
   */
  def accepts(str: String): Boolean = {
    // return value
    var ret:Boolean = false
    // noError flag. If an error occured (char from string not in alphabet or no transitions for a state) then
    // this flag turn to false and str will not be accepted
    var noError:Boolean = true
    for(initState <- initStates) {
      var currentState = initState
      for(transitionChar <- str) {
        if(transitions.contains(currentState)) {
          if(transitions(currentState).contains(transitionChar)){
            currentState = transitions(currentState)(transitionChar)
          } else {
            noError = false
          }
        } else {
          noError = false
        }
      }
      ret |= finalStates.contains(currentState)
    }
    ret & noError
  }

  /**
   * Get all states of this DFA
   * @return set of DFA's states
   */
  def getStates : Set[A] = {
    var states = Set.empty[A]
    for(state <- initStates) {
      states += state
    }
    for(state <- finalStates) {
      states += state
    }
    for((state, adjacency) <- transitions) {
      states += state
      for((_, nextState) <- adjacency) {
        states += nextState
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
    finalStates.contains(state)
  }
}

object Dfa {

  /**
   * Convert a NFA of int to DFA of int
   * @param nfa NFA
   * @return DFA[Int]
   */
  def fromNfa(nfa:Nfa[Int]) : Dfa[Int] = {
    // queue of grouping of states which need to be checked for adding to DFA
    val groupStatesQueue:mutable.Queue[Set[Int]] = new mutable.Queue[Set[Int]]()
    // sinkState will be represented as group of (-1) Set(-1)
    val sinkState = Set(-1)
    // dfaStates which will be represented as grouping of states from NFA
    val dfaGroupsState:ListBuffer[Set[Int]] = new ListBuffer[Set[Int]]
    // enqueue nfa initial state epsilon closures to be checked
    groupStatesQueue.enqueue(nfa.epsilonClosuresForState(nfa.initState, ListBuffer.empty))
    // get NFA alphabet
    val alphabet = nfa.getAlphabet
    // transitions for converted DFA represented as grouping of states from NFA
    var dfaTransitionsInGroups = Map.empty[Set[Int], Map[Char, Set[Int]]]
    // While queue is not empty (this means we have something to check and add to DFA)
    while(groupStatesQueue.nonEmpty) {
      // dequeue the group
      val currentGroupOfStates:Set[Int] = groupStatesQueue.dequeue()
      // add it to DFA
      dfaGroupsState += currentGroupOfStates
      // for every char in NFA's alphabet
      for(transitionChar <- alphabet) {
        // for every state of current group get next states for transitionChar
        val partialStates = new ListBuffer[Int]
        for(state <- currentGroupOfStates) {
          for(nextState <- nfa.next(state, transitionChar)) {
            partialStates += nextState
          }
        }
        var newGroup = Set.empty[Int]
        // If no next states for this group
        if(partialStates.isEmpty) {
          // the newGroup will be sinkState
          newGroup = sinkState
        }
        else {
          // if we have nextStates then add to potentialNewGroup all epsilon closures for every state
          val potentialNewGroup = new ListBuffer[Int]
          for (state <- partialStates) {
            for (epsilonClosedState <- nfa.epsilonClosuresForState(state, ListBuffer.empty)) {
              potentialNewGroup += epsilonClosedState
            }
          }
          newGroup = potentialNewGroup.toList.sorted.toSet
        }
        if (newGroup.nonEmpty) {
          // check if current group is present in transitions Map
          // here also, a transition from stateA to stateA will be added
          if (dfaTransitionsInGroups.contains(currentGroupOfStates)) {
            // then check if this group has no added transitions for current transition char
            if (!dfaTransitionsInGroups(currentGroupOfStates).contains(transitionChar)) {
              // update transitions for current group with transition char to newGroup
              val newMap: Map[Char, Set[Int]] = dfaTransitionsInGroups(currentGroupOfStates).+(transitionChar -> newGroup)
              dfaTransitionsInGroups = dfaTransitionsInGroups + (currentGroupOfStates -> newMap)
            }
          } else {
            // if current group not present in transitions map -> add a new entry
            dfaTransitionsInGroups += (currentGroupOfStates -> Map(transitionChar -> newGroup))
          }
          // if newGroup is not present in DFA states and newGroup isnt enqueued then enqueue it
          if(!dfaGroupsState.contains(newGroup)) {
            if (!groupStatesQueue.contains(newGroup))
              groupStatesQueue.enqueue(newGroup)
          }
        }
      }
    }
    // Make a conversion from state represented as set of states to state represented as int
    // index of group in dfaGroupsState + 1 will be the index of state in DFA[Int]
    val initStates = new ListBuffer[Int]
    val finalStates = new ListBuffer[Int]
    var dfaTransitions = Map.empty[Int, Map[Char, Int]]
    for(group <- dfaGroupsState) {
      if(group.contains(nfa.initState)) {
        initStates += dfaGroupsState.indexOf(group) + 1
      }
      var doFor:Boolean = true
      for(state <- group) {
        if(nfa.finalStates.contains(state) && doFor) {
          finalStates += dfaGroupsState.indexOf(group) + 1
          doFor = false
        }
      }
//      if(group.contains(nfa.finalState)) {
//        finalStates += dfaGroupsState.indexOf(group) + 1
//      }
    }
    var dfaStatesAsNfaStates:Map[Int, Set[Int]] = Map.empty
    for((groupState, adjacency) <- dfaTransitionsInGroups) {
      val groupId:Int = dfaGroupsState.indexOf(groupState) + 1
      dfaStatesAsNfaStates += groupId -> groupState
      for((transitionChar, nextGroup) <- adjacency) {
        if(dfaTransitions.contains(groupId)) {
          if(!dfaTransitions(groupId).contains(transitionChar)) {
            val newMap:Map[Char, Int] = dfaTransitions(groupId).+(transitionChar -> (dfaGroupsState.indexOf(nextGroup) + 1))
            dfaTransitions = dfaTransitions + (groupId -> newMap)
          }
        } else {
          dfaTransitions += groupId -> Map(transitionChar -> (dfaGroupsState.indexOf(nextGroup) + 1))
        }
      }
    }
    new Dfa[Int](initStates.toSet, finalStates.toSet, dfaTransitions, dfaStatesAsNfaStates)
  }

  def fromPrenex(str: String): Dfa[Int] = {
    val nfa:Nfa[Int] = Nfa.fromPrenex(str)
    val dfa = fromNfa(nfa)
    dfa
  }
}
