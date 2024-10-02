/*
 * Copyright (c) 2012, 2013, 2014, 2015, 2016 SURFnet BV
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without modification, are permitted provided that the
 * following conditions are met:
 *
 *   * Redistributions of source code must retain the above copyright notice, this list of conditions and the following
 *     disclaimer.
 *   * Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following
 *     disclaimer in the documentation and/or other materials provided with the distribution.
 *   * Neither the name of the SURFnet BV nor the names of its contributors may be used to endorse or promote products
 *     derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
 * INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
package nl.surfnet.nsiv2.utils

import play.api.Logger

/** Simplified re-implementation of Akka's finite state machine [[akka.actor.FSM]] DSL, without the
  * dependencies on actors.
  */
abstract class FiniteStateMachine[S, D, I, O](initialStateName: S, initialStateData: D)
    extends StateMachine[I, O] {
  private val logger = Logger(classOf[FiniteStateMachine[?, ?, ?, ?]])

  /** Process the given message. Returns `None` if the FSM did not handle the message. Otherwise the
    * replies (if any) are returned.
    */
  def process(message: I): Option[Seq[O]] = {
    val nextState = _handlers(stateName).orElse(_unhandled).lift.apply(Event(message, _stateData))
    nextState map { nextState =>
      _nextStateName = nextState.name
      _nextStateData = nextState.data
      val output = if _transitionHandler.isDefinedAt((_stateName, _nextStateName)) then {
        logger.debug(
          s"state change from ${_stateName} to ${_nextStateName} with defined transition handler"
        )
        _transitionHandler((_stateName, _nextStateName))
      } else {
        Vector.empty
      }
      _stateName = _nextStateName
      _stateData = _nextStateData
      output
    }
  }

  override def toString: String =
    s"${super.toString}(stateName = $stateName, stateData = $stateData)"

  /** This captures all of the managed state of the state machine: the state name, the state data,
    * and replies accumulated while processing the last message.
    */
  protected[this] case class State(name: S, data: D) {
    def using(nextData: D): State = copy(data = nextData)
  }
  protected[this] case class Event(message: I, data: D)

  protected[this] def stateName = _stateName
  protected[this] def stateData = _stateData

  protected[this] def nextStateName = _nextStateName
  protected[this] def nextStateData = _nextStateData

  protected[this] type EventHandler = PartialFunction[Event, State]
  protected[this] type TransitionHandler = PartialFunction[(S, S), Seq[O]]

  protected[this] def when(stateName: S, otherStateNames: S*)(handler: EventHandler): Unit = {
    val states = stateName :: otherStateNames.toList
    states.foreach { state =>
      require(!_handlers.contains(state), s"handler for state $state is already defined")
    }
    _handlers ++= states.map { _ -> handler }
  }
  protected[this] def whenUnhandled(handler: EventHandler): Unit = {
    _unhandled = handler
  }
  protected[this] def onTransition(handler: TransitionHandler): Unit = {
    _transitionHandler = handler
  }

  protected[this] def goto(stateName: S): State = {
    require(_handlers contains stateName, s"cannot goto $stateName: state does not exist")
    State(stateName, stateData)
  }
  protected[this] def stay: State = goto(stateName)

  /** This extractor is just convenience for matching a (S, S) pair, including a reminder what the
    * new state is.
    */
  protected[this] object -> {
    def unapply(in: (S, S)) = Some(in)
  }

  private var _stateName: S = initialStateName
  private var _stateData: D = initialStateData
  private var _nextStateName: S = stateName
  private var _nextStateData: D = stateData

  private var _handlers: Map[S, EventHandler] = Map.empty
  private var _unhandled: EventHandler = PartialFunction.empty
  private var _transitionHandler: TransitionHandler = { case _ => Vector.empty }
}
