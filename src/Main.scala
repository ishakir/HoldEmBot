import action.Action
import state.MatchState

import scala.annotation.tailrec

/**
 * Created by imran on 05/04/15.
 */
object Main extends App {
  override def main (args: Array[String]) {
    recurse(io.Source.stdin.getLines(), MatchState.initial)
  }

  @tailrec
  def recurse(inputs: Iterator[String], state: MatchState): Unit = {
    if(!inputs.hasNext) return
    else {
      val line: String = inputs.next()
      System.err.println(s"Received '$line'")

      val (newState: MatchState, responses: Seq[String]) = processInput(state, line)

      // Log out the current state
      System.err.println(newState)

      // Print out actions to stdout
      for(response <- responses) println(response)

      // Aaaand continue
      recurse(inputs, newState)
    }
  }

  def processInput(state: MatchState, line: String): (MatchState, Seq[String]) = {
    try {
      val args = line.split(" ")
      args(0) match {
        case "Settings" => (state.updateSetting(args(1), args(2)), Seq())
        case "Match"    => (state.updateRound(args(1), args(2)), Seq())
        case "Action"   => (state, Action.chooseActions(state))
        case _          => (state.updatePlayer(args(0), args(1), args(2)), Seq())
      }
    } catch {
      case e: Exception => {
        System.err.println("Caught exception: ")
        e.printStackTrace(System.err)
        (state, Seq())
      }
    }
  }

}
