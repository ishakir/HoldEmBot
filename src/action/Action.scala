package action

import playingcard.{Hand, HighCard, PokerAnalysis, HandScore}
import state.MatchState

object Action {
  def chooseActions(state: MatchState): Seq[String] = {
    Analyser(state).actions
  }
}

object Analyser {
  def apply(state: MatchState): Analyser = state.currentRound.isPreFlop match {
    case true => new PreFlopAnalyser(state)
    case false => new PostFlopAnalyser(state)
  }
}

abstract class Analyser(val state: MatchState) {
  lazy val actions: Seq[String] = {
    System.err.println(probabilities)

    val betAggressively = probabilities.filter(pair => pair._1.awesomeness > bestOfTable.awesomeness).values.sum  > 0.7

    val round = state.currentRound
    val myBotName = state.settings.yourBot
    val amountToCall = round.amountToCallFor(myBotName)
    val currentPot = state.currentRound.money.values.map(x => x.bet).sum

    if(betAggressively) {
      System.err.println("Chose to bet aggresively")
      val myRemainingStack = round.stackLeftFor(myBotName)

      // Attempt to bet 1/5 of remaining stack
      if(amountToCall > myRemainingStack / 2) Seq("fold 0")
      else if(amountToCall > currentPot) Seq(s"call $amountToCall")
      else if(amountToCall == 0) Seq(s"raise $currentPot")
      else Seq(s"call $amountToCall")
    } else {
      System.err.println("Chose to bet non-aggressively")

      // Do we need / want to call anything?
      if(amountToCall > currentPot) Seq("fold 0")
      else if(amountToCall > 0) {
        if(state.currentRound.table.allCards.size == 5) Seq("fold 0")
        else Seq(s"call $amountToCall")
      }
      else Seq("check 0")
    }


  }

  val probabilities: Map[HandScore, Double]
  val bestOfTable: HandScore
}

class PostFlopAnalyser(state: MatchState) extends Analyser(state) {
  override val probabilities: Map[HandScore, Double] = new PokerAnalysis(state.currentRound.handAsSet, state.currentRound.table.allCards, 5).scoreToProbability
  override val bestOfTable = new Hand(state.currentRound.table.allCards).highestScore
}

class PreFlopAnalyser(state: MatchState) extends Analyser(state) {
  override val probabilities: Map[HandScore, Double] = if(state.currentRound.handIsPair) Map(HighCard -> 0) else Map(HighCard -> 1)
  override val bestOfTable = HighCard
}