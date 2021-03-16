package state

import playingcard.PlayingCard

object StackBet { def initial(stack: Int) = new StackBet(stack, 0) }
class StackBet(val stack: Int, val bet: Int) {
  def setBet(amount: Int) = new StackBet(stack + bet - amount, amount)
  override lazy val toString: String = s"Stack: $stack, Bet: $bet"
}

object Round {
  def initial(number: Int) = new Round(number, 0, 0, "", Map(), None, Table.initial, None)
}

class Round(val number: Int, smallBlind: Int, bigBlind: Int, onButton: String, val money: Map[String, StackBet], hand: Option[(PlayingCard, PlayingCard)], val table: Table, winner: Option[String]) {

  lazy val isPreFlop: Boolean = table.flop.isEmpty
  lazy val handIsPair = solidHand._1.rank == solidHand._2.rank
  lazy val handAsSet = Set(solidHand._1, solidHand._2)
  lazy val maxBet = money.values.map(x => x.bet).max

  def amountToCallFor(botName: String) = money.values.maxBy(x => x.bet).bet - moneyFor(botName).bet
  def stackLeftFor(botName: String) = moneyFor(botName).stack

  private lazy val solidHand: (PlayingCard, PlayingCard) = hand match {
    case Some((x, y)) => (x, y)
    case None => throw new IllegalStateException("Asked for information about hand before we know!")
  }
  private def moneyFor(botName: String): StackBet = money.get(botName) match {
    case Some(stackBet: StackBet) => stackBet
    case None => throw new IllegalStateException("Asked for information about money before we know!")
  }

  // Housekeeping stuff below here
  def updateMetadata(key: String, value: String, yourBot: String): Round = {
    if(winner.nonEmpty) throw new IllegalStateException("We thought this round was already finished!")
    key match {

      // We should already have inferred these, check that we're correct
      case "round" => {
        if(value.toInt != number) throw new IllegalStateException(s"Our round -> $number != $value <- Their round")
        this
      }
      case "max_win_pot" => {
        assertMaxWinPotIs(value.toInt)
        this
      }
      case "amount_to_call" => {
        assertAmountToCallIs(value.toInt, yourBot)
        this
      }

      // The rest we can blindly update
      case "small_blind" => new Round(number, value.toInt, bigBlind, onButton, money, hand, table, winner)
      case "big_blind"   => new Round(number, smallBlind, value.toInt, onButton, money, hand, table, winner)
      case "on_button"   => new Round(number, smallBlind, bigBlind, value, money, hand, table, winner)

      // And the more grim one
      case "table" => new Round(number, smallBlind, bigBlind, onButton, money, hand, table.update(value.tail.dropRight(1).split(",")), winner)
    }
  }

  def updatePlayer(name: String, key: String, value: String, yourBot: String): Round = key match {
    case "stack" => new Round(number, smallBlind, bigBlind, onButton, money.updated(name, StackBet.initial(value.toInt)), hand, table, winner)
    case "post"  => new Round(number, smallBlind, bigBlind, onButton, money.updated(name, money.get(name).get.setBet(value.toInt)), hand, table, winner)
    case "hand" if !name.equals(yourBot) => this
    case "hand" => {
      val cards = value.tail.dropRight(1).split(",").map(str => PlayingCard.parse(str))
      assert(cards.length == 2)
      new Round(number, smallBlind, bigBlind, onButton, money, Some((cards(0), cards(1))), table, winner)
    }
    case "fold" | "check" => this
    case "call" => new Round(number, smallBlind, bigBlind, onButton, money.updated(name, money.get(name).get.setBet(maxBet)), hand, table, winner)
    case "raise" => new Round(number, smallBlind, bigBlind, onButton, money.updated(name, money.get(name).get.setBet(maxBet + value.toInt)), hand, table, winner)
    case "wins" => new Round(number, smallBlind, bigBlind, onButton, money, hand, table, Some(name))
  }

  private def assertMaxWinPotIs(expected: Int) = {
    val actual = money.values.map(x => x.bet).sum
    if(actual != expected) throw new IllegalStateException(s"Our maxWinPot -> $actual != $expected <- Their maxWinPot")
  }

  private def assertAmountToCallIs(expected: Int, yourBot: String) = {
    val actual = amountToCallFor(yourBot)
    if(actual != expected) throw new IllegalStateException(s"Our amountToCall -> $actual != $expected <- Their amountToCall")
  }

  private lazy val toMap: Map[String, Any] = Map(
    "number"      -> number,
    "small_blind" -> smallBlind,
    "big_blind"   -> bigBlind,
    "on_button"   -> onButton,
    "money"       -> money,
    "hand"        -> hand,
    "table"       -> table,
    "winner"      -> winner
  )

  override lazy val toString: String =
    s"""Round:
      |    Number: $number
      |    Small Blind: $smallBlind
      |    Big Blind: $bigBlind
      |    On Button: $onButton
      |    Money: $money
      |    Hand: $hand
      |    Table: $table
      |    Winner: winner
    """.stripMargin

}
