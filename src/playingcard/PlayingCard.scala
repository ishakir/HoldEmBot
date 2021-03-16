package playingcard

import scala.collection.immutable.ListSet
import scala.util.Random

object Suit {
  sealed abstract class Suit(val char: Char)

  case object Clubs extends Suit('c')
  case object Diamonds extends Suit('d')
  case object Hearts extends Suit('h')
  case object Spades extends Suit('s')

  val values: Set[Suit] = ListSet(Clubs, Diamonds, Hearts, Spades)

  def parse(char: Char) = values.find(s => s.char == char).getOrElse(throw new IllegalArgumentException(s"No Suit parseable from char $char"))
}

object Rank {
  sealed abstract class Rank(val char: Char, val values: Set[Int]) extends Ordered[Rank] {
    // The only way to order at the moment is ace-high, this can be changed
    override def compare(that: Rank) = {
      if(values.max > that.values.max) 1
      else if(values.max < that.values.max) -1
      else 0
    }
  }

  case object Ace extends Rank('A', Set(1, 14))
  case object Two extends Rank('2', Set(2))
  case object Three extends Rank('3', Set(3))
  case object Four extends Rank('4', Set(4))
  case object Five extends Rank('5', Set(5))
  case object Six extends Rank('6', Set(6))
  case object Seven extends Rank('7', Set(7))
  case object Eight extends Rank('8', Set(8))
  case object Nine extends Rank('9', Set(9))
  case object Ten extends Rank('T', Set(10))
  case object Jack extends Rank('J', Set(11))
  case object Queen extends Rank('Q', Set(12))
  case object King extends Rank('K', Set(13))

  val values: Set[Rank] = ListSet(Ace, Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King)

  def parse(char: Char) = values.find(r => r.char == char).getOrElse(throw new IllegalArgumentException(s"No Rank parseable from char $char"))

}

object PlayingCard {
  def parse(str: String): PlayingCard = {
    assert(str.length == 2, "Parseable cards are of the form \"RS\" where R is rank, S is suit. Not '"+str+"'")
    val rankChar = str.charAt(0)
    val suitChar = str.charAt(1)
    new PlayingCard(Rank.parse(rankChar), Suit.parse(suitChar))
  }
}

class PlayingCard(val rank: Rank.Rank, val suit: Suit.Suit) extends Ordered[PlayingCard] {
  override def compare(that: PlayingCard): Int = rank.compare(that.rank)
  override lazy val toString = rank.char.toString + suit.char.toString()

  override def equals(that: Any): Boolean = that match {
    case _that: PlayingCard => this.rank == _that.rank && this.suit == _that.suit
    case _ => false
  }

  override lazy val hashCode: Int = 31 * rank.hashCode() + suit.hashCode()
}

object Deck {
  lazy val orderedDeck: Seq[PlayingCard] = for(suit <- Suit.values.toList.reverse; rank <- Rank.values.toList.reverse) yield new PlayingCard(rank, suit)
  def newShuffledDeck(): Seq[PlayingCard] = Random.shuffle(orderedDeck)
}