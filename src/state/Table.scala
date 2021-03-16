package state

import playingcard.PlayingCard

object Table {
  val initial = new Table(None, None, None)
}

class Table(val flop: Option[(PlayingCard, PlayingCard, PlayingCard)], turn: Option[PlayingCard], river: Option[PlayingCard]) {
  def update(str: Array[String]): Table = {
    val cards = str.map(st => PlayingCard.parse(st))
    cards.length match {
      case 0 => Table.initial
      case n if n >= 3 && n <= 5 => {
        val flop = Some((cards(0), cards(1), cards(2)))
        cards.length match {
          case 3 => new Table(flop, None, None)
          case 4 => new Table(flop, Some(cards(3)), None)
          case 5 => new Table(flop, Some(cards(3)), Some(cards(4)))
        }
      }
    }
  }

  lazy val allCards: Set[PlayingCard] = {
    flop match {
      case Some(flp) => {
        val flopSet = Set(flp._1, flp._2, flp._3)
        turn match {
          case Some(trn) => {
            val turnSet = flopSet + trn
            river match {
              case Some(rver) => turnSet + rver
              case None => turnSet
            }
          }
          case None => flopSet
        }
      }
      case None => Set()
    }
  }

  private lazy val toMap: Map[String, Any] = Map(
    "flop"  -> flop,
    "turn"  -> turn,
    "river" -> river
  )

  override lazy val toString: String = toMap.toString()
}