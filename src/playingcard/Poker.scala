package playingcard

import playingcard.Rank.Rank


class PokerAnalysis(handCards: Set[PlayingCard], tableCards: Set[PlayingCard], totalTableCards: Int) {
  assert(handCards.size + tableCards.size == (handCards ++ tableCards).size)  // Sets are disjoint
  lazy val remainingInDeck: Set[PlayingCard] = (Deck.orderedDeck.toSet -- handCards) -- tableCards

  lazy val possibleTableCards = remainingInDeck.subsets(totalTableCards - tableCards.size).map(set => set ++ tableCards).toList

  def scoreToProbability = {
    val best = possibleTableCards.map(s => {
      val fluff = handCards.size + s.size - totalTableCards
      val scores = for(
        i <- 0 to fluff;
        handContribution <- handCards.subsets(i).toList;
        tableContribution <- s.subsets(s.size - i)
      ) yield new Hand(handContribution ++ tableContribution).highestScore
      scores.maxBy(s => s.awesomeness)
    })
    best.groupBy(x => x).mapValues(l => l.length.toDouble / best.length)
  }
}

abstract class HandScore(val awesomeness: Int)
case object RoyalFlush extends HandScore(9)
case object StraightFlush extends HandScore(8)
case object FourOfAKind extends HandScore(7)
case object FullHouse extends HandScore(6)
case object Flush extends HandScore(5)
case object Straight extends HandScore(4)
case object Triple extends HandScore(3)
case object TwoPair extends HandScore(2)
case object Pair extends HandScore(1)
case object HighCard extends HandScore(0)

// Most stuff in this class should be Set rather than Seq, but this caused some issues
// Need to sort that out at some point
class Hand(val cards: Set[PlayingCard]) {
  assert(cards.size <= 5, s"A poker hand must have 5 cards or less, $cards does not")

  lazy val isRoyalFlush = isStraightFlush &&
    cards.exists(card => card.rank == Rank.Ace) &&
    cards.exists(card => card.rank == Rank.King)

  lazy val isStraightFlush = isFlush && isStraight

  lazy val isFlush: Boolean = cards.filter(card => card.suit != cards.head.suit).size == 0 && cards.size == 5

  lazy val isStraight: Boolean = chooseOne(cards.toSeq.map(card => card.rank.values)).exists( values => {
    val sortedValues = values.toList.sorted
    sortedValues.foldLeft((sortedValues.head, true)) {
      case ((expected, isStraightSoFar), actual) => (expected + 1, isStraightSoFar && expected == actual)
    }}._2 && cards.size == 5
  )

  lazy val hasPair: Boolean = hasNofAKind(2)
  lazy val hasTriple: Boolean = hasNofAKind(3)
  lazy val isFourOfAKind: Boolean = hasNofAKind(4)

  lazy val isFullHouse: Boolean = hasPair && hasTriple

  lazy val isTwoPair = groupedByRank.values.filter(s => s.size == 2).size == 2

  private lazy val mostCommonRank = groupedByRank.maxBy(p => p._2.size)._1
  private lazy val highestRank = cards.maxBy(c => c.rank).rank

  lazy val highestScore: HandScore = {
    if(isRoyalFlush) RoyalFlush
    else if(isStraightFlush) StraightFlush
    else if(isFourOfAKind) FourOfAKind
    else if(isFullHouse) FullHouse
    else if(isFlush) Flush
    else if(isStraight) Straight
    else if(hasTriple) Triple
    else if(isTwoPair) TwoPair
    else if(hasPair) Pair
    else HighCard
  }

  private def hasNofAKind(n: Int): Boolean = groupedByRank.values.exists(set => set.size == n)
  private lazy val groupedByRank: Map[Rank, Set[PlayingCard]] = cards.groupBy(card => card.rank)

  // This could be much nicer and is probably in the wrong place
  // But will do for now, required in order to allow ace low
  // and ace high straights
  def chooseOne[T](sequences: Seq[Set[T]]) = cards.size match {
    case 3 => chooseOne3(sequences)
    case 4 => chooseOne4(sequences)
    case 5 => chooseOne5(sequences)
  }

  def chooseOne3[T](sequences: Seq[Set[T]]): Set[Seq[T]] = {
    assert(sequences.size == 3)
    for(
      x1 <- sequences.head;
      x2 <- sequences.tail.head;
      x3 <- sequences.tail.tail.head
    ) yield Seq(x1, x2, x3)
  }

  def chooseOne4[T](sequences: Seq[Set[T]]): Set[Seq[T]] = {
    assert(sequences.size == 4)
    for(
      x1 <- sequences.head;
      x2 <- sequences.tail.head;
      x3 <- sequences.tail.tail.head;
      x4 <- sequences.tail.tail.tail.head
    ) yield Seq(x1, x2, x3, x4)
  }

  def chooseOne5[T](sequences: Seq[Set[T]]): Set[Seq[T]] = {
    assert(sequences.size == 5)
    for(
      x1 <- sequences.head;
      x2 <- sequences.tail.head;
      x3 <- sequences.tail.tail.head;
      x4 <- sequences.tail.tail.tail.head;
      x5 <- sequences.tail.tail.tail.tail.head
    ) yield Seq(x1, x2, x3, x4, x5)
  }
}