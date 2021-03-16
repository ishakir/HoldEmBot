package state

object Settings {
  val initial = new Settings(0, 0, 0, 0, "")
}

class Settings(val timeBank: Int, timePerMove: Int, handsPerLevel: Int, startingStack: Int, val yourBot: String) {
  def update(key: String, value: String): Settings = key match {
    // I've seen it give 'timebank' but the documentation say 'time_bank'
    case "time_bank" | "timebank" => new Settings(value.toInt, timePerMove, handsPerLevel, startingStack, yourBot)
    case "time_per_move"          => new Settings(timeBank, value.toInt, handsPerLevel, startingStack, yourBot)
    case "hands_per_level"        => new Settings(timeBank, timePerMove, value.toInt, startingStack, yourBot)
    case "starting_stack"         => new Settings(timeBank, timePerMove, handsPerLevel, value.toInt, yourBot)
    case "your_bot"               => new Settings(timeBank, timePerMove, handsPerLevel, startingStack, value)
  }

  private lazy val toMap: Map[String, String] = Map(
    "time_bank"       -> timeBank.toString,
    "time_per_move"   -> timePerMove.toString,
    "hands_per_level" -> handsPerLevel.toString,
    "starting_stack"  -> startingStack.toString,
    "your_bot"        -> yourBot
  )

  override val toString: String =
    s"""Settings:
      |    Time Bank: $timeBank
      |    Time Per Move: $timePerMove
      |    Hands Per Level: $handsPerLevel
      |    Starting Stack: $startingStack
      |    Your Bot: $yourBot
    """.stripMargin
}
