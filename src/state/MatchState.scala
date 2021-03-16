package state

/**
 * Created by imran on 05/04/15.
 */
class MatchState(val settings: Settings, val currentRound: Round, pastRounds: Seq[Round]) {
  def updateSetting(key: String, value: String): MatchState = new MatchState(settings.update(key, value), currentRound, pastRounds)
  def updateRound(key: String, value: String): MatchState = new MatchState(settings, currentRound.updateMetadata(key, value, settings.yourBot), pastRounds)
  def updatePlayer(name: String, key: String, value: String): MatchState = key match {
    case "wins" => new MatchState(settings, Round.initial(currentRound.number + 1), currentRound.updatePlayer(name, key, value, settings.yourBot) +: pastRounds)
    case _ => new MatchState(settings, currentRound.updatePlayer(name, key, value, settings.yourBot), pastRounds)
  }

  override lazy val toString: String =
    s"""MatchState:
       |  $settings
       |  $currentRound
     """.stripMargin
}

object MatchState {
  val initial = new MatchState(
    Settings.initial,
    Round.initial(1),
    Seq()
  )
}