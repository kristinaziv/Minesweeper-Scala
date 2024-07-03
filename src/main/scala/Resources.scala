sealed trait Difficulty
case object Pocetnik extends Difficulty
case object Srednji extends Difficulty
case object Ekspert extends Difficulty


object Resources{

  val Begginer_Size=(9,9)
  val Medium_Size = (16, 16)
  val Expert_Size = (30, 16)
  val Begginer_Bombs = (10, 15)
  val Medium_Bombs = (40, 45)
  val Expert_Bombs = (90, 100)

  val BLANK_SPACE='-'
  val MINE='#'

  val flag: String = "/images/flag.png"
  val bomb: String = "/images/bomb.png"
  val newGame: String = "/images/nwGame.png"
  val square: String = "/images/square.png"
  val pobedaImg: String="/images/winwin.gif"
  val pobedaSound: String="/images/victory_sound.mp3"
  val izgubiliGif: String="/images/izgubiligif.gif"
  val mapDir:String="src/main/resources/nivoi"
  val savedlvls: String = "src/main/resources/savedLvls"
  val rezultati: String = "src/main/resources/nivoi/rezultati.txt"




  val styleWithELevation="-fx-effect: dropshadow(gaussian, rgba(0, 0, 0, 0.75), 10, 0, 0, 4);"
  val plainBtnStyle="-fx-font-size: 16px;"
  val styleOpenField="-fx-font-size: 20px; -fx-background-color: lightblue; -fx-font-weight:bold; ";
  val randomBtnStyle=s"-fx-font-size: 14px; -fx-background-color: gray;"
  val loadBtnStyle = s"-fx-font-size: 16px; -fx-background-color: cyan;"
  val rezultatiBtnStyle="-fx-background-color: red; -fx-font-size: 16px;"
  val helpBtnStyle = "-fx-background-color: yellow;"
  val help = "-fx-background-color: yellow;"
}
