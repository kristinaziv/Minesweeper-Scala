
import javafx.scene.control.TextField
import scalafx.scene.image.{Image, ImageView}
import scalafx.scene.layout.VBox
import scalafx.scene.media.AudioClip
import scalafx.scene.text.Text


class WinEffects(label:String) {

  private val winImage = new ImageView(new Image(Resources.pobedaImg))
  private val winSound = new AudioClip(getClass.getResource(Resources.pobedaSound).toString)
  private val winPane = new VBox()
  private val text=new Text("Unesite ime ukoliko zelite da sacuvamo vas rezultat:"){wrappingWidth = 180}
  winPane.getChildren().add(winImage)
  winPane.getChildren().add(new Text(label){
    wrappingWidth = 130
  })
  def removeText()={
    winPane.children.remove(2)
    winPane.children.remove(2)
  }

  winPane.getChildren().add(text)
  val tf=new TextField()
  winPane.getChildren().add(tf)
  def getTf:String=tf.getText
  def getVictory: VBox = winPane
  def showWinAnimation(): Unit = {
    winSound.play()
  }
}
