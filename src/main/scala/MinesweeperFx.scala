import javafx.scene.Node
import javafx.scene.input.MouseButton
import scalafx.Includes.observableList2ObservableBuffer
import scalafx.animation.AnimationTimer
import scalafx.application.JFXApp3
import scalafx.geometry.{Insets, Pos}
import scalafx.scene.Scene
import scalafx.scene.control.Alert.AlertType
import scalafx.scene.control._
import scalafx.scene.image.{Image, ImageView}
import scalafx.scene.layout._
import scalafx.scene.paint.Color
import scalafx.scene.text.{Font, FontWeight, Text}
import scalafx.stage.{FileChooser, Stage}

import java.io.{File, PrintWriter}
import java.nio.charset.StandardCharsets
import java.nio.file._
import java.util.stream.Collectors
import scala.io.Source
import scala.jdk.CollectionConverters.CollectionHasAsScala
import scala.util.Random




object MinesweeperFx extends JFXApp3 {
  private var gameButtons: Seq[ButtonWithInfo] = Seq()
  private var timeLabel:Label = _
  private var currentLvl:Seq[String]=Seq()
  private var startTime: Long = _
  private var elapsedTime: Long = _
  private var timerRunning = false
  private var bombsOnCurrLvl:Int=0
  private var helpNum:Int=0
  private var clickCnt: Int = 0
  private var lostGame:Boolean=false
  private var helpBtn:Button=_
  private var sekv:Boolean=false

  class ButtonWithInfo(var info: Int, char: Char) extends Button {
    val num = info
    val character = char
    var opened = false
    var flagged = false
    var helped =false
    def getNum: Int = num

    minWidth = 43
    minHeight = 42
    style = Resources.styleWithELevation
    alignmentInParent = Pos.Center
  }

  def startTimer(): Unit = {
    startTime = System.nanoTime()
    timerRunning = true
  }

  def startTimer(saved:Long): Unit = {
    startTime = System.nanoTime()-saved
    timerRunning = true
  }
  def stopTimer(): Unit = {
    timerRunning = false
  }

  def updateTimerLabel(): Unit = {
    val elapsedTimeSec = (System.nanoTime() - startTime) / 1e9
    val minutes = elapsedTimeSec.toInt / 60
    val seconds = elapsedTimeSec.toInt % 60
    timeLabel.text = f"Vreme: $minutes%02d:$seconds%02d"
  }

  def readFile(path: String): Seq[String] =
    Source.fromFile(path).getLines.toSeq

  def loadSaved():Seq[Button]={

    val directoryPath = Paths.get(Resources.savedlvls)
    val ls = Files.list(directoryPath)
    try {
      ls.collect(Collectors.toList()).asScala.map {(f)=>
          new Button(f.getFileName.toString) {
            style = Resources.helpBtnStyle
            onAction = _ => {
              switchToGameScene(f.getFileName.toString, null)
            }
          }
      }.toSeq
    } finally {
      ls.close()
    }
  }
  def loadLevels(difficulty: Difficulty): Seq[Button] = {
    val (difficultyTag, colorBtn) = difficulty match {
      case Pocetnik => ("p-","#ADFF2F")
      case Ekspert => ("e-","#FA8072")
      case Srednji => ("s-","khaki")
      case null => return Seq()
    }


    val directoryPath = Paths.get(Resources.mapDir)
    val ls = Files.list(directoryPath)
    try {
      ls.collect(Collectors.toList()).asScala.collect {
        case f if f.getFileName.toString.contains(difficultyTag) =>
          new Button(f.getFileName.toString.substring(2, f.getFileName.toString.length - 4)) {
            style = s"-fx-font-size: 14px; -fx-background-color: $colorBtn;"
            onAction = _ => {
              switchToGameScene(f.getFileName.toString,difficulty)

            }
          }
      }.toSeq
    } finally {
      ls.close()
    }
  }
  def parseTimeFromString(timeString: String): (Int, Int) = {
    val Array(minutesStr, secondsStr) = timeString.split(":")
    val minutes = minutesStr.toInt
    val seconds = secondsStr.toInt
    (minutes, seconds)
  }


  def switchToGameScene(levelName: String,difficulty: Difficulty): Unit = {

    val level=difficulty match {
      case null => readFile(Resources.savedlvls+"/"+levelName)
      case _ => readFile(Resources.mapDir+"/"+levelName)
    }
    currentLvl= difficulty match {
      case null => level.dropRight(3).map(e => e.replace("F", "").replace("O", "").replace(" ",""))
      case _ => level
    }

    bombsOnCurrLvl=level.mkString.count(_.equals('#'))

    flagCnt.fill = Color.White
    flagCnt.text = difficulty match {
      case null =>(bombsOnCurrLvl-level.mkString.count(_=='F')).toString
      case _ =>bombsOnCurrLvl.toString
    }
  val temp =level.find((p)=>p.contains("vreme: "))
    if(temp==None) {
       startTimer()
      val timer = AnimationTimer { _ =>
        if (timerRunning) {
          updateTimerLabel()
        }
      }
      timer.start()
    }else {
      val (savedMinutes, savedSeconds) = parseTimeFromString(temp.get.substring(7))
      startTimer((savedMinutes*60+savedSeconds)*1e9.toLong)
      val timer = AnimationTimer { _ =>
        if (timerRunning) {
          updateTimerLabel()
        }
      }
      timer.start()
    }
    val clicks=difficulty match {
      case null =>level.find((p)=>p.contains("click: ")).get.substring(7).toInt
      case _ =>0
    }
    val difficultyforLoading= difficulty match {
      case null => level.find((p) => p.contains("tez: ")).get.substring(5) match {
        case "Pocetnik" => Pocetnik
        case "Srednji" => Srednji
        case "Ekspert" => Ekspert
      }
      case _ => Pocetnik
    }

    gameButtons = fromLevelToGridPane(level, difficulty match {
      case null => difficultyforLoading
      case _ => difficulty
    }, clicks)

    gridPane.children = Seq()
    gridPane.children ++= gameButtons.map { (btn) =>
      btn: Node
    }
   helpBtn= new Button("Pomoc") {
      style = Resources.helpBtnStyle
      onAction = (e) => {
        helpNum = helpNum + 1
        val b: ButtonWithInfo = gameButtons.findLast((p) => {
          !p.opened && p.character != "#"
        }).get
        b.helped = true
        b.setStyle(Resources.help)
      }
    }
    loadSekvBtn= new Button("Ucitaj sekvencu") {
      style = Resources.loadBtnStyle
      onAction= (r) => loadSekvencu()
    }


    val gameScene = new Scene {
      fill = Color.rgb(124, 167, 235)
      content = new BorderPane {
        padding = Insets(20)
        top = new HBox {
          spacing = 100
          alignment = Pos.Center
          padding = Insets(10)
          children = Seq(
            new HBox(
              flagCnt,
              new ImageView(new Image(Resources.flag))
            ){
              alignment = Pos.Center
              spacing=10
            },
           timeLabel,
            helpBtn
          )
        }
        center = new VBox {
          alignment = Pos.Center
          spacing = 20
          children = Seq(
            new Text {
              text = s"Nivo ${difficulty match {
                case null =>difficultyforLoading.toString.toLowerCase()
                case _=>difficulty.toString().toLowerCase()
              }}:"
              font = Font.font("Arial", FontWeight.Bold, 24)
              fill = Color.White
            },
            new HBox {
              alignment = Pos.Center
              spacing = 10
              children = Seq(
                gridPane
              )
            },new HBox(
            new Button("Povratak u Meni") {
              style = Resources.plainBtnStyle
              onAction = _ => {
                if(!wonGame() && !lostGame && !sekv){
                  val textField = new TextField() {
                    promptText = "Unesite ime fajla"
                  }
                new Alert(AlertType.Information){
                  initOwner(stage)
                  title="Da li zelite da sacuvate igru?"
                  contentText = "Pritisnite OK ako zelite da sacuvate igru"
                  buttonTypes = Seq(ButtonType.OK,ButtonType.Cancel)

                  dialogPane().setContent(textField)

                  dialogPane().lookupButton(ButtonType.OK).addEventFilter(javafx.event.ActionEvent.ACTION, (_: javafx.event.ActionEvent) => {
                    if (textField.getText.trim.isEmpty) {
                      new Alert(AlertType.Warning) {
                        initOwner(stage)
                        title = "Upozorenje"
                        contentText = "Ime fajla ne može biti prazno!"
                        buttonTypes = Seq(ButtonType.OK)
                      }.showAndWait()
                    }
                  })
                }.showAndWait() match {
                  case Some(ButtonType.OK) => {
                    val fileName = textField.getText.trim
                    if (fileName.nonEmpty) {
                      fromBtnsToFile(fileName,difficulty)
                    }
                  }
                  case _ => null
                }
                }
                stopTimer()
                stage.scene = createMenuScene()}
            },
              loadSekvBtn
            ){
              spacing= 50
              alignment = Pos.Center
              }
            )
        }
      }
    }
    stage.scene = gameScene
  }
  def setFlagged(btn:ButtonWithInfo)={
    btn.flagged = true
    btn.style = Resources.styleOpenField
    val imageUrl = Resources.flag
    val imageView = new ImageView(new Image(imageUrl))
    imageView.fitWidth = btn.minWidth()
    imageView.fitHeight = btn.minHeight()
    btn.padding = Insets.Empty
    imageView.preserveRatio = true
    btn.graphic = imageView
  }
  def fromBtnsToFile(name:String,difficulty:Difficulty)={
    val colNums=gridPane.getColumnCount()

    val groupedButtons = gameButtons.grouped(colNums).toSeq
    val fileContent = groupedButtons.map { row =>
      row.map((btn) => {
        val charStr = btn.character.toString
        val flaggedStr = if (btn.flagged) "F" else ""
        val openedStr = if (btn.opened) "O" else ""
        s"$charStr$flaggedStr$openedStr"
      }).mkString(" ")
    }.mkString("\n")
    val filectntfinaly=fileContent+"\n"+"vreme: "+timeLabel.getText.substring(7)+"\nclick: "+clickCnt+"\ntez: " +difficulty.toString
    Files.write(Paths.get(Resources.savedlvls+s"/${name}"), filectntfinaly.getBytes(StandardCharsets.UTF_8), StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING)


  }

  def isValid(grid:Seq[String],row: Int, col: Int): Boolean = {
    row >= 0 && row < grid.length && col >= 0 && col < grid(row).length

  }

  def openField(btn: ButtonWithInfo, brBombi: Int, row: Int, col: Int): Unit = {
    gameButtons(row * currentLvl(row).length + col).opened = true;
    btn.style = Resources.styleOpenField

    btn.textFill = brBombi match {
      case 1 => Color.Blue
      case 2 => Color.Green
      case 3 => Color.Red
      case 4 => Color.DarkGreen
      case 5 => Color.OrangeRed
      case 6 => Color.DarkRed
      case 7 => Color.Black
      case 8 => Color.IndianRed
      case 0 => Color.LightBlue
    }
    brBombi match {
      case 0 => {
        val directions = List(
          (-1, -1), (-1, 0), (-1, 1),
          (0, -1), (0, 1),
          (1, -1), (1, 0), (1, 1)
        )
        directions.foreach { case (dx, dy) => {
          val newRow = row + dx
          val newCol = col + dy
          if (isValid(currentLvl, newRow, newCol) && currentLvl(newRow)(newCol) != '#') {
            val temp = countMines(currentLvl, newRow, newCol)
            val btn = gameButtons(newRow * currentLvl(newRow).length + newCol)
            if (!btn.opened)
              openField(btn, temp, newRow, newCol)
          }
        }
        }

        btn.text = ""
      }
      case _ => btn.text = {
        gameButtons(row * currentLvl(row).length + col).opened = true;
        brBombi.toString
      }
    }
  }
  val gridPane = new GridPane {
    hgap = 5
    vgap = 5
  }
  val flagCnt=new Text() {
    font = Font.font("Arial", FontWeight.Bold, 15)
    fill = Color.White
  }
  def updateFlagCnt(df:Int)={flagCnt.setText((flagCnt.getText.toInt+df).toString)}
  def wonGame(): Boolean = {
    gameButtons.forall(b=>{
      if(b.opened || b.flagged)
          true
      else false
    })
  }

  def findCoordinates(matrix: Seq[String], ch: String): Seq[(Int, Int)] = {
    matrix.zipWithIndex.flatMap { case (row, rowIndex) =>
      row.split("\\s+").zipWithIndex.collect {
        case (s, colIndex) if s.contains(ch) => (rowIndex, colIndex)
      }
    }
  }

  def fromLevelToGridPane(level: Seq[String],difficulty: Difficulty,clicks:Int): Seq[ButtonWithInfo] = {

    lostGame=false
    sekv=false;
    clickCnt = clicks
    var flagCoord : Seq[(Int, Int)]= Seq.empty
    var openedCoord : Seq[(Int, Int)]= Seq.empty
    if (clicks > 0) {
        flagCoord = findCoordinates(level.dropRight(3), "F")
        openedCoord = findCoordinates(level.dropRight(3), ch = "O")
  }
    val lines = currentLvl
    lines.zipWithIndex.flatMap { case (line, row) =>
      line.trim.zipWithIndex.map { case (char, col) =>
        val brBombi=countMines(currentLvl,row,col)
        val button = new ButtonWithInfo(brBombi,char) {

          if(!flagCoord.isEmpty && flagCoord.contains((row,col))){
            setFlagged(this)
          }
          if (!openedCoord.isEmpty && openedCoord.contains((row, col))) {
            style = Resources.styleOpenField
            opened=true

            textFill = brBombi match {
              case 1 => Color.Blue
              case 2 => Color.Green
              case 3 => Color.Red
              case 4 => Color.DarkGreen
              case 5 => Color.OrangeRed
              case 6 => Color.DarkRed
              case 7 => Color.Black
              case 8 => Color.IndianRed
              case 0 => Color.LightBlue
            }
            brBombi match {
              case 0 => {
                text = ""
              }
              case _ => text = {
                brBombi.toString
              }
            }
          }
          onMouseClicked = event => {
            val clickedbtn = gameButtons(row * currentLvl(row).length + col)
            val helped = gameButtons.findLast(_.helped)
            if (helped != None) {
              helped.get.setStyle(Resources.styleWithELevation)
              helped.get.helped = false;
            }

            if (event.getButton() == MouseButton.PRIMARY) {

              if (!clickedbtn.flagged) {
                clickCnt=clickCnt+1
                if (char == '#') {
                  clickedbtn.opened = true;
                  showIzgubili(clickedbtn,stage)

                }
                else {
                  openField(this, brBombi, row, col)
                }
              }
              } else {
              if (!clickedbtn.opened) {

                if (!clickedbtn.flagged) {
                  if (flagCnt.getText.toInt != 0) {
                    updateFlagCnt(-1)
                    if (flagCnt.getText.toInt == 0) {
                      flagCnt.fill = Color.Red
                    }
                    setFlagged(clickedbtn)
                  }
                } else {

                  updateFlagCnt(1)
                  if (flagCnt.getText.toInt == 1) {
                    flagCnt.fill = Color.White
                  }
                  clickedbtn.flagged = false
                  graphic = null
                  style = Resources.styleWithELevation

                }
              }

            }

            if(wonGame() ){
              stopTimer()
              gridPane.children.foreach { (btn) => {
                btn.setDisable(true)
                }
              }
              val poeniCalc=poeni(clickCnt,timeLabel.getText)
              helpBtn.setDisable(true)
              loadSekvBtn.setDisable(true)
              val winEffects = new WinEffects(s"Imali ste ${poeniCalc} poena")
              winEffects.showWinAnimation()
              if(sekv) {
                winEffects.tf.setText("")
                winEffects.tf.setDisable(true)
              }
              val buttonCancel=new ButtonType("Zatvori")
              new Alert(AlertType.Information) {
                initOwner(stage)
                title = "Cestitke!"
                dialogPane = new DialogPane(){
                  content =winEffects.getVictory
                  prefWidth = 200
                  prefHeight = 200
                  maxWidth = 200
                  maxHeight = 200
                }
                buttonTypes = Seq(buttonCancel)
              }.showAndWait() match {
                case Some(`buttonCancel`) =>
                  val ime = winEffects.getTf
                  if (ime.nonEmpty) {
                    appendResultToFile(s"${difficulty.toString().toLowerCase().charAt(0)}-$ime-$poeniCalc")
                  }
                case _ =>
              }
            }
          }
        }
        GridPane.setRowIndex(button, row)
        GridPane.setColumnIndex(button, col)
        button
      }
    }
  }
  def showIzgubili(btn:ButtonWithInfo,stage:Stage): Unit = {

    val imageUrl = Resources.bomb
    val imageView = new ImageView(new Image(imageUrl))
    imageView.fitWidth = btn.minWidth()
    imageView.fitHeight =btn.minHeight()
    btn.padding = Insets.Empty
    imageView.preserveRatio = true
    btn.graphic = imageView

    stopTimer()

    val losingGifUrl = Resources.izgubiliGif
    val losingGifView = new ImageView(new Image(losingGifUrl))
    losingGifView.fitWidth = 100
    losingGifView.fitHeight = 100
    losingGifView.preserveRatio = true
    lostGame = true

    helpBtn.setDisable(true)
    loadSekvBtn.setDisable(true)

    gridPane.children.foreach { (btn) => {
      btn.setDisable(true)
    }
    }
    new Alert(AlertType.Information) {
      initOwner(stage)
      title = "Game Over"
      headerText = "GOTOVO :("
      contentText = s"Zgazili ste na minu.\n${timeLabel.getText()}"
      graphic = new VBox {
        spacing = 10
        alignment = Pos.Center
        children = Seq(losingGifView)
      }
    }.showAndWait()
  }

  def appendResultToFile( text: String): Unit = {
    val staro = Source.fromFile(Resources.rezultati).getLines().mkString("\n")
    val dodato = staro + "\n" + text
    val writer = new PrintWriter(Resources.rezultati)
    try {
      writer.write(dodato)
    } finally {
      writer.close()
    }
  }
  def poeni(clicks:Int, time:String):Int={
    val timeparts=time.substring(6).split(":")


    val timeinsec:Int=(timeparts(0).trim.toInt*60+timeparts(1).trim.toInt)
     Math.max(1000-timeinsec/2-clicks*5-helpNum*15,0)
  }

  def countMines(level: Seq[String], row: Int, col: Int): Int = {
    val directions = List(
      (-1, -1), (-1, 0), (-1, 1),
      (0, -1),  (0, 1),
      (1, -1), (1, 0), (1, 1)
    )

    directions.count { case (dr, dc) =>
      val newRow = row + dr
      val newCol = col + dc
      if (isValid(level,newRow,newCol)) {
        level(newRow)(newCol) == '#'
      } else {
        false
      }
    }
  }

  var loadSekvBtn:Button = null

  val options = new GridPane {
    alignment = Pos.CenterLeft
    hgap = 30
    vgap = 6
    margin = Insets(10)

  }



  def loadSavedLvls()={
    options.getChildren.clear()
    val buttons = loadSaved()
    buttons.zipWithIndex.foreach { case (button, index) =>
      options.add(button, 9, index)
    }

  }

  def loadSekvencu() = {
    sekv=true

    val fileChooser = new FileChooser {
      initialDirectory = new File(System.getProperty("user.dir"))
      title = "Изаберите фајл"
      extensionFilters ++= Seq(
        new FileChooser.ExtensionFilter("All Files", "*")
      )
    }
        val selectedFile = fileChooser.showOpenDialog(stage)
        if (selectedFile != null) {
          val fileContent = scala.io.Source.fromFile(selectedFile).mkString
          if (!fileContent.matches("([L,R]\\([0-9],[0-9]\\):)*[L,R]\\([0-9],[0-9]\\)$")) {
            new Alert(AlertType.Error) {
              initOwner(stage)
              title = "Neispravan format!"
              headerText = "Sadrzaj fajla sa sekvencom mora da bude u formatu L(x,y):R(x,y).."
              buttonTypes = Seq(ButtonType.Close)
            }.showAndWait()

          } else {
            val sekvenca = fileContent.split(":").foreach((e) => {
              if (e.startsWith("R")) {
                val row = e.substring(2).split(",")(0).toInt
                val col = e.substring(2).split(",")(1).dropRight(1).toInt

                val btn = gameButtons(row * currentLvl(0).length + col)
                if (btn.flagged) {
                  updateFlagCnt(1)
                  if (flagCnt.getText.toInt == 1) {
                    flagCnt.fill = Color.White
                  }
                  btn.flagged = false
                  btn.graphic = null
                  btn.style = Resources.styleWithELevation

                } else {
                  if (flagCnt.getText.toInt != 0) {
                    updateFlagCnt(-1)
                    if (flagCnt.getText.toInt == 0) {
                      flagCnt.fill = Color.Red
                    }
                    setFlagged(btn)
                  } else {
                    println("Preskocen klik desni, nemoguce postaviti zastavicu")
                  }
                }
              } else {
                val row = e.substring(2).split(",")(0).toInt
                val col = e.substring(2).split(",")(1).dropRight(1).toInt

                val btn = gameButtons(row * currentLvl(0).length + col)
                btn.opened = true
                if (btn.character != '#')
                  openField(btn, btn.num, row, col)
                else
                  showIzgubili(btn, stage)

              }
            })
            if (wonGame()) {
              stopTimer()
              helpBtn.setDisable(true)
              loadSekvBtn.setDisable(true)

              val winEffects = new WinEffects("Ne mozemo vam preracunati poene jer vreme nije validna mera")
              winEffects.showWinAnimation()
              winEffects.removeText()
              val buttonCancel = new ButtonType("Zatvori")
              new Alert(AlertType.Information) {
                initOwner(stage)
                title = "Cestitke!"
                dialogPane = new DialogPane() {
                  content = winEffects.getVictory
                  prefWidth = 200
                  prefHeight = 200
                  maxWidth = 200
                  maxHeight = 200
                }
                buttonTypes = Seq(buttonCancel)
              }.showAndWait()
            }
          }
        }
  }
  def updateLevelOptions(difficulty: Difficulty): Unit = {
    options.children.clear()
    val i = difficulty match {
      case Pocetnik => 0
      case Srednji => 3
      case Ekspert => 6
      case null => -1
    }
    if(i.equals(-1)) return
    val buttons= loadLevels(difficulty)
    val btnRandom=new Button("Random") {
      style = Resources.randomBtnStyle
      onAction = (e) => {
        if (buttons.nonEmpty) {
          val randomIndex = Random.nextInt(buttons.length)
          val randomButton = buttons(randomIndex)
          randomButton.fire()
        } else
          println("Nema nivoa :(")

      }
    }
    options.add(btnRandom,i,0)

    buttons.zipWithIndex.foreach { case (button, index) =>
      options.add(button, i, index+1)
    }
  }

  def createMenuScene(): Scene = {
    new Scene {

      fill = Color.rgb(124, 167, 235)
      content = new StackPane {
        prefHeight = 700
        maxHeight=700
        padding = Insets(200)
        alignment = Pos.Center
        children = new VBox {

          prefHeight = 500
          maxHeight = 500
          alignment = Pos.Center
          spacing = 20
          children = Seq(
            new Text {
              text = "Izbor tezine:"
              font = Font.font("Arial", FontWeight.Bold, 24)
              fill = Color.White
            },
            new HBox {
              alignment = Pos.Center
              spacing = 10
              children = Seq(
                new Button("Početnik") {
                  style = "-fx-background-color: lightgreen; -fx-font-size: 16px;"
                  onAction = (r) => updateLevelOptions(Pocetnik)
                },
                new Button("Srednji") {
                  style = "-fx-background-color: yellow; -fx-font-size: 16px;"
                  onAction = (r) => updateLevelOptions(Srednji)

                },
                new Button("Ekspert") {
                  style = "-fx-background-color: red; -fx-font-size: 16px;"
                  onAction = (r) => updateLevelOptions(Ekspert)

                },
              new Button("Ucitaj sacuvanu Igru") {
                style = Resources.loadBtnStyle
                onAction = (r) => loadSavedLvls()

              }
              )
            },
            options,
            new Button("Rezultati") {
              maxWidth= 200
              style = Resources.rezultatiBtnStyle
              onAction=(r)=>{
                val results=readFile(Resources.rezultati)
                val pocetnici=results.filter(_.contains("p-")).map((r)=>r.substring(2).replace("-",": "))
                val srednji = results.filter(_.contains("s-")).map((r) => r.substring(2).replace("-", ": "))
                val eksperti = results.filter(_.contains("e-")).map((r) => r.substring(2).replace("-", ": "))

                new Alert(AlertType.Information) {
                  initOwner(stage)
                  title = "Rezultati"
                  prefWidth = 500
                  cancelButton = true
                 dialogPane = new DialogPane() {
                   prefWidth = 500
                   buttonTypes = Seq(new ButtonType("Zatvori"))
                   content = new HBox(
                     new VBox(
                       new Label("Pocetnici"),
                       new TextArea() {
                         prefHeight = 200
                         text= "Ime:\t\t\tRezultat\n"+pocetnici.mkString("\n")
                         editable = false
                       }
                     ),
                     new VBox(
                       new Label("Srednji"),
                       new TextArea() {
                         prefHeight = 200
                         text = "Ime:\t\t\tRezultat\n"+srednji.mkString("\n")
                         editable = false
                       }
                     ),
                     new VBox(
                       new Label("Eksperti"),
                       new TextArea() {
                         prefHeight = 200
                         text ="Ime:\t\t\tRezultat\n"+ eksperti.mkString("\n")
                         editable = false
                       }
                     ),
                   )
                 }
                }.showAndWait()
              }

            }
          )
        }
      }
    }

  }


  override def start(): Unit = {

    timeLabel = new Label {
      font = Font.font("Arial", FontWeight.Bold, 18)
      textFill = Color.White
      text = "Vreme: 00:00"
    }
    stage = new JFXApp3.PrimaryStage {
      resizable = false
      icons += new Image(Resources.bomb)
      title = "Minesweeper in Scala"
      scene = createMenuScene()
      updateLevelOptions(difficulty = null)
    }


  }
}