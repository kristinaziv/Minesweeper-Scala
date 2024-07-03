import java.io.PrintWriter
import scala.annotation.tailrec

object MapGenerator extends App{

 def addRow(map:Array[Array[Char]],last:Boolean): Array[Array[Char]] = {

   val newRow = Array.fill(map(0).length)('-')
   last match {
     case true => map :+ newRow
     case false => newRow +: map
   }
 }

  def addColumn(map: Array[Array[Char]], last: Boolean): Array[Array[Char]] = {

    last match {
      case true => map.map(row => row :+ '-')
      case false => map.map(row => '-' +: row)
    }
  }

  def removeRow(map: Array[Array[Char]], last: Boolean): Array[Array[Char]] = {
    last match {
      case true => map.dropRight(1)
      case false => map.drop(1)
    }
  }

  def removeColumn(map: Array[Array[Char]], last: Boolean): Array[Array[Char]] = {
    last match {
      case true => map.map(row => row.dropRight(1))
      case false => map.map(row => row.drop(1))
    }
  }


  def changeCharacter(map: Array[Array[Char]], p: Point): Array[Array[Char]] = {
    map.zipWithIndex.map { case (row, i) =>
      row.zipWithIndex.map { case (cell, j) =>
        if (i == p.x && j == p.y) {
          cell match {
            case Resources.MINE => Resources.BLANK_SPACE
            case Resources.BLANK_SPACE => Resources.MINE
            case other => other
          }
        } else {
          cell
        }
      }
    }
  }


  def cleanSector(map: Array[Array[Char]], topLeft: Point, bottomRight: Point): Array[Array[Char]] = {
    val startX = math.max(0, math.min(topLeft.x, bottomRight.x))
    val endX = math.min(map.length - 1, math.max(topLeft.x, bottomRight.x))
    val startY = math.max(0, math.min(topLeft.y, bottomRight.y))
    val endY = math.min(map(0).length - 1, math.max(topLeft.y, bottomRight.y))

    map.zipWithIndex.map { case (row, i) =>
      row.zipWithIndex.map { case (cell, j) =>
        if (i >= startX && i <= endX && j >= startY && j <= endY) '-' else cell
      }
    }
  }

  def checkValidity(map:Array[Array[Char]],difficulty: Difficulty): Boolean = {
    val mineCnt=map.flatten.count(_=='#')
    val rows=map.length
    if(rows==0){
      print("Mapa je prazna, nije falidna")
      return false
    }
    val columns=map(0).length
    difficulty match {
       case Pocetnik => rows == Resources.Begginer_Size._1 && columns == Resources.Begginer_Size._2 && mineCnt> Resources.Begginer_Bombs._1 && mineCnt<Resources.Begginer_Bombs._2
       case Srednji => rows == Resources.Medium_Size._1 && columns == Resources.Medium_Size._2 && mineCnt> Resources.Medium_Bombs._1 && mineCnt<Resources.Medium_Bombs._2
       case Ekspert  => rows == Resources.Expert_Size._1 && columns == Resources.Expert_Size._2 && mineCnt> Resources.Expert_Bombs._1 && mineCnt<Resources.Expert_Bombs._2
    }
  }

  def extractSector(map: Array[Array[Char]], topLeft: Point, bottomRight: Point): Array[Array[Char]] = {
    map.slice(topLeft.x, bottomRight.x + 1).map(row => row.slice(topLeft.y, bottomRight.y + 1))
  }

  /*def compose(isometries: Seq[Isometry]): Isometry = new Isometry {
    override def applyIsometry(sector: Array[Array[Char]], pivot: Point, topLeft: Point, bottomRight: Point): (Array[Array[Char]], Point, Point) = {
      isometries.foldRight((sector, topLeft, bottomRight)) { (iso2, acc) =>
        val (resultSector, newTopLeft, newBottomRight) = iso2.applyIsometry(acc._1, pivot, acc._2, acc._3)
        (resultSector, newTopLeft, newBottomRight)
      }
    }

    override def inverse: Isometry = new Isometry {
      override def applyIsometry(sector: Array[Array[Char]], pivot: Point, topLeft: Point, bottomRight: Point): (Array[Array[Char]], Point, Point) =
        compose(isometries.map(_.inverse)).applyIsometry(sector, pivot, topLeft, bottomRight)

      override def inverse: Isometry = compose(isometries.map(_.inverse))
    }
  }*/



  def printIsometryMenu(): Seq[Isometry] = {
    println("Unesite izometrije koje želite da kombinujete \n1 - Rotate90 / clockwise\n2- Rotate90 / counterclockwise\n3 - ReflectByColumn\n" +
      "4 - ReflectByRow\n" +
      "5 - ReflectByMainDiag\n"+
      "6 - ReflectByAlterDiag\n" +
      "q - završi unos: ")
    val isometries = scala.collection.mutable.ListBuffer[Isometry]()
    var continue = true

    @tailrec
    def print():Unit={
      val input = scala.io.StdIn.readLine().trim.toLowerCase
      val inversed=if(!input.equals("q")) {
        printf("da li zelite inverz od izabrane izometrije?(y,n)?")
        if (scala.io.StdIn.readLine().trim.toLowerCase == 'y') true else false
      }else true

      input match {
        case "1" => isometries += new Rotate90(!inversed)
        case "2" => isometries += new Rotate90(!inversed)
        case "3" => isometries +=(if (!inversed) new ReflectByColumn {} else new ReflectByColumn {}.inverse)
        case "4" => isometries +=(if (!inversed) new ReflectByRow {} else new ReflectByRow {}.inverse)
        case "5" => isometries +=(if (!inversed) new ReflectByMainDiagonal {} else new ReflectByMainDiagonal {}.inverse)
        case "6" =>isometries += (if (!inversed) new ReflectByAntiDiagonal {} else new ReflectByAntiDiagonal {}.inverse)
        case "q" => continue = false
        case _ => println("Nedefinisan izbor!\n ")
      }
      if(continue) print()
    }
    print()
    isometries.toList
  }

  def saveMapToFile(map: Array[Array[Char]], level: String, username: String): Unit = {
    val fileName = s"$level-${username}.txt"
    val writer = new PrintWriter(Resources.mapDir+"/"+fileName)
    try {
      map.foreach(row => writer.println(row.mkString("")))
    } finally {
      writer.close()
    }
    println(s"Mapa je sacuvana u fajlu: $fileName")
  }

  def printMapRules(): Unit = {
    println("Tezine su:")
    println("Pocetnik:")
    println(s"Velicina mape: ${Resources.Begginer_Size._1} x ${Resources.Begginer_Size._2}")
    println(s"Broj bombi: od ${Resources.Begginer_Bombs._1} do ${Resources.Begginer_Bombs._2}")
    println()
    println("Srednji:")
    println(s"Velicina mape: ${Resources.Medium_Size._1} x ${Resources.Medium_Size._2}")
    println(s"Broj bombi: od ${Resources.Medium_Bombs._1} do ${Resources.Medium_Bombs._2}")
    println()
    println("Ekspert:")
    println(s"Velicina mape: ${Resources.Expert_Size._1} x ${Resources.Expert_Size._2}")
    println(s"Broj bombi: od ${Resources.Expert_Bombs._1} do ${Resources.Expert_Bombs._2}")
  }


  @tailrec
  def printBasicMenu(map:Array[Array[Char]]): Array[Array[Char]] = {

    val allIsometries = NamedIsometriesList.getAllIsometries

    val baseMenu = "Unesite redni broj operacije koju zelite da izvrsite nad mapom\n" +
      "1 - prosiri redom - kraj\n" +
      "2 - prosiri redom - pocetak\n" +
      "3 - prosiri kolonom -pocetak\n" +
      "4 - prosiri kolonom - kraj\n" +
      "5 - ukloni red - kraj \n" +
      "6 - ukloni red - pocetak\n" +
      "7 - ukloni kolonu -pocetak\n" +
      "8 - ukloni kolonu - kraj\n" +
      "9 - zamena polja na poziciji\n" +
      "10 - ciscenje sektora\n" +
      "11 - transformacije - proizvoljne izometrije\n" +
      "12 - proveri validnost mape\n" +
      "13 - upisi u fajl\n" +
      "14 - translacije\n" +
      "15 - centralna simetrija\n"

    val isometryMenu = allIsometries.zipWithIndex.map {
      case ((name, _), index) => s"${index + 16} - $name"
    }.mkString("\n")

    println(baseMenu + isometryMenu + "\nq - završi unos: ")


    var continue = true

    val input = scala.io.StdIn.readLine().trim.toLowerCase
     val newMap:Array[Array[Char]]= input match {
        case "1" => addRow(map,true)
        case "2" => addRow(map,false)
        case "3" => addColumn(map,false)
        case "4" => addColumn(map,true)
        case "5" => removeRow(map,true)
        case "6" => removeRow(map,false)
        case "7" => removeColumn(map,false)
        case "8" => removeColumn(map,true)
        case "9" => {
          println("Unesite koordinate tacke (u formatu x1,y1): ")
          val t1 = scala.io.StdIn.readLine().trim.split(',')
          changeCharacter(map,Point(t1(0).toInt,t1(1).toInt))
        }
        case "10" => {
          val t1 = getCoordinateFromString("Unesite koordinate topLeft tacke sektora (u formatu x1,y1): ")
          val t2 = getCoordinateFromString("Unesite koordinate bottomRight tacke sektora (u formatu x2,y2): ")
          cleanSector(map, t1,t2)
        }
        case "11" =>
          val T1 = getCoordinateFromString("Unesite koordinate topLeft tacke sektora (u formatu x1,y1): ")
          val T2 = getCoordinateFromString("Unesite koordinate bottomRight tacke sektora (u formatu x2,y2): ")
          val tP = getCoordinateFromString("Unesite tacku oko koje zelite da se desava izometrija: ")

          checkDomen(map,Seq(T1,T2,tP))

          val chosenIsometries = printIsometryMenu()

          val sector: Array[Array[Char]] = extractSector(map,T1,T2)

          if(chosenIsometries.length>1) {
            println("Unesite ime kompozicije izometrija(jedna rec): ")
            NamedIsometriesList.addIsometry(scala.io.StdIn.readLine().trim, chosenIsometries.toList)
          }

          val (transformedSector: Array[Array[Char]],topLeftT:Point,bottomLeftT:Point) =new CompositeIsometry(chosenIsometries.toList).applyIsometry(sector,tP,T1,T2)

          println("Izaberite Prosirivost: 1-da ; 2-ne")
          val exp = scala.io.StdIn.readLine().trim.toInt
          println("Izaberite transparentnost: 1-da ; 2-ne")
          val trans = scala.io.StdIn.readLine().trim.toInt

          val tranformedMap= exp match {
            case 1 => trans match {
              case 1=>new IsometricTransformation with Transparent with Expandable
              case 2=>new IsometricTransformation  with NonTransparent with Expandable
              case _=>
                println("izabrano default-na opcija Transparent+Expandable")
                new IsometricTransformation with Transparent with Expandable
            }
            case 2=> trans match {
              case 1 => new IsometricTransformation with Transparent  with NonExpandable
              case 2 => new IsometricTransformation with NonTransparent  with NonExpandable
              case _ =>
                println("izabrano default-na opcija Transparent+Expandable")
                new IsometricTransformation with Transparent with NonExpandable
            }
            case _ =>
              println("izabrano default-na opcija Transparent+Expandable")
              new IsometricTransformation with Transparent with Expandable


          }
          tranformedMap.applyOperation(map,transformedSector,topLeftT,bottomLeftT,T1,T2)

        case "12"=>
          println("Unesite za koji nivo proveravate validnost (p-Pocetni;s-Srednji;e-Ekspert): ")
          val diff = scala.io.StdIn.readLine().trim match {
            case "p"=>Pocetnik
            case "s"=>Srednji
            case "e"=>Ekspert
          }
          printf("Validnost mape: %b\n",checkValidity(map,diff))
          map
        case "13"=>
          val validBeginner = checkValidity(map, Pocetnik)
          val validMedium = checkValidity(map, Srednji)
          val validExpert = checkValidity(map, Ekspert)
          println("Unesite ime fajla: ");
          val name=scala.io.StdIn.readLine().trim

          if (validBeginner) saveMapToFile(map, "p", name)
          else if (validMedium) saveMapToFile(map, "s", name)
          else if (validExpert) saveMapToFile(map, "e", name)
          else {
            println("Nazalost ne mozemo sacuvati vas nivo posto ne ispunjava kriterijume ni za jednu tezinu :(")
            printMapRules()
          }

          map
        case "14"=>

          val topLeft=getCoordinateFromString("Unesite koordinate topLeft tacke sektora (u formatu x1,y1): ")
          val bottomRight =getCoordinateFromString("Unesite koordinate bottomRight tacke sektora (u formatu x2,y2): ")

          checkDomen(map, Seq(topLeft,bottomRight))
          val sector: Array[Array[Char]] = extractSector(map, topLeft, bottomRight)
          println("Dobro dosli u translaciju po x osi! Unesite za koliko zelite da se translira izabrani sektor (-d/+d): ")
          val d = scala.io.StdIn.readLine().trim.toInt
          val firstReflexion= if (d<0) topLeft.y-Math.abs(d)/2 else  bottomRight.y+Math.abs(d)/2
          val (firstIsometry,tt1:Point,tt2:Point)=new ReflectByRow().applyIsometry(sector,bottomRight,topLeft,bottomRight)
          val (second, ttt1, ttt2) = new ReflectByColumn().applyIsometry(firstIsometry, Point(tt1.x,firstReflexion), tt1, tt2)
          val (third, t41, t42) = new ReflectByRow().applyIsometry(second, ttt1, ttt1, ttt2)
          val (forth, t51, t52) = new ReflectByColumn().applyIsometry(third, Point(0,(t42.y-t41.y)/2+t41.y), t41, t42)
          val finaly=(new IsometricTransformation with NonTransparent with Expandable ).applyOperation(map,forth,t51,t52,topLeft,bottomRight)
          printf("po default-u expandable+nontransparent")


          finaly
        case "15"=>map

        case num if num.matches("[0-9]+") && num.toInt > 15 =>
          val isometryIndex = num.toInt - 16
          if (isometryIndex < allIsometries.size) {
            val isometryName = allIsometries.keys.toList(isometryIndex)
            val compositeIsometry = NamedIsometriesList.getIsometry(isometryName).get

            val t1:Point = getCoordinateFromString("Unesite koordinate topLeft tacke sektora (u formatu x1,y1):")
            val t2:Point = getCoordinateFromString("Unesite koordinate bottomRight tacke sektora (u formatu x2,y2):")
            val pivot = getCoordinateFromString("Unesite tacku oko koje zelite da se desava izometrija: ")
            val sector: Array[Array[Char]] = extractSector(map, t1, t2)

            val (transformedSector, topLeftT, bottomLeftT) = compositeIsometry.applyIsometry(sector, pivot, t1, t2)
            println("Izaberite Prosirivost: 1-da ; 2-ne")
            val exp = scala.io.StdIn.readLine().trim.toInt
            println("Izaberite transparentnost: 1-da ; 2-ne")
            val trans = scala.io.StdIn.readLine().trim.toInt

            val tranformedMap = exp match {
              case 1 => trans match {
                case 1 => new IsometricTransformation with Transparent with Expandable
                case 2 => new IsometricTransformation with NonTransparent with Expandable
                case _ =>
                  println("izabrano default-na opcija Transparent+Expandable")
                  new IsometricTransformation with Transparent with Expandable
              }
              case 2 => trans match {
                case 1 => new IsometricTransformation with Transparent with NonExpandable
                case 2 => new IsometricTransformation with NonTransparent with NonExpandable
                case _ =>
                  println("izabrano default-na opcija Transparent+Expandable")
                  new IsometricTransformation with Transparent with NonExpandable
              }
              case _ =>
                println("izabrano default-na opcija Transparent+Expandable")
                new IsometricTransformation with Transparent with Expandable


            }
            tranformedMap.applyOperation(map,transformedSector,topLeftT,bottomLeftT,t1,t2)
          } else {
            println("Nedefinisan izbor")
            map
          }

        case "q" => continue = false
        map
        case _ =>
          println("Nedefinisan izbor!\n ")
         map
      }
    println("Mapa: ")
    newMap.foreach(row => println(row.mkString(" ")))
    if (continue) printBasicMenu(newMap)
    else newMap

  }
  def checkDomen(array: Array[Array[Char]], value: Seq[Point]): Unit = {
    require(value.forall((p)=>{p.x<=(array.length-1) && p.x >= 0 && p.y<=(array(0).length) && p.y>=0}),
      " Sve tačke moraju biti unutar granica mape.")
  }

  def getCoordinateFromString(str: String): Point = {
    println(str)
    val input = scala.io.StdIn.readLine().trim.split(',')
    Point(input(0).toInt, input(1).toInt)
  }
  println("Dobrodošli u opcije za kreiranje nivoa!")
  println("Molim vas unesite broj redova pocetne mape: ")
  val initialRows = scala.io.StdIn.readLine().trim.toInt
  println("Molim vas unesite broj kolona pocetne mape: ")
  val initialCols = scala.io.StdIn.readLine().trim.toInt
  println("Cestitamo! Uneli ste pocetnu mapu koja izgleda ovako: ")
  val map:Array[Array[Char]] = Array.fill(initialRows, initialCols)('#')
  map.foreach(row => println(row.mkString(" ")))
  println("Sada mozemo da krenemo sa generisanjem mapa!")

  printBasicMenu(map)

}
