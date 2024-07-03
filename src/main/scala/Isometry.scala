import scala.collection.mutable

case class Point(x: Int, y: Int)
trait Operation {
  def applyOperation(map: Array[Array[Char]], sector: Array[Array[Char]], topLeftT: Point, bottomRightT: Point, topLeftI: Point, bottomRightI: Point): Array[Array[Char]]
}

trait Transparent extends Operation {
  def applyOperation(map: Array[Array[Char]], sector: Array[Array[Char]], topLeftT: Point, bottomRightT: Point, topLeftI: Point, bottomRightI: Point): Array[Array[Char]] = {
    val result = map.map(_.clone())

    for {
      i <- sector.indices
      j <- sector(0).indices
    } {
        val transformedX = topLeftT.x + i
        val transformedY = topLeftT.y + j
        result(transformedX)(transformedY) = sector(i)(j)


    }
    for (i <- topLeftI.x to bottomRightI.x; j <- topLeftI.y to bottomRightI.y)
     {
      if (!(i >= topLeftT.x && i <= bottomRightT.x && j >= topLeftT.y && j <= bottomRightT.y)) {

        result(i)(j) = '-'
      }

    }

    result
  }
}

trait NonTransparent extends Operation { //I-initial T-transformed
  def applyOperation(map: Array[Array[Char]], sector: Array[Array[Char]], topLeftT: Point, bottomRightT: Point, topLeftI: Point, bottomRightI: Point): Array[Array[Char]] = {
    val result = map.map(_.clone())
    for (i <- topLeftI.x to bottomRightI.x; j <- topLeftI.y to bottomRightI.y) {
      result(i)(j) = '-'
    }
    for (i <- topLeftT.x to bottomRightT.x; j <- topLeftT.y to bottomRightT.y) {
      result(i)(j) = sector(i - topLeftT.x)(j - topLeftT.y)
    }

    result
  }
}

trait Expandable extends Operation {
  abstract override def applyOperation(map: Array[Array[Char]], sector: Array[Array[Char]], topLeftT: Point, bottomRightT: Point, topLeftI: Point, bottomRightI: Point): Array[Array[Char]] = {
    val negRows = if (topLeftT.x < 0) Math.abs(topLeftT.x) else 0
    val negCols = if (topLeftT.y < 0) Math.abs(topLeftT.y) else 0
    println("tacka T")
    println(topLeftT, bottomRightT)

    val newRows = math.max(0, bottomRightT.x + 1 - map.length) + negRows
    val newCols = math.max(0, bottomRightT.y + 1 - map(0).length) + negCols

    val expandedMap = Array.ofDim[Char](map.length + newRows, map(0).length + newCols).map(_.map(_ => '-'))

    for (i <- map.indices; j <- map(i).indices) {
      expandedMap(i + negRows)(j + negCols) = map(i)(j)
    }

    val newTopLeftI = Point(topLeftI.x + negRows, topLeftI.y + negCols)
    val newBottomRightI = Point(bottomRightI.x + negRows, bottomRightI.y + negCols)

    println(topLeftT.copy(x = topLeftT.x + negRows, y = topLeftT.y + negCols), bottomRightT.copy(x = bottomRightT.x + negRows, y = bottomRightT.y + negCols))

    super.applyOperation(expandedMap, sector, topLeftT.copy(x = topLeftT.x + negRows, y = topLeftT.y + negCols), bottomRightT.copy(x = bottomRightT.x + negRows, y = bottomRightT.y + negCols), newTopLeftI, newBottomRightI)
  }
}

trait NonExpandable extends Operation {
  abstract override def applyOperation(map: Array[Array[Char]], sector: Array[Array[Char]], topLeftT: Point, bottomRightT: Point, topLeftI: Point, bottomRightI: Point): Array[Array[Char]] = {
    super.applyOperation(map, sector, topLeftT, bottomRightT, topLeftI, bottomRightI)
  }
}


abstract class IsometricTransformation extends Operation   {
  def applyOperation(map: Array[Array[Char]], sector: Array[Array[Char]], topLeftT: Point, bottomRightT: Point, topLeftI: Point, bottomRightI: Point): Array[Array[Char]] = {
     applyOperation(map, sector, topLeftT, bottomRightT,topLeftI,bottomRightI)
  }
}



trait Isometry {
  def applyIsometry(sector: Array[Array[Char]], pivot: Point,topLeft:Point,bottomRight:Point): (Array[Array[Char]], Point, Point)

  def inverse:Isometry
}
class Rotate90(clockwise:Boolean) extends Isometry{

  def applyIsometry(sector: Array[Array[Char]], pivot: Point,topLeft:Point,bottomRight:Point): (Array[Array[Char]], Point, Point)  = {
    val rows = sector.length
    val cols = if (rows > 0) sector(0).length else 0
    if (clockwise) rotate90Clockwise(sector, pivot,topLeft,bottomRight) else rotate90CounterClockwise(sector, pivot,topLeft,bottomRight)
  }

  def rotate90Clockwise(sector: Array[Array[Char]], pivot: Point,topLeft:Point,bottomRight:Point): (Array[Array[Char]], Point, Point) = {
    val rows = sector.length
    val cols = sector(0).length
    val rotated = Array.ofDim[Char](cols, rows)

    for (i <- sector.indices; j <- sector(i).indices) {
      rotated(j)(rows - 1 - i) = sector(i)(j)
    }
    val newTopLeft = Point( pivot.x - (pivot.y-topLeft.y),pivot.y - (bottomRight.x-pivot.x))
    val newBottomRight = Point(newTopLeft.x+cols-1,newTopLeft.y+rows-1)


    (rotated, newTopLeft, newBottomRight)
  }

  def rotate90CounterClockwise(sector: Array[Array[Char]], pivot: Point,topLeft:Point,bottomRight:Point): (Array[Array[Char]], Point, Point) = {
    val rows = sector.length
    val cols = sector(0).length
    val rotated = Array.ofDim[Char](cols, rows)

    for (i <- sector.indices; j <- sector(i).indices) {
      rotated(cols - 1 - j)(i) = sector(i)(j)
    }

    val newTopLeft = Point(pivot.x - (bottomRight.y-pivot.y), pivot.y - (pivot.x - topLeft.x))
    val newBottomRight = Point(newTopLeft.x+cols-1, newTopLeft.y+rows-1)

    (rotated, newTopLeft, newBottomRight)
  }

  override def inverse: Isometry = new Rotate90(!clockwise)
}
class ReflectByColumn extends Isometry {
  def applyIsometry(sector: Array[Array[Char]], pivot: Point, topLeft: Point, bottomRight: Point): (Array[Array[Char]], Point, Point) = {
    val rows = sector.length
    val cols = sector(0).length
    val reflected = Array.ofDim[Char](rows, cols)

    for {
      i <- 0 until rows
      j <- 0 until cols
    } reflected(i)(j) = sector(i)(cols - 1 - j)

    val newTopLeftX = topLeft.x
    val newTopLeftY = topLeft.y+2*(pivot.y-topLeft.y)-(bottomRight.y-topLeft.y)

    val newBottomRightX = bottomRight.x
    val newBottomRightY = bottomRight.y+2*(pivot.y-bottomRight.y)+(bottomRight.y-topLeft.y)

    val newTopLeft = Point(newTopLeftX, newTopLeftY)
    val newBottomRight = Point(newBottomRightX, newBottomRightY)

    (reflected, newTopLeft, newBottomRight)
  }

  override def inverse: Isometry = new ReflectByColumn {}
}

class ReflectByRow extends Isometry {
  def applyIsometry(sector: Array[Array[Char]], pivot: Point, topLeft: Point, bottomRight: Point): (Array[Array[Char]], Point, Point) = {
    val rows = sector.length
    val cols = sector(0).length
    val reflected = Array.ofDim[Char](rows, cols)

    for {
      i <- 0 until rows
      j <- 0 until cols
    } reflected(i)(j) = sector(rows - 1 - i)(j)


    val newTopLeftX = 2 * pivot.x - bottomRight.x
    val newTopLeftY = topLeft.y

    val newBottomRightX = 2 * pivot.x - topLeft.x
    val newBottomRightY = bottomRight.y


    val newTopLeft = Point(newTopLeftX, newTopLeftY)
    val newBottomRight = Point(newBottomRightX, newBottomRightY)

    (reflected, newTopLeft, newBottomRight)
  }

  override def inverse: Isometry = new ReflectByRow {}
}


 class ReflectByMainDiagonal extends Isometry {
  def applyIsometry(sector: Array[Array[Char]], pivot: Point,topLeft:Point,bottomRight:Point): (Array[Array[Char]], Point, Point) = {
    val rows = sector.length
    val cols = sector(0).length
    val reflected = Array.ofDim[Char](cols, rows)

    for {
      i <- 0 until rows
      j <- 0 until cols
    } reflected(j)(i) = sector(i)(j)

    (reflected, topLeft, bottomRight)
  }

  override def inverse: Isometry = new ReflectByMainDiagonal {}
}
 class ReflectByAntiDiagonal extends Isometry {
  def applyIsometry(sector: Array[Array[Char]], pivot: Point,topLeft:Point,bottomRight:Point): (Array[Array[Char]], Point, Point) = {
    val rows = sector.length
    val cols = sector(0).length
    val reflected = Array.ofDim[Char](cols, rows)

    for {
      i <- 0 until rows
      j <- 0 until cols
    } reflected(cols - 1 - j)(rows - 1 - i) = sector(i)(j)

    (reflected,topLeft,bottomRight)
  }

  override def inverse: Isometry = new ReflectByMainDiagonal {}
}
class CompositeIsometry( isometries: List[Isometry]) extends Isometry {

  override def applyIsometry(sector: Array[Array[Char]], pivot: Point, topLeft: Point, bottomRight: Point): (Array[Array[Char]], Point, Point) = {
    isometries.foldLeft((sector, topLeft, bottomRight)) {
      case ((currentSector, currentTopLeft, currentBottomRight), isometry) =>
        isometry.applyIsometry(currentSector, pivot, currentTopLeft, currentBottomRight)
    }
  }

  override def inverse: Isometry = new CompositeIsometry(isometries.reverse.map(_.inverse))
}

object NamedIsometriesList {
  private val namedIsometries = mutable.Map[String, CompositeIsometry]()

  def addIsometry(name: String, isometries: List[Isometry]): Unit = {
    namedIsometries(name) = new CompositeIsometry(isometries)
  }

  def getIsometry(name: String): Option[CompositeIsometry] = {
    namedIsometries.get(name)
  }
  def getAllIsometries: Map[String, CompositeIsometry] = {
    namedIsometries.toMap
  }
}