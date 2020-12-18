sealed trait Cube
case object Active extends Cube
case object Inactive extends Cube

case class Coordinates(x : Int, y : Int, z : Int){
  def getSurrounds : List[Coordinates] =
    (for {
     x1 <- x - 1 to x + 1
     y1 <- y - 1 to y + 1
     z1 <- z - 1 to z + 1
     if (x1 != x) || (y1 != y) || (z1 != z)
    } yield Coordinates(x1, y1, z1)).toList
}

case class Experiment( cubes : Map[(Int, Int, Int), Cube] ) {
  def setCube(coordinates : Coordinates, cube : Cube) : Experiment = {
    import coordinates._
    Experiment(cubes + ((z, y, x) -> cube))
  }

  def getCube(coordinates: Coordinates) : Cube = {
    import coordinates._
    cubes(z, y, x)
  }
  def getMinsAndMaxes : (Coordinates, Coordinates) = {
    val zs = cubes.keys.map(_._1)
    val ys = cubes.keys.map(_._2)
    val xs = cubes.keys.map(_._3)
    (Coordinates(xs.min, ys.min, zs.min),
    Coordinates(xs.max, ys.max, zs.max))
  }
  def getSurroundingActive(coordinates: Coordinates) : Int = {
    coordinates.getSurrounds.map(getCube).count(_ == Active)
  }

  def checkCube(coordinates: Coordinates) : Option[Cube] = {
    val cube = getCube(coordinates)
    val activeSurrounds = getSurroundingActive(coordinates)
    if (cube == Active) {
      if ((activeSurrounds == 2) || (activeSurrounds == 3))
        None
      else
        Some(Inactive)
     } else {
      if (activeSurrounds == 3) Some(Active) else None
    }
  }

  def generation : Experiment = {
    val (mins, maxes) = getMinsAndMaxes
    val cubesList = (for {
      x <- mins.x -1 to maxes.x + 1
      y <- mins.y -1 to maxes.y + 1
      z <- mins.z -1 to maxes.z + 1
    } yield (z, y, x)).toList
    val cubeChanges = cubesList.flatMap(coords => checkCube(Coordinates(coords._3, coords._2, coords._1)).map(cube => (coords , cube))).toMap.withDefaultValue(Inactive)
    Experiment(cubes ++ cubeChanges)
  }
}

object Experiment {
  def fromInput(input : List[List[Char]]): Experiment = {
    val cubes = input.zipWithIndex.flatMap(
      line => line._1.zipWithIndex.map(
        entry => ((0, line._2, entry._2), entry._1 match {
          case '#' => Active
          case '.' => Inactive
        })
     )
    ).toMap.withDefaultValue(Inactive)
    Experiment(cubes)
  }
}

object Day17 extends App {

  val fileName = "/home/zoe/Work/Repositories/AdventOfCode/src/main/resources/Day17TestData"
  val input = scala.io.Source.fromFile(fileName).getLines().toList.map(_.toList)

  def findAnswer1(input: List[List[Char]]) = {
    (1 to 6).foldLeft(Experiment.fromInput(input))((experiment, _) => experiment.generation).cubes.count(_._2 == Active)
  }


  println(findAnswer1(input))
 // println(findAnswer2(input))
}

