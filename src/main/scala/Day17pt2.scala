case class Coordinates2(x : Int, y : Int, z : Int, w : Int){
  def getSurrounds : List[Coordinates2] =
    (for {
     x1 <- x - 1 to x + 1
     y1 <- y - 1 to y + 1
     z1 <- z - 1 to z + 1
     w1 <- w - 1 to w + 1
     if (x1 != x) || (y1 != y) || (z1 != z) || (w1 != w)
    } yield Coordinates2(x1, y1, z1, w1)).toList
}

case class Experiment2( cubes : Map[(Int, Int, Int, Int), Cube] ) {
  def setCube(coordinates : Coordinates2, cube : Cube) : Experiment2 = {
    import coordinates._
    Experiment2(cubes + ((w, z, y, x) -> cube))
  }

  def getCube(coordinates: Coordinates2) : Cube = {
    import coordinates._
    cubes(w, z, y, x)
  }
  def getMinsAndMaxes : (Coordinates2, Coordinates2) = {
    val ws = cubes.keys.map(_._1)
    val zs = cubes.keys.map(_._2)
    val ys = cubes.keys.map(_._3)
    val xs = cubes.keys.map(_._4)
    (Coordinates2( xs.min, ys.min, zs.min,ws.min),
    Coordinates2(xs.max, ys.max, zs.max,ws.max))
  }
  def getSurroundingActive(coordinates: Coordinates2) : Int = {
    coordinates.getSurrounds.map(getCube).count(_ == Active)
  }

  def checkCube(coordinates: Coordinates2) : Option[Cube] = {
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

  def generation : Experiment2 = {
    val (mins, maxes) = getMinsAndMaxes
    val cubesList = (for {
      x <- mins.x -1 to maxes.x + 1
      y <- mins.y -1 to maxes.y + 1
      z <- mins.z -1 to maxes.z + 1
      w <- mins.w -1 to maxes.w + 1
    } yield (w, z, y, x)).toList
    val cubeChanges = cubesList.flatMap(coords => checkCube(Coordinates2(coords._4, coords._3, coords._2, coords._1)).map(cube => (coords , cube))).toMap.withDefaultValue(Inactive)
    Experiment2(cubes ++ cubeChanges)
  }
}

object Experiment2 {
  def fromInput(input : List[List[Char]]): Experiment2 = {
    val cubes = input.zipWithIndex.flatMap(
      line => line._1.zipWithIndex.map(
        entry => ((0, 0, line._2, entry._2), entry._1 match {
          case '#' => Active
          case '.' => Inactive
        })
     )
    ).toMap.withDefaultValue(Inactive)
    Experiment2(cubes)
  }
}

object Day17pt2 extends App {

  val fileName = "/home/zoe/Work/Repositories/AdventOfCode/src/main/resources/Day17TestData"
  val input = scala.io.Source.fromFile(fileName).getLines().toList.map(_.toList)

  def findAnswer2(input: List[List[Char]]) = {
    (1 to 6).foldLeft(Experiment2.fromInput(input))((experiment, _) => {
      println("Exp")
      println(experiment)
      experiment.generation
    }).cubes.count(_._2 == Active)
  }


  println(findAnswer2(input))
 // println(findAnswer2(input))
}

