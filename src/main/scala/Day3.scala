import scala.annotation.tailrec

case class TreeLine(trees: Set[Int], maxSize: Int) {
  def treeAt(position: Int) = trees.contains(position % maxSize)
}

object TreeLine {
  def fromString(inputString: String) : TreeLine = {
    TreeLine(inputString.toList.zipWithIndex.filter(_._1 == '#').map(_._2).toSet, inputString.size)
  }
}

case class MountainSide(treeLines : List[TreeLine]){
  def treeCount(forward : Int, down: Int): Long = {
      @tailrec
      def treeCountInternal(currentX: Int, currentCount: Long, treeLinesRemaining: List[TreeLine]) : Long = {
        treeLinesRemaining match {
          case Nil => currentCount
          case treeLine :: _ => treeCountInternal(currentX + forward, if (treeLine.treeAt(currentX)) currentCount + 1L else currentCount, treeLinesRemaining.drop(down))
        }
      }

    treeCountInternal(0,0L, treeLines)
  }
}

object MountainSide {
  def fromStrings(inputStrings : List[String]) = MountainSide(inputStrings.map(TreeLine.fromString))
}

object Day3 extends App {

 def findAnswer1(mountain: MountainSide) = mountain.treeCount(3,1)

  def findAnswer2(mountain: MountainSide) = {
    mountain.treeCount(1, 1) * mountain.treeCount(3, 1) *
    mountain.treeCount(5, 1) * mountain.treeCount(7, 1) * mountain.treeCount(1, 2)
  }

  val fileName = "/home/zoe/Work/Repositories/AdventOfCode/src/main/resources/Day3TestData"
  val mountainSide = MountainSide.fromStrings(scala.io.Source.fromFile(fileName).getLines().toList)
  println(findAnswer1(mountainSide))

  println(findAnswer2(mountainSide))
}
