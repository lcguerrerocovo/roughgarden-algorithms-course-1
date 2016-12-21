// run this file as a script like so:
// scala inversions-merge-homework.scala path/to/week2/assignment

import scala.io.Source

// copied over almost verbatim from merge-sort.sc
def inversions(arr: Array[Int]): (Long,Array[Int]) = {

  def merge(first: (Long,Array[Int]), second: (Long,Array[Int])): (Long,Array[Int]) = {
    val (firstInversions,firstArr) = first
    val (secondInversions,secondArr) = second

    def traverse(i: Int, j: Int, countInversions: Long, acc: List[Int]) : (Long,List[Int]) = {
      if(i >= firstArr.length)
        (countInversions,(acc.reverse ::: secondArr.slice(j,secondArr.length).toList))
      else if(j >= secondArr.length)
        (countInversions,(acc.reverse ::: firstArr.slice(i,firstArr.length).toList))
      else if(firstArr(i) < secondArr(j)) {
        traverse(i + 1, j, countInversions, firstArr(i) :: acc)
      }
      else {
        traverse(i,j+1,
          (countInversions + (firstArr.length)-i),secondArr(j) :: acc)
      }
    }
    val res = traverse(0,0,firstInversions+secondInversions, Nil)
    (res._1,res._2.toArray)
  }
  if(arr.length == 1) (0,arr)
  else {
    val half = arr.length / 2
    val first = arr.slice(0, half)
    val second = arr.slice(half, arr.length)
    merge(inversions(first), inversions(second))
  }
}

def readFromFile(filename: String): Array[Int] = {
  val lines = Source.fromFile(filename).getLines().toList
  lines.map(_.toInt).toArray
}

assert(inversions(Array(1,3,5,2,4,6))._1 == 3)
assert(inversions(Array(9, 12, 3, 1, 6, 8, 2, 5, 14, 13, 11, 7, 10, 4, 0))._1 == 56)
assert(inversions(Array(4, 80, 70, 23, 9, 60, 68, 27, 66, 78, 12, 40, 52, 53, 44, 8, 49,
  28, 18, 46, 21, 39, 51, 7, 87, 99, 69, 62, 84, 6, 79, 67, 14, 98, 83, 0, 96, 5, 82, 10,
  26, 48, 3, 2, 15, 92, 11, 55, 63, 97, 43, 45, 81, 42, 95, 20, 25, 74, 24, 72, 91, 35,
  86, 19, 75, 58, 71, 47, 76, 59, 64, 93, 17, 50, 56, 94, 90, 89, 32, 37, 34, 65, 1, 73,
  41, 36, 57, 77, 30, 22, 13, 29, 38, 16, 88, 61, 31, 85, 33, 54))._1 == 2372)
assert(inversions(readFromFile(args(0)))._1 == 2407905288L)
