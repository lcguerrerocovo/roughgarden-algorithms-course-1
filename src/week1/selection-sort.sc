/***
  *
  * https://en.wikipedia.org/wiki/Selection_sort
  * The way selection sort works is that you find the
  * minimum value of the array and swap it with
  * the first position and recursively advance along
  * the array until you have sorted the whole array
  * with recursive calls to find the next minimum
  *
  * the running time complexity is O(n^2) since finding
  * the minimum means traversing the whole array of n
  * values and you need to do that n times until the
  * whole array is sorted
  *
  * although this scala implementations is recursive it
  * uses recursive functions to find indices to swap
  * and the array is sorted in place
  */

def selectionSort(arr: Array[Int]) : Array[Int] = {
  def minimum(arr: Array[Int], index: Int, minIndex: Int) : Int = {
     if(index+1 > arr.length) minIndex
     else if(arr(index) < arr(minIndex)) minimum(arr,index+1,index)
     else minimum(arr,index+1,minIndex)
  }
  def swap(arr: Array[Int], index: Int) : Array[Int] = {
    println(arr mkString ",")
    if(index+1 > arr.length) arr
    else {
      val minIndex = minimum(arr, index, index)
      val indexValue = arr(index)
      arr.update(index, arr(minIndex))
      arr.update(minIndex, indexValue)
      swap(arr, index + 1)
    }
  }
  swap(arr,0)
}
selectionSort(Array(64,25,12,22,11))