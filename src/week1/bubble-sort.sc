/*****
  * https://en.wikipedia.org/wiki/Bubble_sort
 */
def bubbleSort(arr: Array[Int]) : Array[Int] = {
  def swap(arr: Array[Int], index: Int, length: Int): Array[Int] = {
    println("swap:" + (arr mkString ","))
    if (index >= length - 1) arr
    else {
      if (arr(index) > arr(index + 1)) {
        val temp = arr(index + 1)
        arr.update(index + 1, arr(index))
        arr.update(index, temp)
        arr
      }
      swap(arr, index + 1, length)
    }
  }
  def bsort(arr: Array[Int], sorted_size: Int): Array[Int] = {
    println("bsort:" + (arr mkString ","))
    if (sorted_size >= arr.length) arr
    else bsort(swap(arr,0,arr.length - sorted_size),sorted_size+1)
  }
  bsort(arr,0)
}
bubbleSort(Array(64,25,12,22,11))

