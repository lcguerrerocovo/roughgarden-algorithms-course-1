def mergeSort(arr: Array[Int]): Array[Int] = {
  def merge(first: Array[Int], second: Array[Int]): Array[Int] = {
    def traverse(i: Int, j: Int) : List[Int] = {
      if(i >= first.length)
        second.slice(j,second.length).toList
      else if(j >= second.length)
       first.slice(i,first.length).toList
      else if(first(i) < second(j))
        first(i) :: traverse(i+1,j)
      else second(j) :: traverse(i,j+1)
    }
    traverse(0,0).toArray
  }
  if(arr.length == 1) arr
  else {
    val half = arr.length / 2
    val first = arr.slice(0, half)
    val second = arr.slice(half, arr.length)
    merge(mergeSort(first), mergeSort(second))
  }
}

mergeSort(Array(64,25,12,22,11))
mergeSort(Array(64,25,12,22))

