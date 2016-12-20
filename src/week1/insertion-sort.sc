
/*******
  * https://en.wikipedia.org/wiki/Insertion_sort
  *
 */

def isort(xs: List[Int]): List[Int] = xs match {
  case List() => List()
  case y :: ys => insert(y,isort(ys))
}

def insert(x: Int, xs: List[Int]): List[Int] = xs match {
  case List() => x :: Nil
  case y :: ys => {
    if (x > y) y :: insert(x, ys)
    else x :: insert(y, ys)
  }
}

isort(6 :: 3 :: 2 :: 4 :: 1 :: Nil)
