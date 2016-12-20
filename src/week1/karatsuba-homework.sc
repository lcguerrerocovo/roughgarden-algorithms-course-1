def karatsuba(x: String, y: String): String = {

  def toBigIntegerList(x: String) = x.map(x => x.toInt - 48).toList

  def karatsuba(x: List[Int], y: List[Int]): List[Int] = {

    def sumWithCarry(first: List[Int], second: List[Int]): List[Int] = {
      def sumWithCarry(first: List[Int], second: List[Int], crr: Int): List[Int] = (first, second) match {
        case (List(), List()) => if (crr != 0) crr :: Nil else Nil
        case (x :: xs, y :: ys) => {
          if (x + y + crr < 10) (x + y + crr) :: sumWithCarry(xs, ys, 0)
          else (x + y + crr - 10) :: sumWithCarry(xs, ys, 1)
        }
        case (x :: xs, List()) => {
          if (x + crr < 10) {
            (x + crr) :: sumWithCarry(xs, List(), 0)
          } else (x + crr - 10) :: sumWithCarry(xs, List(), 1)
        }
        case (List(), y :: ys) => {
          if (y + crr < 10) {
            (y + crr) :: sumWithCarry(List(), ys, 0)
          } else (y + crr - 10) :: sumWithCarry(List(), ys, 1)
        }
      }
      sumWithCarry(first.reverse, second.reverse, 0).reverse
    }

    def shiftListExponential10(list: List[Int], z: Int) = {
      list ++ List.fill(z)(0)
    }

    def multiplyListWithScalar(list: List[Int], z: Int) = {
      list.zipWithIndex.map { case (x, y) =>
        toBigIntegerList((x * z * math.pow(10, list.length - y - 1)).toInt.toString)
      }.reduceLeft(sumWithCarry)
    }

    if (x.length == 1) {
      multiplyListWithScalar(y, x(0))
    } else if (y.length == 1) {
      multiplyListWithScalar(x, y(0))
    } else {
      val (a, b) = (x.slice(0, x.length / 2),
        x.slice(x.length / 2, x.length))
      val (c, d) = (y.slice(0, y.length / 2),
        y.slice(y.length / 2, x.length))

      val ac = karatsuba(a, c)
      val bd = karatsuba(b, d)
      val ad = karatsuba(a, d)
      val bc = karatsuba(b, c)

      sumWithCarry(shiftListExponential10(ac, x.length), sumWithCarry(shiftListExponential10(sumWithCarry(ad, bc), x.length / 2), bd))
    }
  }
  karatsuba(toBigIntegerList(x), toBigIntegerList(y)) mkString ""
}

assert(karatsuba("5678", "1234") == "7006652")
assert(karatsuba("3141592653589793238462643383279502884197169399375105820974944592",
  "2718281828459045235360287471352662497757247093699959574966967627") == "8539734222673567065463550869546574495034888535765114961879601127067743044893204848617875072216249073013374895871952806582723184")


