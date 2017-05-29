// Steps := 1, 2, 3
/*
List(1)
List(2)
List(3)
List(1, 1)
List(1, 2)
List(1, 3)
List(2, 1)
List(2, 2)
List(2, 3)
List(3, 1)
//...

3
9
27
// 3^length
*/



def expand[A](as: List[A], stream: Stream[List[A]]): Stream[List[A]] =
  stream match {
    case Stream() => Stream()
    case head #:: tail =>
      as.map(a => head ++ List(a)).toStream #::: expand(as, tail)
  }


//val x0 = expand(Stream(List(1), List(2), List(3)))

def combinations[A](as: List[A]): Stream[List[A]] = {
  def loop(stream: Stream[List[A]]): Stream[List[A]] = {
    val x1 = expand(as, stream)
    x1 #::: loop(x1)
  }
  loop(Stream(List()))
}

import bp._

val comb = combinations(List(FillSmall, FillLarge, EmptyLarge, EmptySmall, LargeInSmall, SmallInLarge))

val states = comb.map(c => (c, modify(State.empty, c)))


val solutions = {
  //states.takeWhile(_._1.length <= 6).filter(state => state._2.large == 10)

  1
}










/*
def all: Stream[List[Int]] = {
  def go(stream: Stream[List[Int]]): Stream[List[Int]] = {
    val res = expand(stream)
    res #::: go(res)
  }
  go(Stream(List.empty))
}

all.take(100).toList
*/





























