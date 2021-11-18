object FuzzySet {
  class Universe[T](val values: Set[T])
}

class FuzzySet[T](m: T => Double) {
  import FuzzySet.Universe
  import scala.math.{max, min}

  def isEmpty(implicit universe: Universe[T]): Boolean =
    universe.values.forall(m(_) == 0.0)

  def equalTo(that: FuzzySet[T])(implicit universe: Universe[T]): Boolean =
    universe.values.forall(x => contains(x) == that.contains(x))

  def contains(value: T): Double = m(value)

  def union(that: FuzzySet[T]): FuzzySet[T] = new FuzzySet[T]({
    x => max(contains(x), that.contains(x))
  })

  def intersect(that: FuzzySet[T]): FuzzySet[T] = new FuzzySet[T]({
    x => min(contains(x), that.contains(x))
  })

  def complement(implicit universe: Universe[T]): FuzzySet[T] = new FuzzySet[T]({
      x =>
        if (universe.values.contains(x)) 1 - contains(x)
        else 0.0
  })
}

object FuzzySetApp extends App {
  import FuzzySet.Universe

  implicit val fuzzySetUniverse: Universe[Int] = new Universe(Set.from(1 to 10))

  val emptyFuzzySet = new FuzzySet[Int](_ => 0.0)

  val someNonEmptyFuzzySet = new FuzzySet[Int]({
    case 1 => 0.5
    case 2 => 0.75
    case 3 => 1
    case _ => 0.0
  })

//  val someNonEmptyFuzzySet2 = new FuzzySet[Int]({
//    case 1 => 0.3
//    case 2 => 0.8
//    case 3 => 0.9
//    case _ => 0.1
//  })
//
//  val someNonEmptyFuzzySet3 = new FuzzySet[Int]({
//    case 1 => 0.5
//    case 2 => 0.75
//    case 3 => 1
//    case 4 => 0.0
//    case _ => 0.0
//  })
}