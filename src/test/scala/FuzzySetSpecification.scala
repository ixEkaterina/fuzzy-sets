import FuzzySet.Universe
import org.scalacheck.Prop.{exception, forAll}
import org.scalacheck.{Arbitrary, Gen, Properties}

// https://github.com/typelevel/scalacheck/blob/main/doc/UserGuide.md
object FuzzySetSpecification extends Properties("FuzzySet") {

  import ArbitraryFuzzySet._

  property("isEmpty") = forAll { a: ArbitraryFuzzySet[Int] =>
    !a.fuzzySet.isEmpty(a.universe)
  }

  property("contains") = forAll { a: ArbitraryFuzzySetMy[Int] =>
    a.universe.values.forall( v => a.fuzzySet.contains(v) == a.mapGrades.getOrElse(v, 0.0))
  }

  property("equalTo") = forAll { a: ArbitraryFuzzySet[Int] =>
    a.fuzzySet.equalTo(a.fuzzySet)(a.universe)
  }

  property("union") = forAll { (a: ArbitraryFuzzySet[Int], b: ArbitraryFuzzySet[Int]) =>
    val f = a.fuzzySet.union(b.fuzzySet)
    ( a.universe.values ++ b.universe.values ).forall{x => scala.math.max(a.fuzzySet.contains(x), b.fuzzySet.contains(x)) == f.contains(x) }
  }

  property("intersect") = forAll { (a: ArbitraryFuzzySet[Int], b: ArbitraryFuzzySet[Int]) =>
    val f = a.fuzzySet.intersect(b.fuzzySet)
    ( a.universe.values ++ b.universe.values ).forall{x => scala.math.min(a.fuzzySet.contains(x), b.fuzzySet.contains(x)) == f.contains(x) }
  }

  property("complement") = forAll { a: ArbitraryFuzzySet[Int] =>
    val f = a.fuzzySet.complement(a.universe).complement(a.universe)
    a.fuzzySet.equalTo(f)(a.universe)
  }

}

case class ArbitraryFuzzySet[T](fuzzySet: FuzzySet[T], universe: Universe[T])
case class ArbitraryFuzzySetMy[T](fuzzySet: FuzzySet[T], universe: Universe[T], mapGrades: Map[T, Double])

object ArbitraryFuzzySet {

  implicit def arbitraryNonEmptyUniverse[T](implicit a: Arbitrary[T]): Arbitrary[Universe[T]] =
    Arbitrary {
      for {
        values <- Gen.nonEmptyContainerOf[Set, T](a.arbitrary)
      } yield new Universe(values)
    }

  implicit def arbitraryNonEmptyFuzzySet[T](implicit a: Arbitrary[Universe[T]]): Arbitrary[ArbitraryFuzzySet[T]] =
    Arbitrary {
      for {
        universe <- a.arbitrary
        values <- Gen.nonEmptyContainerOf[List, T](Gen.oneOf(universe.values))
        grades <- Gen.containerOfN[List, Double](values.size, Gen.choose(0.0, 1.0) suchThat (v => v != 0.0))
      } yield {
        val fuzzySet = new FuzzySet[T]({ v: T =>
          val index = values.indexOf(v)
          if (index < 0) 0.0 else grades(index)
        })

        ArbitraryFuzzySet(fuzzySet, universe)
      }
    }

  implicit def arbitraryNonEmptyFuzzySetMy[T](implicit a: Arbitrary[Universe[T]]): Arbitrary[ArbitraryFuzzySetMy[T]] =
    Arbitrary {
      for {
        universe <- a.arbitrary
        values <- Gen.nonEmptyContainerOf[List, T](Gen.oneOf(universe.values))
        grades <- Gen.containerOfN[List, Double](values.size, Gen.choose(0.0, 1.0) suchThat (v => v != 0.0))
      } yield {
        val fuzzySet = new FuzzySet[T]({ v: T =>
          val index = values.indexOf(v)
          if (index < 0) 0.0 else grades(index)
        })
         
        val map = values.map( x => (x, grades(values.indexOf(x)))).toMap

        ArbitraryFuzzySetMy(fuzzySet, universe, map)
      }
    }
}