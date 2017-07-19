import org.scalatest.{FlatSpec, Matchers}

class S99ListsTest extends FlatSpec with Matchers {

  "P01" should "find the last element of a list" in {
    P01.last(List(1, 1, 2, 3, 5, 8)) should equal (8)
  }


}