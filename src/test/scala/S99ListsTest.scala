import org.scalatest.junit.JUnitRunner
import org.scalatest.{FlatSpec, Matchers}
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
abstract class S99UnitTestBase extends FlatSpec with Matchers

class S99P01Test extends S99UnitTestBase {

  "P01" should "find the last element of a list" in {
    P01.last(List(1, 1, 2, 3, 5, 8)) should be (8)
  }

  "P01" should "find the last element of a single element list" in {
    P01.last(List(5)) should be (5)
  }

  it should "throw NoSuchElementException if called on an empty list" in {
    an [NoSuchElementException] should be thrownBy P01.last(List())
  }

  it should "throw NoSuchElementException if called on an Nil list" in {
    an [NoSuchElementException] should be thrownBy P01.last(Nil)
  }

}