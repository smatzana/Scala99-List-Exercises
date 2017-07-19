import org.scalatest.junit.JUnitRunner
import org.scalatest.{FlatSpec, Matchers}
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
abstract class S99UnitTestBase extends FlatSpec with Matchers

class S99P01Test extends S99UnitTestBase {

  "P01" should "find the last element of a list" in {
    P01.last(List(1, 1, 2, 3, 5, 8)) should be(8)
  }

  it should "find the last element of a single element list" in {
    P01.last(List(5)) should be(5)
  }

  it should "throw NoSuchElementException if called on an empty list" in {
    an[NoSuchElementException] should be thrownBy P01.last(List())
  }

  it should "throw NoSuchElementException if called on an Nil list" in {
    an[NoSuchElementException] should be thrownBy P01.last(Nil)
  }

}

class S99P02Test extends S99UnitTestBase {

  "P02" should "find the penultimate element of a list" in {
    P02.penultimate(List(1, 1, 2, 3, 5, 8)) should be(5)
  }

  it should "find the penultimate element of a list with two elements" in {
    P02.penultimate(List(1, 2)) should be(1)
  }

  it should "throw NoSuchElementException if called on a single element list" in {
    an[NoSuchElementException] should be thrownBy P02.penultimate(List(1))
  }

  it should "throw NoSuchElementException if called on an empty list" in {
    an[NoSuchElementException] should be thrownBy P02.penultimate(List())
  }

  it should "throw NoSuchElementException if called on an Nil list" in {
    an[NoSuchElementException] should be thrownBy P02.penultimate(List(1))
  }

}

class S99P03Test extends S99UnitTestBase {

  "P03" should "find the 3rd element of a list" in {
    P03.nth(2, List(1, 1, 2, 3, 5, 8)) should be (2)
  }

  it should "find the first element of a list" in {
    P03.nth(0, List(10, 1, 2, 3, 5, 8)) should be (10)
  }

  it should "throw NoSuchElementException if called with an out of range index" in {
    an[NoSuchElementException] should be thrownBy P03.nth(6, List(1, 1, 2, 3, 5, 8))
  }

  it should "throw NoSuchElementException if called on an empty list" in {
    an[NoSuchElementException] should be thrownBy P03.nth(0, List())
  }

  it should "throw NoSuchElementException if called on an Nil list" in {
    an[NoSuchElementException] should be thrownBy P03.nth(0, Nil)
  }

}

class S99P04Test extends S99UnitTestBase {

  "P04" should "find the number of elements of a list" in {
    P04.length(List(1, 1, 2, 3, 5, 8)) should equal (6)
  }

  it should "return 0 on an empty list" in {
    P04.length(List()) should equal (0)
  }

  it should "return 0 on an Nil list" in {
    P04.length(Nil) should equal (0)
  }

}

class S99P05Test extends S99UnitTestBase {

  "P05" should "reverse a list" in {
    P05.reverse(List(1, 1, 2, 3, 5, 8)) should be (List(8, 5, 3, 2, 1, 1))
  }

  it should "reverse a single element list" in {
    P05.reverse(List(1)) should be (List(1))
  }

  it should "reverse an empty list" in {
    P05.reverse(List()) should be (List())
  }

  it should "reverse a Nil list" in {
    P05.reverse(Nil) should be (Nil)
  }
}