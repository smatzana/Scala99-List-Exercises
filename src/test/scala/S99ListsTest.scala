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

class S99P06Test extends S99UnitTestBase {

  "P06" should "find out whether a list is a palindrome" in {
    P06.isPalindrome(List(1, 2, 3, 2, 1)) should be (true)
  }

  it should "detect a non palindrome" in {
    P06.isPalindrome(List(1, 2, 2, 3, 2, 1)) should be (false)
  }

  it should "detect a palindrome in an empty List" in {
    P06.isPalindrome(List()) should be (true)
  }

  it should "detect a palindrome in an Nil List" in {
    P06.isPalindrome(Nil) should be (true)
  }
}

class S99P07Test extends S99UnitTestBase {

  "P07" should "flatten a nested list" in {
    P07.flatten(List(List(1, 1), 2, List(3, List(5, 8)))) should be (List(1, 1, 2, 3, 5, 8))
  }

  it should "flatten a very deeply nested list" in {
    P07.flatten(List(List(1, List(List(List(4, List(1, 3)), List(10, 9)), 2, List(3, List(5, 8)))))) should be (List(1, 4, 1, 3, 10, 9, 2, 3, 5, 8))
  }

  it should "leave a flat list as is" in {
    P07.flatten(List(1, 2, 2, 3, 2, 1)) should be (List(1, 2, 2, 3, 2, 1))
  }

  it should "leave an empty list as is" in {
    P07.flatten(List()) should be (List())
  }

  it should "leave a Nil list as is" in {
    P07.flatten(Nil) should be (Nil)
  }

}

class S99P08Test extends S99UnitTestBase {

  "P08" should "eliminate consecutive elements" in {
    P08.compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) should be (List('a, 'b, 'c, 'a, 'd, 'e))
  }

  it should "leave a list without consecutive elements as is" in {
    P08.compress(List('a, 'b, 'c, 'a, 'd, 'e)) should be (List('a, 'b, 'c, 'a, 'd, 'e))
  }

  it should "leave an empty list as is" in {
    P08.compress(List()) should be (List())
  }

  it should "leave a Nil list as is" in {
    P08.compress(Nil) should be (Nil)
  }

}