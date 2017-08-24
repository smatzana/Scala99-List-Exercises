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

class S99P09Test extends S99UnitTestBase {

  "P09" should "pack consecutive duplicates of list elements into sub-lists" in {
    P09.pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) should be (List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e)))
  }

  it should "pack an empty list as is" in {
    P09.pack(List()) should be (List())
  }

  it should "leave a Nil list as is" in {
    P09.pack(Nil) should be (Nil)
  }
}

class S99P10Test extends S99UnitTestBase {

  "P10" should "run-encode a list" in {
    P10.encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) should be (List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e)))
  }

  it should "run-encode an empty list and leave as is" in {
    P10.encode(List()) should be (List())
  }
}

class S99P11Test extends S99UnitTestBase {

  "P11" should "run modified encoding" in {
    P11.encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) should be (List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e)))
  }

  it should "encode an empty list and leave as is" in {
    P11.encodeModified(List()) should be (List())
  }

}

class S99P12Test extends S99UnitTestBase {

  "P12" should "decode a run-length encoded list" in {
    P12.decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))) should be (List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
  }

  it should "decode a simple list" in {
    P12.decode(List((1, 'a))) should be (List('a))
  }

  it should "decode an empty list and leave as is" in {
    P11.encodeModified(List()) should be (List())
  }

  it should "deal with negative numbers" in {
    P12.decode(List((-1, 'a))) should be (List())
  }

  it should "deal with zero" in {
    P12.decode(List((0, 'a))) should be (List())
  }
}

class S99P13Test extends S99UnitTestBase {

  "P13" should "directly encode a list" in {
    P13.encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) should be (List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e)))
  }

  it should "rncode an empty list and leave as is" in {
    P13.encodeDirect(List()) should be (List())
  }
}

class S99P14Test extends S99UnitTestBase {

  "P14" should "duplicate the elements of a list" in {
    P14.duplicate(List('a, 'b, 'c, 'c, 'd)) should be (List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd))
  }

  it should "duplicate more" in {
    P14.duplicate(List('a)) should be (List('a, 'a))
  }

  it should "leave an empty list empty" in {
    P14.duplicate(List.empty) should be (List.empty)
  }

  it should "leave a Nil list empty" in {
    P14.duplicate(Nil) should be (Nil)
  }

}

class S99P15Test extends S99UnitTestBase {

  "P15" should "duplicate n times" in {
    P15.duplicateN(3, List('a, 'b, 'c, 'c, 'd)) should be(List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd))
  }

  it should "leave an empty list empty" in {
    P15.duplicateN(5, List.empty) should be(List.empty)
  }

  it should "leave a Nil list empty" in {
    P15.duplicateN(4, Nil) should be(Nil)
  }

  it should "ignore bad arguments" in {
    an[UnsupportedOperationException] should be thrownBy P15.duplicateN(-1, List('a, 'b, 'c, 'c, 'd))
  }
}

class S99P16Test extends S99UnitTestBase {

  "P16" should "drop every 3rd element" in {
    P16.drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) should be(List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k))
  }

  it should "leave an empty list empty" in {
    P16.drop(5, List.empty) should be(List.empty)
  }

  it should "leave a Nil list empty" in {
    P16.drop(4, Nil) should be(Nil)
  }

  it should "ignore bad arguments" in {
    an[UnsupportedOperationException] should be thrownBy P16.drop(-1, List('a, 'b, 'c, 'c, 'd))
  }
}

class S99P17Test extends S99UnitTestBase {

  "P17" should "split a list in two parts" in {
    P17.split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) should be((List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
  }

  it should "leave an empty list empty" in {
    P17.split(5, List.empty) should be((List.empty, List.empty))
  }

  it should "leave a Nil list empty" in {
    P17.split(4, Nil) should be((Nil, Nil))
  }

  it should "ignore bad arguments" in {
    an[UnsupportedOperationException] should be thrownBy P17.split(-1, List('a, 'b, 'c, 'c, 'd))
  }
}

class S99P18Test extends S99UnitTestBase {

  "P18" should "extract a slice from a List" in {
    P18.slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) should be (List('d, 'e, 'f, 'g))
  }

  it should "leave an empty list empty" in {
    P18.slice(1, 1, List.empty) should be(List.empty)
  }

  it should "leave a Nil list empty" in {
    P18.slice(1, 1, Nil) should be(Nil)
  }

  it should "ignore bad arguments" in {
    an[UnsupportedOperationException] should be thrownBy P18.slice(-1, 2, List('a, 'b, 'c, 'c, 'd))
  }
}

class S99P19Test extends S99UnitTestBase {

  "P19" should "rotate a list 3 places to the left" in {
    P19.rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) should be(List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c))
  }

  it should "leave an empty list empty" in {
    P19.rotate(4, List.empty) should be(List.empty)
  }

  it should "leave a Nil list empty" in {
    P19.rotate(11, Nil) should be(Nil)
  }

  it should "honor negative arguments" in {
    P19.rotate(-1, List('a, 'b, 'c, 'c, 'd)) should be(List('d, 'a, 'b, 'c, 'c))
  }

}

class S99P20Test extends S99UnitTestBase {

  "P20" should "remove the second element of a list" in {
    P20.removeAt(1, List('a, 'b, 'c, 'd)) should be((List('a, 'c, 'd), 'b))
  }

  "P20" should "remove the fourth element of a list" in {
    P20.removeAt(3, List('a, 'b, 'c, 'd)) should be((List('a, 'b, 'c), 'd))
  }

  it should "refuse to split an empty list" in {
    an[UnsupportedOperationException] should be thrownBy P20.removeAt(4, List.empty)
  }

}