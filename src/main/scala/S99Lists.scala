import scala.annotation.tailrec

object P01 {
  /*
    P01 (*) Find the last element of a list.
    Example:
    scala> last(List(1, 1, 2, 3, 5, 8))
    res0: Int = 8
  */
  @tailrec def last[T](aList: List[T]) :T =
    aList match {
      case h :: Nil => h
      case _ :: tail => last(tail)
      case Nil => throw new NoSuchElementException
    }

}

object P02 {
  /*
    P02 (*) Find the last but one element of a list.
    Example:
    scala> penultimate(List(1, 1, 2, 3, 5, 8))
    res0: Int = 5
   */
  @tailrec def penultimate[T](aList: List[T]) : T =
    aList match {
      case h :: List(_) => h
      case _ :: tail => penultimate(tail)
      case _ =>  throw new NoSuchElementException
    }
}

object P03 {
  /*
    P03 (*) Find the N-th element of a list.
    By convention, the first element in the list is element 0.
    Example:

    scala> nth(2, List(1, 1, 2, 3, 5, 8))
    res0: Int = 2
   */

  def nth[T](n: Int, aList: List[T]) : T =
    aList match {
      case h :: _ if n == 0 => h
      case _ :: tail => nth(n-1, tail)
      case Nil => throw new NoSuchElementException

    }
}


object P04 {
  /*
    P04 (*) Find the number of elements of a list.
    Example:
    scala> length(List(1, 1, 2, 3, 5, 8))
    res0: Int = 6
  */

  def length[T](aList: List[T]) : Int = {
    @tailrec def helper(acc: Int, aList: List[T]) : Int =
      aList match {
        case _ :: tail => helper(acc+1, tail)
        case Nil => acc
      }

    helper(0, aList)
  }

}

object P05 {
  /*
    P05 (*) Reverse a list.
    Example:
    scala> reverse(List(1, 1, 2, 3, 5, 8))
    res0: List[Int] = List(8, 5, 3, 2, 1, 1)
  */

  def reverse[T](aList: List[T]) : List[T] = {

    @tailrec def reverseHelper(accumulator: List[T], current: List[T]): List[T] = {
      current match {
        case h :: tail => reverseHelper(h :: accumulator, tail)
        case Nil => accumulator
      }
    }

    reverseHelper(Nil, aList)
  }
}

object P06 {
  /*
    P06 (*) Find out whether a list is a palindrome.
    Example:
    scala> isPalindrome(List(1, 2, 3, 2, 1))
    res0: Boolean = true
   */

  def isPalindrome[T](aList: List[T]) = P05.reverse(aList) == aList

}

object P07 {
  /*
    P07 (**) Flatten a nested list structure.
    Example:
    scala> flatten(List(List(1, 1), 2, List(3, List(5, 8))))
    res0: List[Any] = List(1, 1, 2, 3, 5, 8)
   */

  def flatten(aList: List[Any]): List[Any] = {
    def helper(element: Any) : List[Any] =
      element match {
        case h :: tail => helper(h) ::: helper(tail)
        case Nil => Nil
        case _ => List(element)
      }

    helper(aList)
  }

  def flattenWithFlatMap(aList: List[Any]): List[Any] = {

    aList flatMap {
      case someList: List[_] => flattenWithFlatMap(someList)
      case element => List(element)
    }
  }

}

object P08 {
  /*
    P08 (**) Eliminate consecutive duplicates of list elements.
    If a list contains repeated elements they should be replaced with a single copy of the element. The order of the elements should not be changed.
    Example:

    scala> compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
    res0: List[Symbol] = List('a, 'b, 'c, 'a, 'd, 'e)

  */
  
  def compress[T](aList: List[T]) : List[T] = {

    @tailrec def helper(accumulator: List[T], aList : List[T]) : List[T] =
      aList match {
        case h :: Nil => h :: accumulator
        case h :: tail if h == tail.head => helper(accumulator, tail)
        case h :: tail => helper(h :: accumulator, tail)
        case e => e
      }

    helper(Nil, aList).reverse
  }

}

object P09 {
  /*
    P09 (**) Pack consecutive duplicates of list elements into sublists.
    If a list contains repeated elements they should be placed in separate sublists.
    Example:

    scala> pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
    res0: List[List[Symbol]] = List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e))
   */

  def pack[T](aList: List[T]) : List[List[T]] =
    aList.foldRight(List[List[T]](List())){
      (t, acc) =>
        acc.head match {
          case h :: _ if h == t => (t :: acc.head) :: acc.tail
          case _ :: _ => List(t) :: acc
          case Nil => List(t) :: Nil
        }
  }
}

object P10 {
  /*
    P10 (*) Run-length encoding of a list.
    Use the result of problem P09 to implement the so-called run-length encoding data compression method. Consecutive duplicates of elements are encoded as tuples (N, E) where N is the number of duplicates of the element E.
    Example:

    scala> encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
    res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
   */

  def encode[T](aList: List[T]) : List[(Int, T)] =
    P09.pack(aList).map( list => (list.length, list.head))
}

object P11 {
  /*
    P11 (*) Modified run-length encoding.
    Modify the result of problem P10 in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as (N, E) terms.
    Example:

    scala> encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
    res0: List[Any] = List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e))
  */
  def encodeModified[T](aList: List[T]) : List[Any] =
    P09.pack(aList).map( list => list.length match {
      case 1 => list.head
      case _ =>  (list.length, list.head)
    })

}

object P12 {
  /*
    P12 (**) Decode a run-length encoded list.
    Given a run-length code list generated as specified in problem P10, construct its uncompressed version.
    Example:

    scala> decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))
    res0: List[Symbol] = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
   */
  def decode[T](aList: List[(Int, T)]) : List[T] =
    aList.flatMap( encodedList => for { i <- 1 to encodedList._1 } yield encodedList._2 )

}

object P13 {
  /*
    P13 (**) Run-length encoding of a list (direct solution).
    Implement the so-called run-length encoding data compression method directly. I.e. don't use other methods you've written (like P09's pack); do all the work directly.
    Example:

    scala> encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
    res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
   */

  def encodeDirect[T](aList: List[T]) : List[(Int, T)] =
    aList.foldRight(List[(Int, T)]()){
      (t, acc) => {
        acc match {
          case h :: _ if h._2 == t => (h._1+1, h._2) :: acc.tail
          case _ :: _ => (1,t) :: acc
          case Nil => (1,t)  :: Nil
        }
      }
    }
}

object P14 {
  /*
    P14 (*) Duplicate the elements of a list.
    Example:
    scala> duplicate(List('a, 'b, 'c, 'c, 'd))
    res0: List[Symbol] = List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd)
   */

  def duplicate[T](aList: List[T]) : List[T] =
    aList.flatMap( e => List(e,e) )
}

object P15 {
  /*
    P15 (**) Duplicate the elements of a list a given number of times.
    Example:
    scala> duplicateN(3, List('a, 'b, 'c, 'c, 'd))
    res0: List[Symbol] = List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd)
   */

  def duplicateN[T](n: Int, aList: List[T]) : List[T] = {
    if (n <= 0) throw new UnsupportedOperationException
    aList.flatMap(e => for (i <- 1 to n) yield e)
  }
}

object P16 {
  /*
    P16 (**) Drop every Nth element from a list.
    Example:
    scala> drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
    res0: List[Symbol] = List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k)
   */
  def drop[T](n: Int, aList: List[T]) : List[T] = {

    @tailrec def helper(accumulator: List[T], currentList: List[T], i: Int) : List[T] = {
      currentList match {
        case _ :: tail if i == 1 => helper(accumulator, tail, n)
        case h :: tail => helper(h :: accumulator, tail, i - 1)
        case Nil => accumulator
      }
    }

    if (n <= 0) throw new UnsupportedOperationException
    helper(List(), aList, n).reverse
  }
}

// TODO: maybe handle negatives too (as in from the tail of list)
object P17 {
  /*
    P17 (*) Split a list into two parts.
    The length of the first part is given. Use a Tuple for your result.
    Example:

    scala> split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
    res0: (List[Symbol], List[Symbol]) = (List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
   */
  def split[T](n: Int, aList: List[T]) : (List[T], List[T]) = {
    @tailrec def helper(i: Int, currentList: List[T], accumulator: List[T]) : (List[T], List[T]) = {
      currentList match {
        case _ :: _ if (i == 0 ) => (accumulator.reverse, currentList)
        case h :: t => helper(i - 1, t, h :: accumulator)
        case Nil => (Nil, Nil)
      }
    }
    if (n < 0) throw new UnsupportedOperationException
    helper(n, aList, Nil)
  }

  def splitWithBuiltInFunctions[T](n: Int, aList: List[T]) : (List[T], List[T]) =
    (aList.take(n), aList.drop(n))

}

object P18 {
  /*
    P18 (**) Extract a slice from a list.
    Given two indices, I and K, the slice is the list containing the elements from and including the Ith element up to but not including the Kth element of the original list. Start counting the elements with 0.
    Example:

    scala> slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
    res0: List[Symbol] = List('d, 'e, 'f, 'g)
   */
  def slice[T](from: Int, to: Int, aList: List[T]): List[T] =
    P17.split(to - from, P17.split(from - 1, aList)._2)._1

}


object P19 {
  /*
    P19 (**) Rotate a list N places to the left.
    Examples:
    scala> rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
    res0: List[Symbol] = List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c)

    scala> rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
    res1: List[Symbol] = List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i)

   */

  def rotate[T](n: Int, aList: List[T]) : List[T] = {
    val splitResult = P17.split((aList.length + n ) % aList.length, aList)
    splitResult._2 ::: splitResult._1
  }
}

object P20 {
  /*
    P20 (*) Remove the Kth element from a list.
    Return the list and the removed element in a Tuple. Elements are numbered from 0.
    Example:

    scala> removeAt(1, List('a, 'b, 'c, 'd))
    res0: (List[Symbol], Symbol) = (List('a, 'c, 'd),'b)
   */
  def removeAt[T](n: Int, aList: List[T]) : (List[T], T) = {
    @tailrec def helper(accumulator: List[T], currentList: List[T], n: Int): (List[T], T)  = {
      (n, currentList) match {
        case (0, h :: t) =>  (accumulator.reverse ::: t, h)
        case (_, h :: t) => helper(h :: accumulator, t, n - 1)
        case (_, Nil) => throw new NoSuchElementException
      }
    }
    helper(Nil, aList, Math.max(n, (aList.length + n) % aList.length))
  }
}

object P21 {
  /*
    P21 (*) Insert an element at a given position into a list.
    Example:
    scala> insertAt('new, 1, List('a, 'b, 'c, 'd))
    res0: List[Symbol] = List('a, 'new, 'b, 'c, 'd)
   */
  def insertAt[T](element: T, n: Int, aList: List[T]) = {
    val splitResult = P17.split((aList.length + n ) % aList.length, aList)
    splitResult._1 ::: element :: splitResult._2
  }
}

import scala.language.postfixOps

object P22 {
  /*
    P22 (*) Create a list containing all integers within a given range.
    Example:
    scala> range(4, 9)
    res0: List[Int] = List(4, 5, 6, 7, 8, 9)
   */
  def range(from: Int, to: Int) : List[Int] =
    (for (i <- from to to) yield i) toList

}

object P23 {

  /*
    P23 (**) Extract a given number of randomly selected elements from a list.
    Example:
    scala> randomSelect(3, List('a, 'b, 'c, 'd, 'f, 'g, 'h))
    res0: List[Symbol] = List('e, 'd, 'a)
   */
  def randomSelect[T](n: Int, aList: List[T]) : List[T] = {

    @tailrec def helper(accumulator: List[T], currentList: List[T], i: Int): List[T] = {
      i match {
        case 0 => accumulator
        case n if n < 0 => throw new UnsupportedOperationException
        case _ => {
          val (newList, e) = P20.removeAt( ((Math.random()*100 * currentList.length)/100).toInt, currentList)
          helper(e :: accumulator, newList, i - 1)
        }
      }
    }

    helper(Nil, aList, n)
  }
    
}

object P24 {
  /*
    P24 (*) Lotto: Draw N different random numbers from the set 1..M.
    Example:
    scala> lotto(6, 49)
    res0: List[Int] = List(23, 1, 17, 33, 21, 37)
   */
  def lotto(n: Int, from: Int): List[Int] =
    P23.randomSelect(n, (for (i <- 1 to from) yield i).toList )

}

object P25 {
  /*
    P25 (*) Generate a random permutation of the elements of a list.
    Hint: Use the solution of problem P23.
    Example:

    scala> randomPermute(List('a, 'b, 'c, 'd, 'e, 'f))
    res0: List[Symbol] = List('b, 'a, 'd, 'c, 'e, 'f)
   */
  def randomPermute[T](aList: List[T]) = P23.randomSelect(aList.length, aList)

}

object P26 {
  /*
    P26 (**) Generate the combinations of K distinct objects chosen from the N elements of a list.
    In how many ways can a committee of 3 be chosen from a group of 12 people? We all know that
    there are C(12,3) = 220 possibilities (C(N,K) denotes the well-known binomial coefficient).
    For pure mathematicians, this result may be great. But we want to really generate all the possibilities.
    Example:

    scala> combinations(3, List('a, 'b, 'c, 'd, 'e, 'f))
    res0: List[List[Symbol]] = List(List('a, 'b, 'c), List('a, 'b, 'd), List('a, 'b, 'e), ...

   */

  def combinations[T](n: Int, aList: List[T]) : List[List[T]] = {
    n match {
      case 0 => Nil
      case 1 => for (e <- aList) yield List(e)
      case _ =>
        (for {
          i <- 1 to aList.length
          (s1, s2) = aList.splitAt(i)
          head = s1.last
          partialResults <- combinations(n - 1, s2)
        } yield head :: partialResults) toList
    }

  }

}

object P27 {
  /*
    P27 (**) Group the elements of a set into disjoint subsets.
    a) In how many ways can a group of 9 people work in 3 disjoint subgroups of 2, 3 and 4 persons?
    Write a function that generates all the possibilities.
    Example:

    scala> group3(List("Aldo", "Beat", "Carla", "David", "Evi", "Flip", "Gary", "Hugo", "Ida"))
    res0: List[List[List[String]]] = List(List(List(Aldo, Beat), List(Carla, David, Evi), List(Flip, Gary, Hugo, Ida)), ...

    b) Generalize the above predicate in a way that we can specify a list of group sizes and the predicate will
    return a list of groups.

    Example:

    scala> group(List(2, 2, 5), List("Aldo", "Beat", "Carla", "David", "Evi", "Flip", "Gary", "Hugo", "Ida"))
    res0: List[List[List[String]]] = List(List(List(Aldo, Beat), List(Carla, David), List(Evi, Flip, Gary, Hugo, Ida)), ...
    Note that we do not want permutations of the group members; i.e. ((Aldo, Beat), ...) is the same solution as
     ((Beat, Aldo), ...).
     However, we make a difference between ((Aldo, Beat), (Carla, David), ...) and ((Carla, David), (Aldo, Beat), ...).

    You may find more about this combinatorial problem in a good book on discrete mathematics under the term "multinomial coefficients".
   */

  def minus[T](a: List[T], b: List[T]) =
    a.filterNot( b.contains(_))

  def group3[T](aList: List[T]) : List[List[List[T]]] = {

    for {
      combi2: List[T] <- P26.combinations[T](2, aList);
      combi3: List[T] <- P26.combinations[T](3, minus(aList, combi2) )
      combi4: List[T] <- P26.combinations[T](4, minus(minus(aList, combi2), combi3))

    } yield List(combi2, combi3, combi4)
  }

  def group[T](groups: List[Int], aList: List[T]) : List[List[List[T]]] = {
    groups match {
      case Nil => Nil
      case h :: Nil => List(P26.combinations(h, aList))
      case _ =>
        for {
          first: List[T] <- P26.combinations(groups.head, aList)
          rest: List[List[T]] <- group(groups.tail, minus(aList, first))
        } yield first :: rest
    }

  }
}

object P28 {
  /*
    P28 (**) Sorting a list of lists according to length of sublists.
    a) We suppose that a list contains elements that are lists themselves. The objective is to sort the elements
    of the list according to their length. E.g. short lists first, longer lists later, or vice versa.
    Example:

    scala> lsort(List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o)))
    res0: List[List[Symbol]] = List(List('o), List('d, 'e), List('d, 'e), List('m, 'n), List('a, 'b, 'c), List('f, 'g, 'h), List('i, 'j, 'k, 'l))

    b) Again, we suppose that a list contains elements that are lists themselves. But this time the objective is to
    sort the elements according to their length frequency; i.e. in the default, sorting is done ascendingly, lists with
    rare lengths are placed first, others with a more frequent length come later.

    Example:

    scala> lsortFreq(List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o)))
    res1: List[List[Symbol]] = List(List('i, 'j, 'k, 'l), List('o), List('a, 'b, 'c), List('f, 'g, 'h), List('d, 'e), List('d, 'e), List('m, 'n))

    Note that in the above example, the first two lists in the result have length 4 and 1 and both lengths appear
    just once. The third and fourth lists have length 3 and there are two list of this length. Finally, the last three lists have length 2. This is the most frequent length.


   */
  def lsort[T](lists: List[List[T]]) : List[List[T]] =
    lists.sortWith( (t1, t2) => t1.length < t2.length)

  def lsortFreq[T](lists: List[List[T]]) : List[List[T]] = {
    val lengthsMap = P10.encode(lists.map( _.length).sortWith( (a,b) => a< b)).map( t => (t._2, t._1)).toMap
    lists.sortWith( (l1, l2) => lengthsMap(l1.length) < lengthsMap(l2.length))
  }

}
object S99 extends App {

  /*val aList = List(1, 1, 2, 3, 5, 8)
  println(s"P01 Last element : ${P01.last(aList)}")

  println(s"P02 Penultimate element : ${P02.penultimate(aList)}")

  println(s"P03 nth element (first) : ${P03.nth(0, aList)}")
  println(s"P03 nth element (third) : ${P03.nth(2, aList)}")
  println(s"P03 nth element (last) : ${P03.nth(aList.length - 1, aList)}")

  println(s"P04 length of list : ${P04.length(aList)}")
  println(s"P04 length of empty list : ${P04.length(List())}")
  println(s"P04 length of Nil list : ${P04.length(Nil)}")

  println(s"P05 reverse a list : ${P05.reverse(aList)}")

  println(s"P06 is list a palindrome: ${P06.isPalindrome(List(1, 2, 3, 2, 1))}")

  println(s"P07 flatten nested list: ${P07.flatten(List(List(1, 1), 2, List(3, List(5, 8))))}")
  println(s"P07 flatten nested list: ${P07.flattenWithFlatMap(List(List(1, 1), 2, List(3, List(5, 8))))}")

  println(s"P08 compress elements : ${P08.compressTailRec(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))}")

  println(s"P09 pack elements : ${P09.pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))}")
  println(s"P10 encode : ${P10.encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))}")
  println(s"P11 encode modified : ${P11.encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))}")


  println(s"P12 decode : ${P12.decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))}")

  println(s"P13 encodeDirect: ${P13.encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))}")

  println(s"P14 duplicate elements : ${P14.duplicate(List('a, 'b, 'c, 'c, 'd))}")
  println(s"P15 duplicateN elements : ${P15.duplicateN(3, List('a, 'b, 'c, 'c, 'd))}")

  println(s"P16 drop Nth element: ${P16.drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))}")

  println(s"P17 split : ${P17.split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))}")
  println(s"P18 slice : ${P18.slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))}")

  println(s"P19 split list at 3 : ${P19.rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))}")
  println(s"P19 split list at -2 : ${P19.rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))}")

  println(s"P20 remove at 1 : ${P20.removeAt(1, List('a, 'b, 'c, 'd))}")
  println(s"P20 remove at -1 : ${P20.removeAt(-1, List('a, 'b, 'c, 'd))}")
  println(s"P20 remove at 25 : ${P20.removeAt(25, List('a, 'b, 'c, 'd))}")
  println(s"P21 insertAt 1 : ${P21.insertAt('new, 1, List('a, 'b, 'c, 'd))}")
  println(s"P22 Int range : ${P22.range(4,9)}")
  println(s"P23 random select : ${P23.randomSelect(3, List('a, 'b, 'c, 'd, 'f, 'g, 'h))}")
  println(s"P24 lotto : ${P24.lotto(6, 49)}")
  println(s"P25 random permute ${P25.randomPermute(List('a, 'b, 'c, 'd, 'e, 'f))}")

  println(s"P26 combinations ${P26.combinations(3, List('a, 'b, 'c, 'd, 'e, 'f)).length}")
  println(s"P27 combinations of 2,3,4 ${P27.group3(List("Aldo", "Beat", "Carla", "David", "Evi", "Flip", "Gary", "Hugo", "Ida"))}")
  println(s"P27 generic combinations of 2,2,5 ${P27.group(List(2,2,5), List("Aldo", "Beat", "Carla", "David", "Evi", "Flip", "Gary", "Hugo", "Ida"))}")
  */

  println(s"Sort lists by length ${P28.lsort(List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o)))}")

}