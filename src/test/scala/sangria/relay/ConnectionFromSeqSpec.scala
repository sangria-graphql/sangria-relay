package sangria.relay

import org.scalatest.{Matchers, WordSpec}
import sangria.relay.util.AwaitSupport

import scala.concurrent.Future

class ConnectionFromSeqSpec extends WordSpec with Matchers with AwaitSupport {
  val Letters = List("A", "B", "C", "D", "E") map (Some(_))
  val FutureLetters = Future.successful(Letters)

  import Connection.{cursorForObjectInConnection, connectionFromSeq, connectionFromFutureSeq}

  "connectionFromSeq" when {
    "Handles basic slicing" should {
      "Returns all elements without filters" in {
        connectionFromSeq(Letters, ConnectionArgs.empty) should be (
          DefaultConnection(
            PageInfo(
              startCursor = Some("YXJyYXljb25uZWN0aW9uOjA="),
              endCursor = Some("YXJyYXljb25uZWN0aW9uOjQ="),
              hasPreviousPage = false,
              hasNextPage = false
            ),
            List(
              Edge("A", "YXJyYXljb25uZWN0aW9uOjA="),
              Edge("B", "YXJyYXljb25uZWN0aW9uOjE="),
              Edge("C", "YXJyYXljb25uZWN0aW9uOjI="),
              Edge("D", "YXJyYXljb25uZWN0aW9uOjM="),
              Edge("E", "YXJyYXljb25uZWN0aW9uOjQ="))))
      }

      "Respects a smaller first" in {
        connectionFromSeq(Letters, ConnectionArgs(first = Some(2))) should be (
          DefaultConnection(
            PageInfo(
              startCursor = Some("YXJyYXljb25uZWN0aW9uOjA="),
              endCursor = Some("YXJyYXljb25uZWN0aW9uOjE="),
              hasPreviousPage = false,
              hasNextPage = true
            ),
            List(
              Edge("A", "YXJyYXljb25uZWN0aW9uOjA="),
              Edge("B", "YXJyYXljb25uZWN0aW9uOjE="))))
      }

      "Respects an overly large first" in {
        connectionFromSeq(Letters, ConnectionArgs(first = Some(10))) should be (
          DefaultConnection(
            PageInfo(
              startCursor = Some("YXJyYXljb25uZWN0aW9uOjA="),
              endCursor = Some("YXJyYXljb25uZWN0aW9uOjQ="),
              hasPreviousPage = false,
              hasNextPage = false
            ),
            List(
              Edge("A", "YXJyYXljb25uZWN0aW9uOjA="),
              Edge("B", "YXJyYXljb25uZWN0aW9uOjE="),
              Edge("C", "YXJyYXljb25uZWN0aW9uOjI="),
              Edge("D", "YXJyYXljb25uZWN0aW9uOjM="),
              Edge("E", "YXJyYXljb25uZWN0aW9uOjQ="))))
      }

      "Respects a smaller last" in {
        connectionFromSeq(Letters, ConnectionArgs(last = Some(2))) should be (
          DefaultConnection(
            PageInfo(
              startCursor = Some("YXJyYXljb25uZWN0aW9uOjM="),
              endCursor = Some("YXJyYXljb25uZWN0aW9uOjQ="),
              hasPreviousPage = true,
              hasNextPage = false
            ),
            List(
              Edge("D", "YXJyYXljb25uZWN0aW9uOjM="),
              Edge("E", "YXJyYXljb25uZWN0aW9uOjQ="))))
      }

      "Respects an overly large last" in {
        connectionFromSeq(Letters, ConnectionArgs(last = Some(10))) should be (
          DefaultConnection(
            PageInfo(
              startCursor = Some("YXJyYXljb25uZWN0aW9uOjA="),
              endCursor = Some("YXJyYXljb25uZWN0aW9uOjQ="),
              hasPreviousPage = false,
              hasNextPage = false
            ),
            List(
              Edge("A", "YXJyYXljb25uZWN0aW9uOjA="),
              Edge("B", "YXJyYXljb25uZWN0aW9uOjE="),
              Edge("C", "YXJyYXljb25uZWN0aW9uOjI="),
              Edge("D", "YXJyYXljb25uZWN0aW9uOjM="),
              Edge("E", "YXJyYXljb25uZWN0aW9uOjQ="))))
      }
    }

    "Handles pagination" should {
      "Respects first and after" in {
        connectionFromSeq(Letters, ConnectionArgs(first = Some(2), after = Some("YXJyYXljb25uZWN0aW9uOjE="))) should be (
          DefaultConnection(
            PageInfo(
              startCursor = Some("YXJyYXljb25uZWN0aW9uOjI="),
              endCursor = Some("YXJyYXljb25uZWN0aW9uOjM="),
              hasPreviousPage = false,
              hasNextPage = true
            ),
            List(
              Edge("C", "YXJyYXljb25uZWN0aW9uOjI="),
              Edge("D", "YXJyYXljb25uZWN0aW9uOjM="))))
      }

      "Respects first and after with long first" in {
        connectionFromSeq(Letters, ConnectionArgs(first = Some(10), after = Some("YXJyYXljb25uZWN0aW9uOjE="))) should be (
          DefaultConnection(
            PageInfo(
              startCursor = Some("YXJyYXljb25uZWN0aW9uOjI="),
              endCursor = Some("YXJyYXljb25uZWN0aW9uOjQ="),
              hasPreviousPage = false,
              hasNextPage = false
            ),
            List(
              Edge("C", "YXJyYXljb25uZWN0aW9uOjI="),
              Edge("D", "YXJyYXljb25uZWN0aW9uOjM="),
              Edge("E", "YXJyYXljb25uZWN0aW9uOjQ="))))
      }

      "Respects last and before" in {
        connectionFromSeq(Letters, ConnectionArgs(last = Some(2), before = Some("YXJyYXljb25uZWN0aW9uOjM="))) should be (
          DefaultConnection(
            PageInfo(
              startCursor = Some("YXJyYXljb25uZWN0aW9uOjE="),
              endCursor = Some("YXJyYXljb25uZWN0aW9uOjI="),
              hasPreviousPage = true,
              hasNextPage = false
            ),
            List(
              Edge("B", "YXJyYXljb25uZWN0aW9uOjE="),
              Edge("C", "YXJyYXljb25uZWN0aW9uOjI="))))
      }

      "Respects last and before with long last" in {
        connectionFromSeq(Letters, ConnectionArgs(last = Some(10), before = Some("YXJyYXljb25uZWN0aW9uOjM="))) should be (
          DefaultConnection(
            PageInfo(
              startCursor = Some("YXJyYXljb25uZWN0aW9uOjA="),
              endCursor = Some("YXJyYXljb25uZWN0aW9uOjI="),
              hasPreviousPage = false,
              hasNextPage = false
            ),
            List(
              Edge("A", "YXJyYXljb25uZWN0aW9uOjA="),
              Edge("B", "YXJyYXljb25uZWN0aW9uOjE="),
              Edge("C", "YXJyYXljb25uZWN0aW9uOjI="))))
      }

      "Respects first and after and before, too few" in {
        connectionFromSeq(Letters, ConnectionArgs(first = Some(2), after = Some("YXJyYXljb25uZWN0aW9uOjA="), before = Some("YXJyYXljb25uZWN0aW9uOjQ="))) should be (
          DefaultConnection(
            PageInfo(
              startCursor = Some("YXJyYXljb25uZWN0aW9uOjE="),
              endCursor = Some("YXJyYXljb25uZWN0aW9uOjI="),
              hasPreviousPage = false,
              hasNextPage = true
            ),
            List(
              Edge("B", "YXJyYXljb25uZWN0aW9uOjE="),
              Edge("C", "YXJyYXljb25uZWN0aW9uOjI="))))
      }

      "Respects first and after and before, too many" in {
        connectionFromSeq(Letters, ConnectionArgs(first = Some(4), after = Some("YXJyYXljb25uZWN0aW9uOjA="), before = Some("YXJyYXljb25uZWN0aW9uOjQ="))) should be (
          DefaultConnection(
            PageInfo(
              startCursor = Some("YXJyYXljb25uZWN0aW9uOjE="),
              endCursor = Some("YXJyYXljb25uZWN0aW9uOjM="),
              hasPreviousPage = false,
              hasNextPage = false
            ),
            List(
              Edge("B", "YXJyYXljb25uZWN0aW9uOjE="),
              Edge("C", "YXJyYXljb25uZWN0aW9uOjI="),
              Edge("D", "YXJyYXljb25uZWN0aW9uOjM="))))
      }

      "Respects first and after and before, exactly right" in {
        connectionFromSeq(Letters, ConnectionArgs(first = Some(3), after = Some("YXJyYXljb25uZWN0aW9uOjA="), before = Some("YXJyYXljb25uZWN0aW9uOjQ="))) should be (
          DefaultConnection(
            PageInfo(
              startCursor = Some("YXJyYXljb25uZWN0aW9uOjE="),
              endCursor = Some("YXJyYXljb25uZWN0aW9uOjM="),
              hasPreviousPage = false,
              hasNextPage = false
            ),
            List(
              Edge("B", "YXJyYXljb25uZWN0aW9uOjE="),
              Edge("C", "YXJyYXljb25uZWN0aW9uOjI="),
              Edge("D", "YXJyYXljb25uZWN0aW9uOjM="))))
      }

      "Respects last and after and before, too few" in {
        connectionFromSeq(Letters, ConnectionArgs(last = Some(2), after = Some("YXJyYXljb25uZWN0aW9uOjA="), before = Some("YXJyYXljb25uZWN0aW9uOjQ="))) should be (
          DefaultConnection(
            PageInfo(
              startCursor = Some("YXJyYXljb25uZWN0aW9uOjI="),
              endCursor = Some("YXJyYXljb25uZWN0aW9uOjM="),
              hasPreviousPage = true,
              hasNextPage = false
            ),
            List(
              Edge("C", "YXJyYXljb25uZWN0aW9uOjI="),
              Edge("D", "YXJyYXljb25uZWN0aW9uOjM="))))
      }

      "Respects last and after and before, too many" in {
        connectionFromSeq(Letters, ConnectionArgs(last = Some(10), after = Some("YXJyYXljb25uZWN0aW9uOjA="), before = Some("YXJyYXljb25uZWN0aW9uOjQ="))) should be (
          DefaultConnection(
            PageInfo(
              startCursor = Some("YXJyYXljb25uZWN0aW9uOjE="),
              endCursor = Some("YXJyYXljb25uZWN0aW9uOjM="),
              hasPreviousPage = false,
              hasNextPage = false
            ),
            List(
              Edge("B", "YXJyYXljb25uZWN0aW9uOjE="),
              Edge("C", "YXJyYXljb25uZWN0aW9uOjI="),
              Edge("D", "YXJyYXljb25uZWN0aW9uOjM="))))
      }

      "Respects last and after and before, exactly right" in {
        connectionFromSeq(Letters, ConnectionArgs(last = Some(3), after = Some("YXJyYXljb25uZWN0aW9uOjA="), before = Some("YXJyYXljb25uZWN0aW9uOjQ="))) should be (
          DefaultConnection(
            PageInfo(
              startCursor = Some("YXJyYXljb25uZWN0aW9uOjE="),
              endCursor = Some("YXJyYXljb25uZWN0aW9uOjM="),
              hasPreviousPage = false,
              hasNextPage = false
            ),
            List(
              Edge("B", "YXJyYXljb25uZWN0aW9uOjE="),
              Edge("C", "YXJyYXljb25uZWN0aW9uOjI="),
              Edge("D", "YXJyYXljb25uZWN0aW9uOjM="))))
      }
    }

    "Handles cursor edge cases" should {
      "Returns no elements if first is 0" in {
        connectionFromSeq(Letters, ConnectionArgs(first = Some(0))) should be (
          Connection.empty[Option[String]].copy(pageInfo = PageInfo.empty.copy(hasNextPage = true)))
      }

      "Returns all elements if cursors are invalid" in {
        connectionFromSeq(Letters, ConnectionArgs(after = Some("invalid"), before = Some("invalid"))) should be (
          DefaultConnection(
            PageInfo(
              startCursor = Some("YXJyYXljb25uZWN0aW9uOjA="),
              endCursor = Some("YXJyYXljb25uZWN0aW9uOjQ="),
              hasPreviousPage = false,
              hasNextPage = false
            ),
            List(
              Edge("A", "YXJyYXljb25uZWN0aW9uOjA="),
              Edge("B", "YXJyYXljb25uZWN0aW9uOjE="),
              Edge("C", "YXJyYXljb25uZWN0aW9uOjI="),
              Edge("D", "YXJyYXljb25uZWN0aW9uOjM="),
              Edge("E", "YXJyYXljb25uZWN0aW9uOjQ="))))
      }

      "Returns all elements if cursors are on the outside" in {
        connectionFromSeq(Letters, ConnectionArgs(after = Some("YXJyYXljb25uZWN0aW9uOi0xCg=="), before = Some("YXJyYXljb25uZWN0aW9uOjYK"))) should be (
          DefaultConnection(
            PageInfo(
              startCursor = Some("YXJyYXljb25uZWN0aW9uOjA="),
              endCursor = Some("YXJyYXljb25uZWN0aW9uOjQ="),
              hasPreviousPage = false,
              hasNextPage = false
            ),
            List(
              Edge("A", "YXJyYXljb25uZWN0aW9uOjA="),
              Edge("B", "YXJyYXljb25uZWN0aW9uOjE="),
              Edge("C", "YXJyYXljb25uZWN0aW9uOjI="),
              Edge("D", "YXJyYXljb25uZWN0aW9uOjM="),
              Edge("E", "YXJyYXljb25uZWN0aW9uOjQ="))))
      }

      "Returns no elements if cursors cross" in {
        connectionFromSeq(Letters, ConnectionArgs(after = Some("YXJyYXljb25uZWN0aW9uOjQ="), before = Some("YXJyYXljb25uZWN0aW9uOjI="))) should be (Connection.empty)
      }
    }
  }

  "connectionFromFutureSeq" should {
    import scala.concurrent.ExecutionContext.Implicits.global

    "Returns all elements without filters" in {
      connectionFromFutureSeq(FutureLetters, ConnectionArgs.empty).await should be (
        DefaultConnection(
          PageInfo(
            startCursor = Some("YXJyYXljb25uZWN0aW9uOjA="),
            endCursor = Some("YXJyYXljb25uZWN0aW9uOjQ="),
            hasPreviousPage = false,
            hasNextPage = false
          ),
          List(
            Edge("A", "YXJyYXljb25uZWN0aW9uOjA="),
            Edge("B", "YXJyYXljb25uZWN0aW9uOjE="),
            Edge("C", "YXJyYXljb25uZWN0aW9uOjI="),
            Edge("D", "YXJyYXljb25uZWN0aW9uOjM="),
            Edge("E", "YXJyYXljb25uZWN0aW9uOjQ="))))
    }

    "Respects a smaller first" in {
      connectionFromFutureSeq(FutureLetters, ConnectionArgs(first = Some(2))).await should be (
        DefaultConnection(
          PageInfo(
            startCursor = Some("YXJyYXljb25uZWN0aW9uOjA="),
            endCursor = Some("YXJyYXljb25uZWN0aW9uOjE="),
            hasPreviousPage = false,
            hasNextPage = true
          ),
          List(
            Edge("A", "YXJyYXljb25uZWN0aW9uOjA="),
            Edge("B", "YXJyYXljb25uZWN0aW9uOjE="))))
    }

    "respects a smaller first (sliced)" in {
      connectionFromFutureSeq(FutureLetters map (_ take 3), ConnectionArgs(first = Some(2)), SliceInfo(0, 5)).await should be (
        DefaultConnection(
          PageInfo(
            startCursor = Some("YXJyYXljb25uZWN0aW9uOjA="),
            endCursor = Some("YXJyYXljb25uZWN0aW9uOjE="),
            hasPreviousPage = false,
            hasNextPage = true
          ),
          List(
            Edge("A", "YXJyYXljb25uZWN0aW9uOjA="),
            Edge("B", "YXJyYXljb25uZWN0aW9uOjE="))))
    }
  }

  "cursorForObjectInConnection" should {
    "returns an edge's cursor, given an array and a member object" in {
      cursorForObjectInConnection(Letters, "B") should be (Some("YXJyYXljb25uZWN0aW9uOjE="))
    }

    "returns null, given an array and a non-member object" in {
      cursorForObjectInConnection(Letters, "F") should be (None)
    }
  }

  "connectionFromSeq (with slice)" should {
    "works with a just-right array slice" in {
      connectionFromSeq(Letters.slice(1, 3), ConnectionArgs(first = Some(2), after = Some("YXJyYXljb25uZWN0aW9uOjA=")), SliceInfo(1, 5)) should be(
        DefaultConnection(
          PageInfo(
            startCursor = Some("YXJyYXljb25uZWN0aW9uOjE="),
            endCursor = Some("YXJyYXljb25uZWN0aW9uOjI="),
            hasPreviousPage = false,
            hasNextPage = true
          ),
          List(
            Edge("B", "YXJyYXljb25uZWN0aW9uOjE="),
            Edge("C", "YXJyYXljb25uZWN0aW9uOjI="))))
    }

    "works with an oversized array slice (left side)" in {
      connectionFromSeq(Letters.slice(0, 3), ConnectionArgs(first = Some(2), after = Some("YXJyYXljb25uZWN0aW9uOjA=")), SliceInfo(0, 5)) should be(
        DefaultConnection(
          PageInfo(
            startCursor = Some("YXJyYXljb25uZWN0aW9uOjE="),
            endCursor = Some("YXJyYXljb25uZWN0aW9uOjI="),
            hasPreviousPage = false,
            hasNextPage = true
          ),
          List(
            Edge("B", "YXJyYXljb25uZWN0aW9uOjE="),
            Edge("C", "YXJyYXljb25uZWN0aW9uOjI="))))
    }

    "works with an oversized array slice (right side)" in {
      connectionFromSeq(Letters.slice(2, 4), ConnectionArgs(first = Some(1), after = Some("YXJyYXljb25uZWN0aW9uOjE=")), SliceInfo(2, 5)) should be(
        DefaultConnection(
          PageInfo(
            startCursor = Some("YXJyYXljb25uZWN0aW9uOjI="),
            endCursor = Some("YXJyYXljb25uZWN0aW9uOjI="),
            hasPreviousPage = false,
            hasNextPage = true
          ),
          List(
            Edge("C", "YXJyYXljb25uZWN0aW9uOjI="))))
    }

    "works with an oversized array slice (both sides)" in {
      connectionFromSeq(Letters.slice(1, 4), ConnectionArgs(first = Some(1), after = Some("YXJyYXljb25uZWN0aW9uOjE=")), SliceInfo(1, 5)) should be(
        DefaultConnection(
          PageInfo(
            startCursor = Some("YXJyYXljb25uZWN0aW9uOjI="),
            endCursor = Some("YXJyYXljb25uZWN0aW9uOjI="),
            hasPreviousPage = false,
            hasNextPage = true
          ),
          List(
            Edge("C", "YXJyYXljb25uZWN0aW9uOjI="))))
    }

    "works with an undersized array slice (left side)" in {
      connectionFromSeq(Letters.slice(3, 5), ConnectionArgs(first = Some(3), after = Some("YXJyYXljb25uZWN0aW9uOjE=")), SliceInfo(3, 5)) should be(
        DefaultConnection(
          PageInfo(
            startCursor = Some("YXJyYXljb25uZWN0aW9uOjM="),
            endCursor = Some("YXJyYXljb25uZWN0aW9uOjQ="),
            hasPreviousPage = false,
            hasNextPage = false
          ),
          List(
            Edge("D", "YXJyYXljb25uZWN0aW9uOjM="),
            Edge("E", "YXJyYXljb25uZWN0aW9uOjQ="))))
    }

    "works with an undersized array slice (right side)" in {
      connectionFromSeq(Letters.slice(2, 4), ConnectionArgs(first = Some(3), after = Some("YXJyYXljb25uZWN0aW9uOjE=")), SliceInfo(2, 5)) should be(
        DefaultConnection(
          PageInfo(
            startCursor = Some("YXJyYXljb25uZWN0aW9uOjI="),
            endCursor = Some("YXJyYXljb25uZWN0aW9uOjM="),
            hasPreviousPage = false,
            hasNextPage = true
          ),
          List(
            Edge("C", "YXJyYXljb25uZWN0aW9uOjI="),
            Edge("D", "YXJyYXljb25uZWN0aW9uOjM="))))
    }

    "works with an undersized array slice (both sides)" in {
      connectionFromSeq(Letters.slice(3, 4), ConnectionArgs(first = Some(3), after = Some("YXJyYXljb25uZWN0aW9uOjE=")), SliceInfo(3, 5)) should be(
        DefaultConnection(
          PageInfo(
            startCursor = Some("YXJyYXljb25uZWN0aW9uOjM="),
            endCursor = Some("YXJyYXljb25uZWN0aW9uOjM="),
            hasPreviousPage = false,
            hasNextPage = true
          ),
          List(
            Edge("D", "YXJyYXljb25uZWN0aW9uOjM="))))
    }
  }
}
