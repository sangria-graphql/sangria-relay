package sangria.relay

import sangria.relay.util.AwaitSupport

import scala.concurrent.Future
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ConnectionFromSeqSpec extends AnyWordSpec with Matchers with AwaitSupport {
  val Letters = List("A", "B", "C", "D", "E")
  val FutureLetters = Future.successful(Letters)

  import Connection.{connectionFromFutureSeq, connectionFromSeq, cursorForObjectInConnection}

  private val CursorOfA = "YXJyYXljb25uZWN0aW9uOjA="
  private val CursorOfB = "YXJyYXljb25uZWN0aW9uOjE="
  private val CursorOfC = "YXJyYXljb25uZWN0aW9uOjI="
  private val CursorOfD = "YXJyYXljb25uZWN0aW9uOjM="
  private val CursorOfE = "YXJyYXljb25uZWN0aW9uOjQ="

  "connectionFromSeq" when {
    "Handles basic slicing" should {
      "Returns all elements without filters" in {
        connectionFromSeq(Letters, ConnectionArgs.empty) should be(
          DefaultConnection(
            PageInfo(
              startCursor = Some(CursorOfA),
              endCursor = Some(CursorOfE),
              hasPreviousPage = false,
              hasNextPage = false
            ),
            List(
              Edge("A", CursorOfA),
              Edge("B", CursorOfB),
              Edge("C", CursorOfC),
              Edge("D", CursorOfD),
              Edge("E", CursorOfE)
            )
          ))
      }

      "Respects a smaller first" in {
        connectionFromSeq(Letters, ConnectionArgs(first = Some(2))) should be(
          DefaultConnection(
            PageInfo(
              startCursor = Some(CursorOfA),
              endCursor = Some(CursorOfB),
              hasPreviousPage = false,
              hasNextPage = true
            ),
            List(Edge("A", CursorOfA), Edge("B", CursorOfB))
          ))
      }

      "Respects an overly large first" in {
        connectionFromSeq(Letters, ConnectionArgs(first = Some(10))) should be(
          DefaultConnection(
            PageInfo(
              startCursor = Some(CursorOfA),
              endCursor = Some(CursorOfE),
              hasPreviousPage = false,
              hasNextPage = false
            ),
            List(
              Edge("A", CursorOfA),
              Edge("B", CursorOfB),
              Edge("C", CursorOfC),
              Edge("D", CursorOfD),
              Edge("E", CursorOfE)
            )
          ))
      }

      "Respects a smaller last" in {
        connectionFromSeq(Letters, ConnectionArgs(last = Some(2))) should be(
          DefaultConnection(
            PageInfo(
              startCursor = Some(CursorOfD),
              endCursor = Some(CursorOfE),
              hasPreviousPage = true,
              hasNextPage = false
            ),
            List(Edge("D", CursorOfD), Edge("E", CursorOfE))
          ))
      }

      "Respects an overly large last" in {
        connectionFromSeq(Letters, ConnectionArgs(last = Some(10))) should be(
          DefaultConnection(
            PageInfo(
              startCursor = Some(CursorOfA),
              endCursor = Some(CursorOfE),
              hasPreviousPage = false,
              hasNextPage = false
            ),
            List(
              Edge("A", CursorOfA),
              Edge("B", CursorOfB),
              Edge("C", CursorOfC),
              Edge("D", CursorOfD),
              Edge("E", CursorOfE)
            )
          ))
      }
    }

    "Handles pagination" should {
      "Respects first and after" in {
        connectionFromSeq(
          Letters,
          ConnectionArgs(first = Some(2), after = Some(CursorOfB))) should be(
          DefaultConnection(
            PageInfo(
              startCursor = Some(CursorOfC),
              endCursor = Some(CursorOfD),
              hasPreviousPage = true, // A and B are previous
              hasNextPage = true
            ),
            List(Edge("C", CursorOfC), Edge("D", CursorOfD))
          ))
      }

      "Respects first and after with long first" in {
        connectionFromSeq(
          Letters,
          ConnectionArgs(first = Some(10), after = Some(CursorOfB))) should be(
          DefaultConnection(
            PageInfo(
              startCursor = Some(CursorOfC),
              endCursor = Some(CursorOfE),
              hasPreviousPage = true, // A and B are previous
              hasNextPage = false
            ),
            List(Edge("C", CursorOfC), Edge("D", CursorOfD), Edge("E", CursorOfE))
          ))
      }

      "Respects last and before" in {
        connectionFromSeq(
          Letters,
          ConnectionArgs(last = Some(2), before = Some(CursorOfD))) should be(
          DefaultConnection(
            PageInfo(
              startCursor = Some(CursorOfB),
              endCursor = Some(CursorOfC),
              hasPreviousPage = true,
              hasNextPage = true // D and E are next
            ),
            List(Edge("B", CursorOfB), Edge("C", CursorOfC))
          ))
      }

      "Respects last and before with long last" in {
        connectionFromSeq(
          Letters,
          ConnectionArgs(last = Some(10), before = Some(CursorOfD))) should be(
          DefaultConnection(
            PageInfo(
              startCursor = Some(CursorOfA),
              endCursor = Some(CursorOfC),
              hasPreviousPage = false,
              hasNextPage = true // D and E are next
            ),
            List(Edge("A", CursorOfA), Edge("B", CursorOfB), Edge("C", CursorOfC))
          ))
      }

      "Respects first and after and before, too few" in {
        connectionFromSeq(
          Letters,
          ConnectionArgs(
            first = Some(2),
            after = Some(CursorOfA),
            before = Some(CursorOfE))) should be(
          DefaultConnection(
            PageInfo(
              startCursor = Some(CursorOfB),
              endCursor = Some(CursorOfC),
              hasPreviousPage = true, // A is previous
              hasNextPage = true
            ),
            List(Edge("B", CursorOfB), Edge("C", CursorOfC))
          ))
      }

      "Respects first and after and before, too many" in {
        connectionFromSeq(
          Letters,
          ConnectionArgs(
            first = Some(4),
            after = Some(CursorOfA),
            before = Some(CursorOfE))) should be(
          DefaultConnection(
            PageInfo(
              startCursor = Some(CursorOfB),
              endCursor = Some(CursorOfD),
              hasPreviousPage = true, // A is previous
              // `hasNextPage=false` because the spec says:
              // "If first is set: [...] If edges contains more than first elements return true, otherwise false."
              // and after the cursors have been applied, the array contains 3 elements <= first (4)
              // https://relay.dev/graphql/connections.htm#sec-undefined.PageInfo
              hasNextPage = false
            ),
            List(Edge("B", CursorOfB), Edge("C", CursorOfC), Edge("D", CursorOfD))
          ))
      }

      "Respects first and after and before, exactly right" in {
        connectionFromSeq(
          Letters,
          ConnectionArgs(
            first = Some(3),
            after = Some(CursorOfA),
            before = Some(CursorOfE))) should be(
          DefaultConnection(
            PageInfo(
              startCursor = Some(CursorOfB),
              endCursor = Some(CursorOfD),
              hasPreviousPage = true,
              // `hasNextPage=false` because the spec says:
              // "If first is set: [...] If edges contains more than first elements return true, otherwise false."
              // and after the cursors have been applied, the array contains 3 elements <= first (3)
              // https://relay.dev/graphql/connections.htm#sec-undefined.PageInfo
              hasNextPage = false
            ),
            List(Edge("B", CursorOfB), Edge("C", CursorOfC), Edge("D", CursorOfD))
          ))
      }

      "Respects last and after and before, too few" in {
        connectionFromSeq(
          Letters,
          ConnectionArgs(
            last = Some(2),
            after = Some(CursorOfA),
            before = Some(CursorOfE))) should be(
          DefaultConnection(
            PageInfo(
              startCursor = Some(CursorOfC),
              endCursor = Some(CursorOfD),
              hasPreviousPage = true,
              hasNextPage = true
            ),
            List(Edge("C", CursorOfC), Edge("D", CursorOfD))
          ))
      }

      "Respects last and after and before, too many" in {
        connectionFromSeq(
          Letters,
          ConnectionArgs(
            last = Some(10),
            after = Some(CursorOfA),
            before = Some(CursorOfE))) should be(
          DefaultConnection(
            PageInfo(
              startCursor = Some(CursorOfB),
              endCursor = Some(CursorOfD),
              // `hasPreviousPage=false` because the spec says:
              // "If last is set: [...] If edges contains more than last elements return true, otherwise false."
              // and after the cursors have been applied, the array contains 3 elements <= last (4)
              hasPreviousPage = false,
              hasNextPage = true
            ),
            List(Edge("B", CursorOfB), Edge("C", CursorOfC), Edge("D", CursorOfD))
          ))
      }

      "Respects last and after and before, exactly right" in {
        connectionFromSeq(
          Letters,
          ConnectionArgs(
            last = Some(3),
            after = Some(CursorOfA),
            before = Some(CursorOfE))) should be(
          DefaultConnection(
            PageInfo(
              startCursor = Some(CursorOfB),
              endCursor = Some(CursorOfD),
              // `hasPreviousPage=false` because the spec says:
              // "If last is set: [...] If edges contains more than last elements return true, otherwise false."
              // and after the cursors have been applied, the array contains 3 elements <= last (3)
              hasPreviousPage = false,
              hasNextPage = true
            ),
            List(Edge("B", CursorOfB), Edge("C", CursorOfC), Edge("D", CursorOfD))
          ))
      }
    }

    "Handles cursor edge cases" should {
      "Returns no elements if first is 0" in {
        connectionFromSeq(Letters, ConnectionArgs(first = Some(0))) should be(
          Connection.empty[Option[String]].copy(pageInfo = PageInfo.empty.copy(hasNextPage = true)))
      }

      "throws an error if first < 0" in {
        an[ConnectionArgumentValidationError] should be thrownBy
          connectionFromSeq(Letters, ConnectionArgs(first = Some(-1)))
      }

      "throws an error if last < 0" in {
        an[ConnectionArgumentValidationError] should be thrownBy
          connectionFromSeq(Letters, ConnectionArgs(last = Some(-1)))
      }

      "Returns all elements if cursors are invalid" in {
        connectionFromSeq(
          Letters,
          ConnectionArgs(after = Some("invalid"), before = Some("invalid"))) should be(
          DefaultConnection(
            PageInfo(
              startCursor = Some(CursorOfA),
              endCursor = Some(CursorOfE),
              hasPreviousPage = false,
              hasNextPage = false
            ),
            List(
              Edge("A", CursorOfA),
              Edge("B", CursorOfB),
              Edge("C", CursorOfC),
              Edge("D", CursorOfD),
              Edge("E", CursorOfE)
            )
          ))
      }

      "Returns all elements if cursors are on the outside" in {
        connectionFromSeq(
          Letters,
          ConnectionArgs(
            after = Some("YXJyYXljb25uZWN0aW9uOi0xCg=="),
            before = Some("YXJyYXljb25uZWN0aW9uOjYK"))) should be(
          DefaultConnection(
            PageInfo(
              startCursor = Some(CursorOfA),
              endCursor = Some(CursorOfE),
              hasPreviousPage = false,
              hasNextPage = false
            ),
            List(
              Edge("A", CursorOfA),
              Edge("B", CursorOfB),
              Edge("C", CursorOfC),
              Edge("D", CursorOfD),
              Edge("E", CursorOfE)
            )
          ))
      }

      "Returns no elements if cursors cross" in {
        val pageInfo = PageInfo(hasNextPage = true, hasPreviousPage = true)
        connectionFromSeq(
          Letters,
          ConnectionArgs(after = Some(CursorOfE), before = Some(CursorOfC))) should be(
          Connection.empty.copy(pageInfo = pageInfo))
      }
    }
  }

  "connectionFromFutureSeq" should {
    import scala.concurrent.ExecutionContext.Implicits.global

    "Returns all elements without filters" in {
      connectionFromFutureSeq(FutureLetters, ConnectionArgs.empty).await should be(
        DefaultConnection(
          PageInfo(
            startCursor = Some(CursorOfA),
            endCursor = Some(CursorOfE),
            hasPreviousPage = false,
            hasNextPage = false
          ),
          List(
            Edge("A", CursorOfA),
            Edge("B", CursorOfB),
            Edge("C", CursorOfC),
            Edge("D", CursorOfD),
            Edge("E", CursorOfE)
          )
        ))
    }

    "Respects a smaller first" in {
      connectionFromFutureSeq(FutureLetters, ConnectionArgs(first = Some(2))).await should be(
        DefaultConnection(
          PageInfo(
            startCursor = Some(CursorOfA),
            endCursor = Some(CursorOfB),
            hasPreviousPage = false,
            hasNextPage = true
          ),
          List(Edge("A", CursorOfA), Edge("B", CursorOfB))
        ))
    }

    "respects a smaller first (sliced)" in {
      connectionFromFutureSeq(
        FutureLetters.map(_.take(3)),
        ConnectionArgs(first = Some(2)),
        SliceInfo(0, 5)).await should be(
        DefaultConnection(
          PageInfo(
            startCursor = Some(CursorOfA),
            endCursor = Some(CursorOfB),
            hasPreviousPage = false,
            hasNextPage = true
          ),
          List(Edge("A", CursorOfA), Edge("B", CursorOfB))
        ))
    }
  }

  "cursorForObjectInConnection" should {
    "returns an edge's cursor, given an array and a member object" in {
      cursorForObjectInConnection(Letters, "B") should be(Some(CursorOfB))
    }

    "returns null, given an array and a non-member object" in {
      cursorForObjectInConnection(Letters, "F") should be(None)
    }
  }

  "connectionFromSeq (with slice)" should {
    "works with a just-right array slice" in {
      connectionFromSeq(
        arraySlice = Letters.slice(1, 3),
        args = ConnectionArgs(first = Some(2), after = Some(CursorOfA)),
        sliceInfo = SliceInfo(1, 5)) should be(
        DefaultConnection(
          PageInfo(
            startCursor = Some(CursorOfB),
            endCursor = Some(CursorOfC),
            hasPreviousPage = true, // A is previous
            hasNextPage = true
          ),
          List(Edge("B", CursorOfB), Edge("C", CursorOfC))
        ))
    }

    "works with an oversized array slice (left side)" in {
      connectionFromSeq(
        arraySlice = Letters.slice(0, 3), // A, B, C
        args = ConnectionArgs(first = Some(2), after = Some(CursorOfA)),
        sliceInfo = SliceInfo(0, 5)) should be(
        DefaultConnection(
          PageInfo(
            startCursor = Some(CursorOfB),
            endCursor = Some(CursorOfC),
            hasPreviousPage = true, // A is previous
            hasNextPage = true
          ),
          List(Edge("B", CursorOfB), Edge("C", CursorOfC))
        ))
    }

    "works with an oversized array slice (right side)" in {
      connectionFromSeq(
        arraySlice = Letters.slice(2, 4), // C, D
        args = ConnectionArgs(first = Some(1), after = Some(CursorOfB)),
        sliceInfo = SliceInfo(2, 5)) should be(
        DefaultConnection(
          PageInfo(
            startCursor = Some(CursorOfC),
            endCursor = Some(CursorOfC),
            hasPreviousPage = true, // A and B are previous
            hasNextPage = true
          ),
          List(Edge("C", CursorOfC))
        ))
    }

    "works with an oversized array slice (both sides)" in {
      connectionFromSeq(
        Letters.slice(1, 4), // B, C, D
        ConnectionArgs(first = Some(1), after = Some(CursorOfB)),
        SliceInfo(1, 5)) should be(
        DefaultConnection(
          PageInfo(
            startCursor = Some(CursorOfC),
            endCursor = Some(CursorOfC),
            hasPreviousPage = true, // A and B are previous
            hasNextPage = true
          ),
          List(Edge("C", CursorOfC))
        ))
    }

    "works with an undersized array slice (left side)" in {
      connectionFromSeq(
        Letters.slice(3, 5), // D, E
        ConnectionArgs(first = Some(3), after = Some(CursorOfB)),
        SliceInfo(3, 5)) should be(
        DefaultConnection(
          PageInfo(
            startCursor = Some(CursorOfD),
            endCursor = Some(CursorOfE),
            hasPreviousPage = true, // A, B, and C are previous
            hasNextPage = false
          ),
          List(Edge("D", CursorOfD), Edge("E", CursorOfE))
        ))
    }

    "works with an undersized array slice (right side)" in {
      connectionFromSeq(
        Letters.slice(2, 4), // C, D
        ConnectionArgs(first = Some(3), after = Some(CursorOfB)),
        SliceInfo(2, 5)) should be(
        DefaultConnection(
          PageInfo(
            startCursor = Some(CursorOfC),
            endCursor = Some(CursorOfD),
            hasPreviousPage = true,
            hasNextPage = false
          ),
          List(Edge("C", CursorOfC), Edge("D", CursorOfD))
        ))
    }

    "works with an undersized array slice (both sides)" in {
      connectionFromSeq(
        Letters.slice(3, 4),
        ConnectionArgs(first = Some(3), after = Some(CursorOfB)),
        SliceInfo(3, 5)) should be(
        DefaultConnection(
          PageInfo(
            startCursor = Some(CursorOfD),
            endCursor = Some(CursorOfD),
            hasPreviousPage = true,
            hasNextPage = false
          ),
          List(Edge("D", CursorOfD))
        ))
    }

    "Retruns no elements and proper hasPreviousPage/hasNextPage when cursor out of range" in {
      connectionFromSeq(
        List.empty,
        ConnectionArgs(
          first = Some(2),
          // after 10th
          after = Some("YXJyYXljb25uZWN0aW9uOjEw")
        ),
        SliceInfo(10, 4)) should be(
        DefaultConnection(
          PageInfo(
            startCursor = None,
            endCursor = None,
            hasPreviousPage = true,
            hasNextPage = false
          ),
          List.empty
        ))
    }
  }
}
