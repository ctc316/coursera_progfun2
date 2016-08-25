package streams

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Bloxorz._

@RunWith(classOf[JUnitRunner])
class BloxorzSuite extends FunSuite {

  trait SolutionChecker extends GameDef with Solver with StringParserTerrain {
    /**
     * This method applies a list of moves `ls` to the block at position
     * `startPos`. This can be used to verify if a certain list of moves
     * is a valid solution, i.e. leads to the goal.
     */
    def solve(ls: List[Move]): Block =
      ls.foldLeft(startBlock) { case (block, move) => move match {
        case Left => block.left
        case Right => block.right
        case Up => block.up
        case Down => block.down
      }
    }
  }

  trait Level1 extends SolutionChecker {
      /* terrain for level 1*/

    val level =
    """ooo-------
      |oSoooo----
      |ooooooooo-
      |-ooooooooo
      |-----ooToo
      |------ooo-""".stripMargin

    val optsolution = List(Right, Right, Down, Right, Right, Right, Down)
  }


	test("terrain function level 1") {
    new Level1 {
      assert(terrain(Pos(0,0)), "0,0")
      assert(terrain(Pos(1,1)), "1,1") // start
      assert(terrain(Pos(4,7)), "4,7") // goal
      assert(terrain(Pos(5,8)), "5,8")
      assert(!terrain(Pos(5,9)), "5,9")
      assert(terrain(Pos(4,9)), "4,9")
      assert(!terrain(Pos(6,8)), "6,8")
      assert(!terrain(Pos(4,11)), "4,11")
      assert(!terrain(Pos(-1,0)), "-1,0")
      assert(!terrain(Pos(0,-1)), "0,-1")
    }
  }

	test("findChar level 1") {
    new Level1 {
      assert(startPos == Pos(1,1))
    }
  }


	test("optimal solution for level 1") {
    new Level1 {
      assert(solve(solution) == Block(goal, goal))
    }
  }


	test("optimal solution length for level 1") {
    new Level1 {
      assert(solution.length == optsolution.length)
    }
  }


  /********************************************************************/


  test("isStanding") {
    new Level1 {
      assert(Block(Pos(3,5), Pos(3,5)).isStanding, "3,5:3,5")
      assert(!Block(Pos(2,4), Pos(3,5)).isStanding, "2,4:3,5")
    }
  }

  test("legalNeighbors") {
    new Level1 {
      assert(startBlock.legalNeighbors == List(
        (Block(Pos(1,2), Pos(1,3)), Right),
        (Block(Pos(2,1), Pos(3,1)), Down))
      )
    }
  }

  test("done") {
    new Level1 {
      assert(done(Block(Pos(4,7), Pos(4,7))), "4,7:4,7")
      assert(!done(Block(Pos(4,6), Pos(4,6))), "4,6:4,6")
      assert(!done(Block(Pos(4,7), Pos(4,8))), "4,7:4,8")
      assert(!done(Block(Pos(4,7), Pos(5,7))), "4,7:5,7")
      assert(!done(Block(Pos(4,6), Pos(4,7))), "4,6:4,7")
    }
  }

  test("neighborsWithHistory") {
    new Level1 {
      assert(neighborsWithHistory(Block(Pos(1,1),Pos(2,1)), List(Left,Up)).toSet == Set(
        (Block(Pos(1,0),Pos(2,0)), List(Left,Left,Up)),
        (Block(Pos(1,2),Pos(2,2)), List(Right,Left,Up)),
        (Block(Pos(0,1),Pos(0,1)), List(Up,Left,Up)),
        (Block(Pos(3,1),Pos(3,1)), List(Down,Left,Up))
      ))
    }
  }


  test("newNeighborsOnly") {
    new Level1 {
      val test = newNeighborsOnly(
        Set(
          (Block(Pos(1,2),Pos(1,3)), List(Right,Left,Up)),
          (Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))
        ).toStream,
        Set(Block(Pos(1,2),Pos(1,3)), Block(Pos(1,1),Pos(1,1)))
      )

      val expected = Set(
        (Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))
      ).toStream

      assert(test == expected)
    }
  }


  trait LevelImpossible extends SolutionChecker {
    /* terrain for level impossible */

    val level =
      """-----
        |-Soo-
        |-oTo-
        |-----""".stripMargin

    val optsolution = Nil
  }

  test("optimal solution length for level impossible") {
    new LevelImpossible {
      assert(solution.length == optsolution.length)
    }
  }

  test("optimal solution path for level 1") {
    new Level1 {
      assert(solution == optsolution)
    }
  }
}
