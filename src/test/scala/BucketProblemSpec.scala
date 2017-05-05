import org.scalacheck.Prop._
import org.scalacheck.{Arbitrary, Gen, Properties}

class BucketProblemSpec extends Properties("BucketProblem") {

  sealed trait Step
  case object EmptySmall extends Step
  case object EmptyLarge extends Step
  case object FillSmall extends Step
  case object FillLarge extends Step
  case object SmallInLarge extends Step
  case object LargeInSmall extends Step

  case class State private (small: Int, large: Int)
  object State {
    def empty = State(0, 0)
  }

  def modify(state: State, step: Step): State =
    step match {
      case EmptySmall => state.copy(small = 0)
      case EmptyLarge => state.copy(large = 0)
      case FillSmall => state.copy(small = 3)
      case FillLarge => state.copy(large = 5)
      case SmallInLarge =>
        val newLarge = math.min(state.large + state.small, 5)
        val newSmall = state.small - (newLarge - state.large)
        State(newSmall, newLarge)
      case LargeInSmall =>
        val newSmall = math.min(state.small + state.large, 3)
        val newLarge = state.large - (newSmall - state.small)
        State(newSmall, newLarge)
    }

  def modify(state: State, steps: List[Step]): State = {
    var currentState = state
    steps.foreach { step =>
      currentState = modify(currentState, step)
    }
    currentState
  }

  /*steps match {
      case head :: tail => modify(modify(state, head), tail)
      case Nil => state
    }*/

  implicit val arbitraryState: Arbitrary[State] = Arbitrary(for {
    small <- Gen.chooseNum(0, 3)
    large <- Gen.chooseNum(0, 5)
  } yield State(small, large))

  implicit val arbitraryStep: Arbitrary[Step] = Arbitrary(
    Gen.oneOf(EmptySmall,
              EmptyLarge,
              FillSmall,
              FillLarge,
              LargeInSmall,
              SmallInLarge))

  property("EmptySmall empties the small bucket") = forAll { state: State =>
    modify(state, EmptySmall).small == 0
  }

  property("modify 1") = forAll { (state: State, step: Step) =>
    val newState = modify(state, step)
    newState.small >= 0 && newState.small <= 3 &&
    newState.large >= 0 && newState.large <= 5
  }

  property("modify 2") = forAll { (steps: List[Step]) =>
    val newState = modify(State.empty, steps)
    newState.large != 4
  }

  /*
  > test
[info] ! BucketProblem.modify 2: Falsified after 4714 passed tests.
[info] > ARG_0: List("FillLarge", "LargeInSmall", "EmptySmall", "LargeInSmall", "FillLarge", "LargeInSmall")
[info] > ARG_0_ORIGINAL: List("LargeInSmall", "EmptySmall", "FillSmall", "LargeInSmall", "SmallInLarge", "SmallInLarge", "FillSmall", "LargeInSmall", "LargeInSmall", "EmptyLarge", "FillLarge", "EmptySmall", "EmptySmall", "SmallInLarge", "FillSmall", "FillSmall",
 "FillSmall", "EmptySmall", "LargeInSmall", "LargeInSmall", "EmptySmall", "FillLarge", "EmptySmall", "EmptyLarge", "EmptySmall", "SmallInLarge", "SmallInLarge", "SmallInLarge", "SmallInLarge", "SmallInLarge", "FillLarge", "LargeInSmall", "FillLarge", "SmallInLar
ge", "SmallInLarge", "FillSmall", "LargeInSmall", "FillSmall", "LargeInSmall", "FillLarge", "EmptySmall", "LargeInSmall", "EmptySmall", "LargeInSmall", "FillLarge", "LargeInSmall")
[info] + BucketProblem.EmptySmall empties the small bucket: OK, passed 10000 tests.
[info] + BucketProblem.modify 1: OK, passed 10000 tests.
[info] Failed: Total 3, Failed 1, Errors 0, Passed 2

   */
}
