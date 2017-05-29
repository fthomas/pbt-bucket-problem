/**
  * Created by thomasfr on 19.05.2017.
  */
object bp {
  sealed trait Step extends Product with Serializable
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

}
