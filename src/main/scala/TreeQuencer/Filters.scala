package TreeQuencer
import SessionLogLoader._

object Filters {

  val NodeCreationFilter: SessionLogLoader.RowFilter = { row =>
      row(SessionLogLoader.RowEvtType) == "Event" && row(SessionLogLoader.RowMsg) == "Created Node"
  }

  val NodeRemovalFilter: SessionLogLoader.RowFilter = { row =>
      row(SessionLogLoader.RowEvtType) == "Event" && row(SessionLogLoader.RowMsg) == "Removed Node"
  }

  val NodeSetAddFilter: SessionLogLoader.RowFilter = { row =>
      row(SessionLogLoader.RowEvtType) == "Event" && row(SessionLogLoader.RowMsg) == "Added node to set"
  }

  val NodeSetRemovalFilter: SessionLogLoader.RowFilter = { row =>
      row(SessionLogLoader.RowEvtType) == "Event" && row(SessionLogLoader.RowMsg) == "Removed node from set"
  }

  val BeginGestureFilter: SessionLogLoader.RowFilter = { row =>
      row(SessionLogLoader.RowEvtType) == "BeginGesture"
  }

  val EndGestureFilter: SessionLogLoader.RowFilter = { row =>
      row(SessionLogLoader.RowEvtType) == "EndGesture"
  }

}
