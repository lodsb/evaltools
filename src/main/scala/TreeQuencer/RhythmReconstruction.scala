package TreeQuencer

import scalax.collection.Graph
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._
import collection.Set

object RhythmReconstruction {
  def synthNodeClassID(node: Node): Int = {
    // for now it is just the offset in the list
    TreeQuencerLogAnalyzer.synthNodeTypes.indexOf(node.name)
  }

  def numInstruments(): Int = TreeQuencerLogAnalyzer.synthNodeTypes.length

  type Staff = Array[Boolean]

  def createStaff(): Staff = {
    new Array[Boolean](numInstruments())
  }

  type Score = Seq[Array[Boolean]]

  def createScore(): Score = {
    Seq[Staff]()
  }

  def game1(rootNodes: Seq[Node], graph: Graph[Node, DiEdge]): Score = {
    var myScore = createScore()


    // check whether all root nodes are already part of the graph
    var allRootNodesContained = true
    rootNodes.foreach({
      x =>
        allRootNodesContained = allRootNodesContained && graph.contains(x)
    })

    if (allRootNodesContained) {
      // all synth nodes are connected to one of the root nodes (beat 1, beat 2, beat 3, ...)
      rootNodes.foreach({
        root =>
          val currentStaff: Staff = createStaff()
          myScore = myScore :+ currentStaff

          val successors = graph.get(root).diSuccessors

          successors.foreach({
            node =>
              val idx = synthNodeClassID(node)
              if (idx >= 0) currentStaff(synthNodeClassID(node)) = true
          })
      })
    }

    myScore
  }

  def printScore(sc: Score) = {
    val nInst = numInstruments()

    println("############# SCORE #################")

    (0 to nInst - 1).foreach({
      x =>
        var line = ""
        sc.foreach(y => {
          line += (if (y(x)) "X" else "-")
        })
        println(line)
    })
    println("Length: " + sc.length+"\n\n")
  }

  def game0(rootNodes: Seq[Node], graph: Graph[Node, DiEdge]) = game0And2(rootNodes, graph, {
    x => 1
  })

  def game2(rootNodes: Seq[Node], graph: Graph[Node, DiEdge], pulses: (Node) => Int): Score = game0And2(rootNodes, graph, pulses)

  private def game0And2(rootNodes: Seq[Node], graph: Graph[Node, DiEdge], pulses: (Node) => Int): Score = {
    var myScore = createScore()


    // depending on the game the first nodes added to the graph are not root nodes
    if (graph.contains(rootNodes(0))) {

      // only one root node:
      val root = graph.get(rootNodes(0))

      //println("R " + root + " " + root.diSuccessors + " " + root.diPredecessors)


      var branchMap = Map[graph.type#NodeT, (Int, Int, List[graph.type#NodeT])]()
      root.diSuccessors.foreach({
        n =>
        //pulses to go, pulses of branch, list of nodes to visit
          branchMap += (n ->(pulses(n), pulses(n), List(n)))

          println("PULSES FOR BRANCH-NODE " + n.name + " " + pulses(n))
      })

      var done = false

      while (!done) {
        done = true

        val currentStaff = createStaff()
        var nextBranchMap = Map[graph.type#NodeT, (Int, Int, List[graph.type#NodeT])]()

        branchMap.foreach({
          kv =>

            var cL: List[graph.type#NodeT] = List()

            // one pulse less to go
            var pulsesToGo = kv._2._1 - 1
            val pulses = kv._2._2
            val vistingNodes = kv._2._3

            if (pulsesToGo <= 0) {
              vistingNodes.foreach({
                vn =>

                  // center nodes are not part of the synthesizer event graph, so ignore them id == -1
                  val idx = synthNodeClassID(vn)
                  if (idx >= 0) currentStaff(idx  ) = true

                  val nextVisitingNodes = vn.diSuccessors

                  if (!nextVisitingNodes.isEmpty) {
                    cL = cL ++ nextVisitingNodes

                    done = false

                  }
              })

              // reset counter
              pulsesToGo = pulses

              // if all subbranches are done
              // begin again (loop)
              if (cL.isEmpty) {
                cL = List(kv._1)

                done = done && true

              } else {
                done = false
              }


            } else {
              // still have to go some more pulse repetitions until done...
              cL = cL ++ vistingNodes

              done = false
            }

            nextBranchMap += (kv._1 -> (pulsesToGo, pulses, cL))

        })

        branchMap = nextBranchMap

        myScore = myScore :+ currentStaff

      }

    }

    myScore

  }

}
