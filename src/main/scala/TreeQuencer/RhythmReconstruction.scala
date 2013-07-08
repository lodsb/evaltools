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

  def numInstruments() : Int = TreeQuencerLogAnalyzer.synthNodeTypes.length

  type Staff = Array[Boolean]
  def createStaff(): Staff = {
    new Array[Boolean](numInstruments())
  }

  type Score = Seq[Array[Boolean]]
  def createScore() : Score = {
    Seq[Staff]()
  }

  def game1(rootNodes: Seq[Node], graph: Graph[Node, DiEdge]) : Score = {
    var myScore = createScore()

    // all synth nodes are connected to one of the root nodes (beat 1, beat 2, beat 3, ...)
    rootNodes.foreach({ root =>
      val currentStaff = createStaff()
      myScore +: currentStaff

      val successors = graph.get(root).diSuccessors

      successors.foreach( { node =>
        currentStaff(synthNodeClassID(node)) = true
      })
    })

    myScore
  }

  def printScore(sc: Score) = {
    val nInst = numInstruments()

    println("############# SCORE #################")

    (0 to nInst-1).foreach({x=>
      var line = ""
      sc.foreach(y => {
        line += ( if (y(x)) "X" else "-" )
      })
      println(line)
    })
  }

  def game0And2(rootNodes: Seq[Node], graph: Graph[Node, DiEdge], pulses: (Node) => Int) : Score = {
    var myScore = createScore()


    // only one root node:
    val root = graph.get(rootNodes(0))

    println("R "+root + " " + root.diSuccessors + " " + root.diPredecessors)

    var branchMap = Map[graph.type#NodeT, (Int, Int,List[graph.type#NodeT])]()

    root.diSuccessors.foreach({ n =>
                         //pulses to go, pulses normal, list of nodes to visit
      branchMap += ( n -> (pulses(n), pulses(n),List(n)) )
    })

    var branchesToGo = 42

    while(branchesToGo > 0) {
      branchesToGo = branchMap.size

      val currentStaff = createStaff()
      var nextBranchMap = Map[graph.type#NodeT, (Int, Int,List[graph.type#NodeT])]()

      branchMap.foreach({ kv =>


        var cL: List[graph.type#NodeT] = List()

        // one pulse less to go
        var pulsesToGo = kv._2._1 - 1
        val pulses = kv._2._2
        val vistingNodes = kv._2._3

        if (pulsesToGo <= 0) {
          vistingNodes.foreach({ vn =>
            currentStaff(synthNodeClassID(vn)) = true

            val nextVisitingNodes = vn.diSuccessors

            if(!nextVisitingNodes.isEmpty) {
              cL = cL ++ nextVisitingNodes


            } else {
              // begin again (loop)
              cL = cL ++ List(kv._1)

              branchesToGo = branchesToGo - 1

            }

          })

          // reset counter
          pulsesToGo = pulses

        }

        nextBranchMap += (kv._1 -> (pulsesToGo, pulses, cL) )

      })

      branchMap = nextBranchMap

      myScore = myScore :+ currentStaff

    }

    myScore

  }

}
