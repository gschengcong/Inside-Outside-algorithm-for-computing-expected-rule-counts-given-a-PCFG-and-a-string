package lin567_p2.parsers

import lin567_p2.Grammar._

class IOParser( pcfg:Map[NonTerminal,Map[RHS,Double]] ) {

  // For convenience, we want a reversal of the rules map; given an expansion, what are the possible
  // dominating non-terminals?
  var reversePCFG = Map[RHS,Map[NonTerminal,Double]]()

  pcfg.foreach{ case ( lhs, rhses ) =>
    rhses.foreach{ case ( rhs, prob ) =>
      if( reversePCFG.isDefinedAt( rhs ) ) {
        reversePCFG += rhs -> ( reversePCFG( rhs ) + ( lhs -> prob ) )
      } else {
        reversePCFG += rhs -> Map( lhs -> prob )
      }
    }
  }

  val maxLength = 20

  // Arrays are always mutable!
  var insideChart = Array.fill( maxLength, maxLength )( Map[NonTerminal,Double]().withDefaultValue(0D) )
  var outsideChart = Array.fill( maxLength, maxLength )( Map[NonTerminal,Double]().withDefaultValue(0D) )

  def lexFill( word:String, i:Int ) {
    val rhs = RHS( Terminal( word ) )
    reversePCFG.getOrElse( rhs, Map() ).foreach{ case ( pos, prob ) =>
      insideChart( i )( i+1 ) += pos -> prob
    }
  }

  def synFill( i:Int, j:Int ) {
    ( (i+1) to (j-1) ).foreach{ k =>
      val leftChildren = insideChart( i )( k )
      val rightChildren = insideChart( k )( j )

      leftChildren.foreach{ case ( lNode, lProb ) =>
        rightChildren.foreach{ case ( rNode, rProb ) =>
          val rhs = RHS( lNode, rNode )

          reversePCFG.getOrElse( rhs, Map() ).foreach{ case ( pNode, ruleProb ) =>
            if( insideChart( i )( j ).contains( pNode ) ) {
              insideChart( i )( j ) += pNode -> (
                insideChart( i )( j )( pNode ) + ( 
                  lProb * rProb * ruleProb
                )
              )
            } else {
              insideChart( i )( j ) += pNode -> ( lProb * rProb * ruleProb )
            }

          }
        }
      }
    }
  }

  def outsidePassWithCounts( s:Array[String] ) = {
    // Outside probability is 1
    // Assume start symbol is "S"
    outsideChart( 0 )( s.length ) += NonTerminal( "S" ) -> 1.0

    var ruleCounts = Map[Tuple2[NonTerminal,RHS],Double]().withDefaultValue(0D)

    ( 1 to s.length).reverse.foreach{ spanLength =>
      (0 to (s.length-spanLength)).foreach{ i =>
        val j = i + spanLength
        val parents = outsideChart( i )( j )

        (i+1 to j-1).foreach{ k =>
          val leftChildren = insideChart( i )( k )
          val rightChildren = insideChart( k )( j )

          parents.foreach{ case (pNode, outsideProb ) =>

            leftChildren.foreach{ case ( lNode, lInsideProb ) =>
              rightChildren.foreach{ case ( rNode, rInsideProb ) =>
                val rhs = RHS( lNode, rNode )

                if( pcfg( pNode ).isDefinedAt( rhs ) ) {
                  val ruleProb = pcfg( pNode )( rhs )

                  val leftSummand = ruleProb * outsideProb * rInsideProb
                  val rightSummand = ruleProb * outsideProb * lInsideProb

                  outsideChart( i )( k ) += lNode -> (
                      outsideChart( i )( k )( lNode ) + leftSummand
                    )


                  outsideChart( k )( j ) += rNode -> (
                      outsideChart( k )( j )( rNode ) + rightSummand
                    )

                  // Now, compute the marginal probability of the rule occurring with the parent
                  // node spanning from i to j, the left child spanning from i to k, and the right
                  // child spanning from k to j, and increment the expected count by this
                  // probability.
                  val ruleMarginal = insideChart(i)(k)(lNode) * insideChart(k)(j)(rNode) * ruleProb * outsideProb / insideChart(0)(s.length)(NonTerminal( "S" ))

                  ruleCounts += ( pNode, rhs ) -> (
                    ruleCounts( (pNode,rhs) ) + ruleMarginal
                  )

                }

              }

            }

          }

        }
      }
    }

    ruleCounts
  }

  def initializeChart( length:Int ) {
    insideChart = Array.fill( maxLength, maxLength+1 )( Map[NonTerminal,Double]().withDefaultValue(0) )
    outsideChart = Array.fill( maxLength, maxLength+1 )( Map[NonTerminal,Double]().withDefaultValue(0) )
  }

  def insidePass( s:Array[String] ) {
    // first clean the chart
    initializeChart( s.length )

    (1 to s.length).foreach{ j =>
      lexFill( s( j-1 ), j-1 )

      if( j > 1 ) {
        (0 to (j-2)).reverse.foreach{ i =>
          synFill( i, j )
        }
      }
    }

  }

  def insideOutside( s:Array[String] ) = {
    insidePass( s )
    outsidePassWithCounts( s )
  }


}

