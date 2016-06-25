package lin567_p2

import lin567_p2.parsers.IOParser

object runIOParser {
  def main( args:Array[String] ) {
    if( args.length != 1 ) {
      println( "Must provide one argument providing the path to the grammar file" )
      System.exit(0)
    }

    val rules = GrammarReader.read( args(0) )
    val cnfRules = GrammarReader.toCNF( rules )
    val pcfg = GrammarReader.randomPCFG( cnfRules )

    println("The random PCFG probabilities are:")
    pcfg.foreach{ case (lhs, rhses) =>
      println( rhses.map{rhs => rhs._1 + "\t" + rhs._2}.mkString( s"$lhs --> ", s"\n$lhs --> ", "\n" ) )
    }

    val ioParser = new IOParser( pcfg )

    while( true ) {
      val line = io.StdIn.readLine( "> " )

      if( line == "quit" || line == null ) {
        System.exit(0)
      } else {
        val s = line.split( " " )
        var ruleCounts = ioParser.insideOutside( s )

        val stringProb = ioParser.insideChart( 0 )( s.length ).values.sum
        println( s"string probability: $stringProb" )

        (0 to s.length-1).foreach{ i =>
          val j = i + 1

          // get sum of (insideProbability * outsideProbability) for each lexical cell; should match
          // stringProb
          val nonTerminals = ioParser.insideChart(i)(j).keys
          val cellProb = nonTerminals.map{ nt =>
              ioParser.insideChart(i)(j)(nt) * ioParser.outsideChart(i)(j)(nt)
          }.sum
          println( (i,j) + "\t" + cellProb )
        }

        ruleCounts.foreach{ case ( rule, count ) =>
          println( s"${rule._1} --> ${rule._2}\t$count" )
        }

      }

    }

  }
}

