requires "domains.k"

module DAY2-SYNTAX
  imports DOMAINS
  imports LIST

    syntax Input ::= Int "," Input | Int
    syntax Ints ::= List{Int, ""}
    syntax Command ::= "#op" KItem "#args" KItem KItem KItem

    syntax  Int ::= count(Input) [function]
    rule count(Int) => 1
    rule count(I, IS) => 1 +Int count(IS)

endmodule

module DAY2
  imports DAY2-SYNTAX
  imports ARRAY

  configuration
    <k> $PGM:Input </k>
    <pc> 0 </pc>
    <log> .K </log>
    
    // Setup
    rule <k> I:Input => I ~> 0 ~> .Map  </k>
    rule <k> I:Int , IS:Input ~> X:Int ~> M:Map => IS ~> X +Int 1 ~> M[X <- I]  </k>
    rule <k> I:Int ~> X:Int ~> M:Map => M[X <- I][1 <- 12][2 <- 2]  </k>
    
    
    rule <k> M:Map => #op M[PC] #args M[M[PC +Int 1]] M[M[PC +Int 2]] M[PC +Int 3] ~> M </k>
         <pc> PC => PC +Int 4 </pc>

    rule <k> #op 1 #args L1 L2 LT ~> M:Map => M[LT <- L1 +Int L2] </k>
         <log> LOG => LOG ~> #op 1 #args L1 L2 LT </log>
    rule <k> #op 2 #args L1 L2 LT ~> M:Map => M[LT <- L1 *Int L2] </k>

endmodule

// Findings:
// Trying to print an empty array (makeEmptyArray(X)) causes a failure, "key not found: #Bottom"
