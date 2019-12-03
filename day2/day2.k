requires "domains.k"

module DAY2-SYNTAX
  imports DOMAINS
  imports LIST

    syntax Input ::= Int "," Input | Int
    syntax Ints ::= List{Int, ""}
    syntax Command ::= "#load" | Op ":" Params
    syntax Op ::= TernOp | "Halt"
    syntax TernOp ::= "Mul" | "Add"
    syntax Params ::= KItem "x" KItem ">" KItem

    syntax Op ::= toOp(KItem) [function]
    rule toOp(1) => Add
    rule toOp(2) => Mul
    rule toOp(99) => Halt

endmodule

module DAY2
  imports DAY2-SYNTAX
  imports ARRAY

  configuration
    <k> $PGM:Input </k>
    <mem> .Map </mem>
    <pc> 0 </pc>
    <log> .K </log>
    
    // Setup: Load the list into memory.
    rule <k> I:Input => I ~> 0 ~> .Map  </k>
    rule <k> I:Int , IS:Input ~> X:Int ~> M:Map => IS ~> X +Int 1 ~> M[X <- I]  </k>
    rule <k> I:Int ~> X:Int ~> M:Map => . </k>
         <mem> _ => M[X <- I][1 <- 12][2 <- 2] </mem>
    
    // Load next operation.
    rule <k> . => toOp(M[PC]) </k>
         <mem> M </mem>
         <pc> PC => PC +Int 1 </pc>
    
    /// Load params.
    rule <k> OP:TernOp => OP : M[M[PC]]  x  M[M[PC +Int 1]] > M[PC +Int 2] ... </k>
         <mem> M </mem>
         <pc> PC => PC +Int 3 </pc>

    // Perform operation.
    rule <k> Add : X x Y > LOC => . ... </k>
         <mem> M => M[LOC <- X +Int Y] </mem>

    rule <k> Mul : X x Y > LOC => . ... </k>
         <mem> M => M[LOC <- X *Int Y] </mem>

endmodule

// Findings:
// Trying to print an empty array (makeEmptyArray(X)) causes a failure, "key not found: #Bottom"
