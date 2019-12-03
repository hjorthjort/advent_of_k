requires "domains.k"

module DAY2-SYNTAX
  imports DOMAINS
  imports LIST

   syntax Version ::= "part1" | "part2"

    syntax Input ::= Int "," Input | Int
    syntax Command ::= "#done" | Op ":" Params
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
    <version> part2 </version>
    <mem> .Map </mem>
    <pc> 0 </pc>
    <originalMem> .Map </originalMem>
    <variable> 0 </variable>
    
    // Setup: Load the list into memory.
    rule <k> I:Input => I ~> 0 ~> .Map  </k>
    rule <k> I:Int , IS:Input ~> X:Int ~> M:Map => IS ~> X +Int 1 ~> M[X <- I]  </k>

    rule <k> I:Int ~> X:Int ~> M:Map => . </k>
         <mem> _ => M[X <- I][1 <- 12][2 <- 2] </mem>
         <version> part1 </version>

    rule <version> part2 </version>
         <k> I:Int ~> X:Int ~> M:Map => Halt </k>
         <originalMem> _ => M[X <- I] </originalMem>
         <variable> V </variable>
    
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

    // Search for correct value.
    rule <version> part2 </version>
         <k> Halt => . ... </k>
         <pc> _ => 0 </pc>
         <mem> M => OM[1 <- V /Int 100][2 <- V modInt 100] </mem>
         <originalMem> OM </originalMem>
         <variable> V => V +Int 1 </variable>
      requires M[0] =/=K 19690720
       andBool V <Int 9999

    rule <version> part2 </version>
         <k> Halt => #done ... </k>
         <mem> M </mem>
         <originalMem> _ => .Map </originalMem>
         <variable> V => V -Int 1 </variable>
      requires M[0] ==K 19690720

endmodule

// Findings:
// Trying to print an empty array (makeEmptyArray(X)) causes a failure, "key not found: #Bottom"
