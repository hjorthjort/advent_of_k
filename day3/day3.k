module DAY3-SYNTAX
  imports DOMAINS

    syntax Input ::= Wire Wire
    syntax Wire ::= List{Move, ","}
    syntax Direction ::= "L" | "R" | "U" | "D"
    syntax Move ::= Direction Int

    syntax Coord ::= "(" Int "," Int ")"

    syntax Content ::= "one" | "two" | "cross"
    
endmodule

module DAY3
  imports DAY3-SYNTAX

  configuration
    <k> $PGM:Input </k>
    <map1> .Map </map1>
    <map2> .Map </map2>
    <xy> (0, 0) </xy>

    // Store wire 1

    rule <k> D (N => N -Int 1) , W1:Wire W2:Wire </k>
         <map1> MAP => MAP[(X, Y -Int 1) <- one] </map1>
         <xy> (X, Y => Y -Int 1) </xy>
      requires N >Int 0

    rule <k> U (N => N -Int 1) , W1:Wire W2:Wire </k>
         <map1> MAP => MAP[(X, Y +Int 1) <- one] </map1>
         <xy> (X, Y => Y +Int 1) </xy>
      requires N >Int 0

    rule <k> L (N => N -Int 1) , W1:Wire W2:Wire </k>
         <map1> MAP => MAP[(X -Int 1, Y) <- one] </map1>
         <xy> (X => X -Int 1, Y) </xy>
      requires N >Int 0

    rule <k> R (N => N -Int 1) , W1:Wire W2:Wire </k>
         <map1> MAP => MAP[(X +Int 1, Y) <- one] </map1>
         <xy> (X => X +Int 1, Y) </xy>
      requires N >Int 0

    rule <k> _ 0 , W1:Wire W2:Wire => W1 W2:Wire </k>
    rule <k> .Wire W2:Wire => W2:Wire </k>
         <xy> _ => (0, 0) </xy>

    // Store wire 2

    rule <k> D (N => N -Int 1) , W1:Wire </k>
         <map2> MAP => MAP[(X, Y -Int 1) <- one] </map2>
         <xy> (X, Y => Y -Int 1) </xy>
      requires N >Int 0

    rule <k> U (N => N -Int 1) , W1:Wire </k>
         <map2> MAP => MAP[(X, Y +Int 1) <- one] </map2>
         <xy> (X, Y => Y +Int 1) </xy>
      requires N >Int 0

    rule <k> L (N => N -Int 1) , W1:Wire </k>
         <map2> MAP => MAP[(X -Int 1, Y) <- one] </map2>
         <xy> (X => X -Int 1, Y) </xy>
      requires N >Int 0

    rule <k> R (N => N -Int 1) , W1:Wire </k>
         <map2> MAP => MAP[(X +Int 1, Y) <- one] </map2>
         <xy> (X => X +Int 1, Y) </xy>
      requires N >Int 0

    rule <k> _ 0 , W2:Wire => W2:Wire </k>

    // Find duplicates.
    rule <k> .Wire => #duplicates </k>

    syntax K ::= "#duplicates"

    rule <k> #duplicates => #duplicates ~> K ... </k>
         <map1> K |-> _ M1 => M1 </map1>
         <map2> M2 => M2[K <- undef] </map2>
    requires K in_keys(M2)

    rule <k> #duplicates ... </k>
         <map1> K |-> _ M1 => M1 </map1>
         <map2> M2 => M2[K <- undef] </map2>
    requires notBool K in_keys(M2)


    rule <k> #duplicates => . ... </k>
         <map1> M1 </map1>
         <map2> M2 </map2>
      requires M1 ==K .Map orBool M2 ==K .Map


    // Find the duplicate closest to origin.
    rule <k> (X, Y):Coord ~> (X', Y'):Coord => #if absInt(X) +Int absInt(Y) <Int absInt(X') +Int absInt(Y') #then (X, Y) #else (X', Y') #fi ... </k>

    rule <k> (X, Y) => absInt(X) +Int absInt(Y) </k>

endmodule
