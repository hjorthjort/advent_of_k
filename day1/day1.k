module DAY1
  imports DOMAINS

  configuration
  <k> $PGM:Input </k>
  <out> 0 </out>
  <k2> $PGM:Input </k2>
  <out2> 0 </out2>

  syntax Input ::= List{Int, ""}
  
  rule <k> I:Int IS:Input => IS </k>
       <out> O => O +Int ((I /Int 3) -Int 2) </out>

  rule <k2> I:Int IS:Input => IS </k2> 
    requires I <Int 9 // If mass is less than 9, fuel is 0 or negative.

  rule <k2> I:Int IS:Input => (I /Int 3 -Int 2) IS </k2>
       <out2> O => O +Int I /Int 3 -Int 2 </out2>
    requires I >=Int 9

endmodule
