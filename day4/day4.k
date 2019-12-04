module DAY4
  imports DOMAINS

  // The answer should be 966.
  configuration
      <k> $PGM:Prog </k>
      <max> 0 </max>
      <accum> 0 </accum>
      <accum2> 0 </accum2>

  syntax Prog ::= Int "-" Int

  rule <k> MIN - MAX => MIN </k>
       <max> _ => MAX </max>
 
  rule <k> I => I +Int 1 </k>
       <max> MAX </max>
       <accum> ACC => ACC +Int #if increasing(I) andBool duplicates(I) #then 1 #else 0 #fi </accum>
       <accum2> ACC2 => ACC2 +Int #if increasing(I) andBool duplicates2(I) #then 1 #else 0 #fi </accum2>
    requires I <=Int MAX

    syntax Bool ::= increasing( Int ) [function]
    syntax Bool ::= decreasingBack(Int, Int ) [function]
 // --------------------------------------------------- 
    rule increasing(N) => decreasingBack(N modInt 10, N divInt 10)
    rule decreasingBack(LAST, N) => LAST >=Int N requires N <Int 10
    rule decreasingBack(LAST, N) => LAST >=Int (N modInt 10) andBool decreasingBack(N modInt 10, N divInt 10) requires N >=Int 10

    syntax Bool ::= duplicates(Int) [function]
 // -----------------------------
    rule duplicates(N) => false requires N <Int 10
    rule duplicates(N) => (N modInt 10 ==Int (N divInt 10) modInt 10) orBool duplicates(N divInt 10)
      requires N >=Int 10

    syntax Bool ::= duplicates2(Int) [function]
    syntax  Int ::= strip(Int, Int) [function]
 // -----------------------------
    rule duplicates2(N) => false requires N <Int 10
    rule duplicates2(N) => #if N modInt 10 ==Int (N divInt 10) modInt 10   andBool   N modInt 10 ==Int (N divInt 100) modInt 10 //Lookahead.
                          #then duplicates2(strip (N modInt 10, N))
                          #else N modInt 10 ==Int (N divInt 10) modInt 10    orBool duplicates2(N divInt 10)
                          #fi
      requires N >=Int 10
                
    rule strip(I, 0) => 0
    rule strip(I, N) => #if N modInt 10 ==Int I #then strip(I, N divInt 10) #else N #fi

endmodule

