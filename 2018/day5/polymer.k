requires "domains.k"

module POLYMER
   imports DOMAINS
   imports BYTES-HOOKED

  configuration
    <k> $PGM:Polymer </k>
    <result> String2Bytes("") </result>
    <output> "" </output>

   syntax Polymer ::= r"\\$[a-zA-Z]+" [token]
   syntax String ::= Polymer2String(Polymer) [function, functional, hook(STRING.token2string)]

   rule <k> BYTES => substrBytes(BYTES, 1, lengthBytes(BYTES)) ... </k>
        <result> RES => RES[ lengthBytes(RES) <- BYTES[0] ] </result>

endmodule
