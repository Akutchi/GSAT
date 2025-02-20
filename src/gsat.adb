with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Text_IO.Text_Streams; use Ada.Text_IO.Text_Streams;

with Expressions;
with T_Buffer;
with Lexer;

procedure Gsat is

   V : Expressions.Expr_Visitor;

   F : File_Type;
   Input : Stream_Access;

   Code_Tokens : T_Buffer.String_Buffer.Vector;

begin

   Open
     (File => F,
      Mode => In_File,
      Name => "./src/gsat.adb");

   Input := Stream (F);

   Code_Tokens := Lexer.Lexing (F, Input);

   T_Buffer.Print (Code_Tokens);

end Gsat;
