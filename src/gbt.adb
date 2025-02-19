with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Text_IO.Text_Streams; use Ada.Text_IO.Text_Streams;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;

with Expressions;
with Expressions_List;
with T_Buffer;
procedure Gbt is

   V : Expressions.Expr_Visitor;

   F : File_Type;
   Input : Stream_Access;

   Exprs  : Expressions_List.Expr_List.Vector;
   Buffer : T_Buffer.Char_Buffer;

   Char : Character := ' ';

begin

   Open
     (File => F,
      Mode => In_File,
      Name => "./test/test.adb");

   Input := Stream (F);

   while not End_Of_File (F) loop

      while Char /= ';' and then Char /= LF and then not Buffer.Has_Keyword loop

         Character'Read (Input, Char);
         T_Buffer.Append (Buffer, Char);

      end loop;

      T_Buffer.Print (Buffer);

      Buffer.Clear;
      Char := ' ';

   end loop;

end Gbt;
