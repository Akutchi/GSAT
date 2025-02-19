with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Text_IO.Text_Streams; use Ada.Text_IO.Text_Streams;

with Expressions;
with Expressions_List;
with T_Buffer;
with Processing_Utils;

procedure Gbt is

   V : Expressions.Expr_Visitor;

   F : File_Type;
   Input : Stream_Access;

   Exprs  : Expressions_List.Expr_List.Vector;
   Buffer : T_Buffer.Char_Buffer;

   Char : Character := '$';
   Test : String := "(lol)";
   In_Text : Boolean := False;

begin

   Open
     (File => F,
      Mode => In_File,
      Name => "./src/gbt.adb");

   Input := Stream (F);

   while not End_Of_File (F) loop

      loop

         Character'Read (Input, Char);

         exit when Processing_Utils.EOL (Char);

         T_Buffer.Append (Buffer, Char);

      end loop;

      T_Buffer.Print (Buffer);
      Buffer.Clear;

      if Char = '(' or else Char = ')' then
         T_Buffer.Append (Buffer, Char);
         T_Buffer.Print (Buffer);
         Buffer.Clear;
      end if;

      Char := '$';

   end loop;

end Gbt;
