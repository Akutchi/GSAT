with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Text_IO.Text_Streams; use Ada.Text_IO.Text_Streams;

with Expressions;
with Expressions_List;
with T_Buffer;
with Processing_Utils;

procedure Gsat is

   V : Expressions.Expr_Visitor;

   F : File_Type;
   Input : Stream_Access;

   Exprs  : Expressions_List.Expr_List.Vector;
   Buffer : T_Buffer.Char_Buffer;
   In_Text : Boolean := False;

   Char : Character := '$';

begin

   Open
     (File => F,
      Mode => In_File,
      Name => "./src/gsat.adb");

   Input := Stream (F);

   while not End_Of_File (F) loop

      loop

         Character'Read (Input, Char);

         Processing_Utils.Toogle (In_Text, Char, Buffer.Last);

         exit when Processing_Utils.EOL (Char, In_Text);

         T_Buffer.Append (Buffer, Char);

      end loop;

      T_Buffer.Print (Buffer);
      Buffer.Clear;

      if Processing_Utils.EOL (Char, In_Text) then
         T_Buffer.Append (Buffer, Char);
         T_Buffer.Print (Buffer);
         Buffer.Clear;
      end if;

      Char := '$';

   end loop;

end Gsat;
