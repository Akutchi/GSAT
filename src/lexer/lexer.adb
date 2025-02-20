with Expressions_List;
with Processing_Utils;

package body Lexer is

   function Lexing (F : File_Type; Input : Stream_Access)
   return T_Buffer.File_Buffer.Vector
   is

      Exprs       : Expressions_List.Expr_List.Vector;
      Buffer      : T_Buffer.Char_Buffer;
      File_Tokens : T_Buffer.File_Buffer.Vector;

      In_Text  : Boolean := False;
      Char     : Character := '$';
   begin

      T_Buffer.Append (File_Tokens, Name (F));

      while not End_Of_File (F) loop

         loop

            Character'Read (Input, Char);

            Processing_Utils.Toogle (In_Text, Char, Buffer.Last);

            exit when Processing_Utils.EOL (Char, In_Text);

            T_Buffer.Append (Buffer, Char);

         end loop;

         T_Buffer.Append (File_Tokens, T_Buffer.Buffer_To_String (Buffer));
         Buffer.Clear;

         if Processing_Utils.EOL (Char, In_Text) then

            T_Buffer.Append (Buffer, Char);
            T_Buffer.Append (File_Tokens, T_Buffer.Buffer_To_String (Buffer));
            Buffer.Clear;

         end if;

         Char := '$';

      end loop;

      return File_Tokens;

   end Lexing;

end Lexer;