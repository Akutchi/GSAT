with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;

with Processing_Utils;

package body Lexer is

   function Lexing (F : File_Type; Input : Stream_Access;
                    Non_Textual_Keywords : Constants.Keyword.Map)
   return T_Buffer.File_Buffer
   is

      File_Tokens : T_Buffer.File_Buffer;

      In_Text, Is_Comment  : Boolean := False;
      Char                 : Character := '$';

      Name_Buffer : T_Buffer.Char_Buffer;

   begin

      Name_Buffer.Append (Name (F));
      Name_Buffer.Freeze (Using => Non_Textual_Keywords);
      File_Tokens.Append (Name_Buffer);

      while not End_Of_File (F) loop

         declare
            Buffer : T_Buffer.Char_Buffer;
         begin

            loop

               Character'Read (Input, Char);

               Processing_Utils.Toogle (In_Text, Char, Buffer.Last);

               exit when Processing_Utils.EOL (Char, In_Text);

               Buffer.Append (Char);

            end loop;

            Buffer.Freeze (Using => Non_Textual_Keywords);
            File_Tokens.Append (Buffer);

            if Buffer.Kind = Constants.comment_t then
               Is_Comment := True;
            end if;

            if Char = LF and then Is_Comment then
               declare
                  End_Comment_Buffer : T_Buffer.Char_Buffer;
               begin
                  End_Comment_Buffer.Append ("--");
                  End_Comment_Buffer.Freeze (Using => Non_Textual_Keywords);
                  File_Tokens.Append (End_Comment_Buffer);
                  Is_Comment := False;
               end;
            end if;

            if Processing_Utils.EOL (Char, In_Text) then

               declare
                  Buffer : T_Buffer.Char_Buffer;
               begin
                  Buffer.Append (Char);
                  Buffer.Freeze (Using => Non_Textual_Keywords);
                  File_Tokens.Append (Buffer);
               end;
            end if;

            Char := '$';

         end;
      end loop;

      return File_Tokens;

   end Lexing;

end Lexer;