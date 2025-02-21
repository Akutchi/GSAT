with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Text_IO.Text_Streams; use Ada.Text_IO.Text_Streams;

with T_Buffer;
with Constants;

package Lexer is

   function Lexing (F : File_Type; Input : Stream_Access;
                   Non_Textual_Keywords : Constants.Keyword.Map)
   return T_Buffer.File_Buffer;

end Lexer;