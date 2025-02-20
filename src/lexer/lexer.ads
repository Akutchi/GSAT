with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Text_IO.Text_Streams; use Ada.Text_IO.Text_Streams;

with T_Buffer;

package Lexer is

   function Lexing (F : File_Type; Input : Stream_Access)
   return T_Buffer.File_Buffer.Vector;

end Lexer;