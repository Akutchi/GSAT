with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Text_IO.Text_Streams; use Ada.Text_IO.Text_Streams;

with Ada.Command_Line;

with Expressions;
with T_Buffer;
with Lexer;
with Gsat_System;

procedure Gsat is

   package CLI renames Ada.Command_Line;

   V : Expressions.Expr_Visitor;

   F : File_Type;
   Input : Stream_Access;

   Code_Tokens : T_Buffer.String_Buffer.Vector;

begin

   if CLI.Argument_Count > 0 then

      declare

         Src_Path : constant String := CLI.Argument (1);

      begin

         Gsat_System.Go_Trough_Level (Src_Path);

         Open
         (File => F,
            Mode => In_File,
            Name => "./src/gsat.adb");

         Input := Stream (F);

         Code_Tokens := Lexer.Lexing (F, Input);

         T_Buffer.Print (Code_Tokens);

      end;

   end if;

end Gsat;
