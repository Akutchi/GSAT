with Ada.Command_Line;

with Expressions;
with T_Buffer;
with Gsat_System;
with Constants;

procedure Gsat is

   package CLI renames Ada.Command_Line;

   V : Expressions.Expr_Visitor;

   Code : T_Buffer.Code_Buffer;
   Non_Textual_Keywords : Constants.Keyword.Map;
   --  Defined here so that It ought not to be re-Init at each file.

begin

   if CLI.Argument_Count > 0 then

      declare

         Src_Path : constant String := CLI.Argument (1);

      begin

         Constants.Init_Map (Non_Textual_Keywords);
         Gsat_System.Lex_Level (Src_Path, Code, Non_Textual_Keywords);

         --  T_Buffer.Print (Code);

      end;
   end if;

end Gsat;
