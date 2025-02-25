with Ada.Text_IO; use Ada.Text_IO;

with Ada.Command_Line;

with Expressions.File; use Expressions.File;

with T_Buffer;
with Gsat_System;
with Constants;

with Visitor;

procedure Gsat is

   package CLI renames Ada.Command_Line;

   Code_Tokens : T_Buffer.Code_Buffer;
   Non_Textual_Keywords : Constants.Keyword.Map;
   --  Defined here so that It ought not to be re-Init at each file.

   V_Parse : Visitor.Visitor_Parse;
   V_Print : Visitor.Visitor_Print;

begin

   if CLI.Argument_Count > 0 then

      declare
         Src_Path : constant String := CLI.Argument (1);
         F        : File_Type;

      begin

         Gsat_System.Create_Generation_Directory;
         Constants.Init_Map (Non_Textual_Keywords);

         Put_Line (Standard_Output, "Transpile");
         Gsat_System.Lex_Level (Src_Path, Code_Tokens, Non_Textual_Keywords);

         for File of Code_Tokens.Get_Files loop

            declare

               AST      : File_Expr;
               Backbone : T_Buffer.AST_Backbone := T_Buffer.Make (File, 1);

            begin

               null;

               AST := AST.Make (Constants.file_t);
               AST.Accept_v (V_Parse, Backbone);
               AST.Accept_v (V_Print, F);

            end;
         end loop;
      end;
   end if;

exception

   when Constraint_Error => null;
   --  When Code_Tokens.Get_File raise such an error because there is no new
   --  file to lex/parse.

end Gsat;
