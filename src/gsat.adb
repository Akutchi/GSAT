with Ada.Text_IO; use Ada.Text_IO;

with Ada.Command_Line;

with Expressions; use Expressions;

with T_Buffer;
with Gsat_System;
with Constants;

procedure Gsat is

   package CLI renames Ada.Command_Line;

   Code_Tokens : T_Buffer.Code_Buffer;
   Non_Textual_Keywords : Constants.Keyword.Map;
   --  Defined here so that It ought not to be re-Init at each file.

   Code_ASTs : Expr_List.Vector;

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

               F_Expr   : File_Expr;
               Backbone : T_Buffer.AST_Backbone := T_Buffer.Make (File,
                                                                  1);
            begin

               F_Expr := F_Expr.Make (Constants.file_t);
               F_Expr.Parse (Backbone);
               Expr_List.Append (Code_ASTs, F_Expr);

            end;
         end loop;

         for File_Node of Code_ASTs loop
            File_Node.Print (F);
         end loop;
      end;
   end if;

exception

   when Constraint_Error => null;
   --  When Code_Tokens.Get_File raise such an error because there is no new
   --  file to lex/parse.

end Gsat;
