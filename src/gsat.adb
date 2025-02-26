with Ada.Text_IO;             use Ada.Text_IO;
with Ada.Exceptions;          use Ada.Exceptions;
with Ada.Calendar;            use Ada.Calendar;

with Ada.Command_Line;

with Expressions.File;     use Expressions.File;
with Exceptions.Arguments; use Exceptions.Arguments;

with T_Buffer;
with Gsat_System;
with Constants;

with Visitor;

procedure Gsat is

   package CLI renames Ada.Command_Line;

   Code_Tokens : T_Buffer.Code_Buffer;
   Non_Textual_Keywords : Constants.Keyword.Map;
   --  Defined here so that it ought not to be re-Init at each file.

   V_Parse : Visitor.Visitor_Parse;
   V_Print : Visitor.Visitor_Print;

begin

   if CLI.Argument_Count > 0 then

      declare
         Src_Path : constant String :=
            Gsat_System.Parse_Src (CLI.Argument (1));

         F : File_Type;

         Start_Time  : Time;
         End_Time    : Time;

      begin

         Gsat_System.Create_Generation_Directory;
         Constants.Init_Map (Non_Textual_Keywords);

         Start_Time := Clock;

         Gsat_System.Show_Grey_Text ("Transpile");
         Gsat_System.Lex_Level (Src_Path, Code_Tokens, Non_Textual_Keywords);

         for File of Code_Tokens.Get_Files loop

            declare

               AST      : File_Expr;
               Backbone : T_Buffer.AST_Backbone := T_Buffer.Make (File, 1);

            begin

               null;

               AST := AST.Make (Constants.source_file_t);
               AST.Accept_v (V_Parse, Backbone);
               AST.Accept_v (V_Print, Constants.NO_TAB, F);

            end;
         end loop;

         End_Time := Clock;
         Gsat_System.Show_Duration (End_Time - Start_Time);
      end;

   else
      Gsat_System.Show_Grey_Text ("Nothing to transpile, abort.");
   end if;

   Put_Line
   ("BEWARE Has_Signature_Changed is put to 'return CHANGED' for dev " &
   "purposes");

exception

   when Constraint_Error => null;
   --  When Code_Tokens.Get_File raise such an error because there is no new
   --  file to lex/parse.

   when E : Argument_Option_Error | Argument_Name_Error =>
   Put_Line (Exception_Message (E));

end Gsat;
