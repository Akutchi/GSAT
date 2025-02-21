with Ada.Command_Line;
with Ada.Strings.Unbounded;

with Expressions; use Expressions;

with Expressions_List;
with T_Buffer;
with Gsat_System;
with Constants;

procedure Gsat is

   package CLI renames Ada.Command_Line;
   package SU renames Ada.Strings.Unbounded;

   V : Expressions.Visitor;

   Code_Tokens : T_Buffer.Code_Buffer;
   Non_Textual_Keywords : Constants.Keyword.Map;
   --  Defined here so that It ought not to be re-Init at each file.
   Code_ASTs : Expressions_List.Expr_List.Vector;

begin

   if CLI.Argument_Count > 0 then

      declare

         Src_Path : constant String := CLI.Argument (1);

      begin

         Constants.Init_Map (Non_Textual_Keywords);
         Gsat_System.Lex_Level (Src_Path, Code_Tokens, Non_Textual_Keywords);

         for File of Code_Tokens.Get_Files loop

            declare

               F_Name : constant SU.Unbounded_String :=
                  SU.To_Unbounded_String
                     (T_Buffer.Buffer_To_String (File.Get (1)));

               F : File_Expr := Make (Constants.file_t, F_Name);

            begin

               F.Parse (V);
               Expressions_List.Expr_List.Append (Code_ASTs, F);

            end;
         end loop;

      end;
   end if;

end Gsat;
