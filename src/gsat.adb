with Ada.Command_Line;

with Expressions;
with T_Buffer;
with Gsat_System;

procedure Gsat is

   package CLI renames Ada.Command_Line;

   V : Expressions.Expr_Visitor;

   Code : T_Buffer.Source_Code.Vector;

begin

   if CLI.Argument_Count > 0 then

      declare

         Src_Path : constant String := CLI.Argument (1);

      begin

         Gsat_System.Go_Trough_Level (Src_Path, Code);

         for Code_File of Code loop
            T_Buffer.Print (Code_File);
         end loop;

      end;
   end if;

end Gsat;
