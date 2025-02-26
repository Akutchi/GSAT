with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with T_Buffer;

package Expressions_Utils is

   package SU renames Ada.Strings.Unbounded;

   function Get_Dotted_Name (Backbone   : in out T_Buffer.AST_Backbone'Class;
                             Is_Comment : Boolean := False)
   return SU.Unbounded_String;

end Expressions_Utils;