with Ada.Text_IO; use Ada.Text_IO;

with Ada.Strings.Unbounded;

with Expressions.Container; use Expressions.Container;

with Constants;
with T_Buffer;

package Expressions.File is

   package SU renames Ada.Strings.Unbounded;

   type File_Expr is new Expression with private;

   function Make (F              : File_Expr;
                  Kind_T         : Constants.Lex_Type)
   return File_Expr;

   function Make (F              : File_Expr;
                  F_Name         : SU.Unbounded_String;
                  Dependencies   : Expr_List.Vector;
                  Container      : Container_Expr'Class)
   return File_Expr;

   procedure Parse (F : in out File_Expr;
   Backbone : in out T_Buffer.AST_Backbone'Class);

   procedure Print (F_Expr : File_Expr; F : in out File_Type);

private

   type File_Expr is new Expression with record

      File_Name      : SU.Unbounded_String;
      Dependencies   : Expr_List.Vector;
      Container      : Container_Expr;

   end record;

   procedure Parse_File (F_Expr     : in out File_Expr;
                         Backbone   : in out T_Buffer.AST_Backbone'Class);

   procedure Print_File (F_Expr : File_Expr; F : in out File_Type);

end Expressions.File;