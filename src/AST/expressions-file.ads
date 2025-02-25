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

   overriding
   procedure Parse (Expr      : in out File_Expr;
                    V         : Visitor_Int'Class;
                    Backbone  : in out T_Buffer.AST_Backbone'Class);

   overriding
   procedure Print (Expr   : File_Expr;
                    V      : Visitor_Int'Class;
                    F      : in out File_Type);

   overriding
   procedure Accept_v (Expr      : in out File_Expr;
                       V         : Visitor_Int'Class;
                       Backbone  : in out T_Buffer.AST_Backbone'Class);

   overriding
   procedure Accept_v (Expr   : File_Expr;
                       V      : Visitor_Int'Class;
                       F      : in out File_Type);

private

   type File_Expr is new Expression with record

      File_Name      : SU.Unbounded_String;
      Dependencies   : Expr_List.Vector;
      Container      : Container_Expr;

   end record;

end Expressions.File;