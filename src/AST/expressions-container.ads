with Ada.Strings.Unbounded;

with Constants;
with T_Buffer;

package Expressions.Container is

   package SU renames Ada.Strings.Unbounded;

   type Container_Expr is new Expression with private;

   function Make
      (C                   : Container_Expr;
       Kind_T              : Constants.Lex_Type;
       Name                : SU.Unbounded_String;
       Has_Body            : Boolean;
       Declarations        : Expr_List.Vector;
       Body_Expr           : Expr_List.Vector)
   return Container_Expr;

   procedure Parse (C         : in out Container_Expr;
                   Backbone   : in out T_Buffer.AST_Backbone'Class);

   overriding
   procedure Print (Expr   : Container_Expr;
                    V      : Visitor_Int'Class;
                    F      : in out File_Type);

   overriding
   procedure Accept_v (Expr   : Container_Expr;
                       V      : Visitor_Int'Class;
                       F      : in out File_Type);

private

   type Container_Expr is new Expression with record

      Name           : SU.Unbounded_String;
      Has_Body       : Boolean;
      Declarations   : Expr_List.Vector;
      Body_Expr      : Expr_List.Vector;

   end record;

   procedure Parse_Package (C         : in out Container_Expr;
                           Backbone   : in out T_Buffer.AST_Backbone'Class);

   procedure Print_Package (C : Container_Expr; V : Visitor_Int'Class;
   F : in out File_Type);

end Expressions.Container;