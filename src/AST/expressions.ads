with Ada.Text_IO; use Ada.Text_IO;

with Ada.Strings.Unbounded;
with Ada.Containers.Indefinite_Vectors;

with Constants; use Constants;

with T_Buffer;

package Expressions is

   package SU renames Ada.Strings.Unbounded;

   type Expression is abstract tagged record
      Kind_T : Constants.Lex_Type;
   end record;

   type Visitor_Int is abstract tagged private;

   procedure Parse (Expr      : in out Expression;
                    V         : Visitor_Int'Class;
                    Backbone  : in out T_Buffer.AST_Backbone'Class)
   is abstract;

   procedure Print (Expr   : Expression;
                    V      : Visitor_Int'Class;
                    F      : in out File_Type)
   is abstract;

   procedure Accept_v (Expr      : in out Expression;
                       V         : Visitor_Int'Class;
                       Backbone  : in out T_Buffer.AST_Backbone'Class)
   is abstract;

   procedure Accept_v (Expr   : Expression;
                       V      : Visitor_Int'Class;
                       F      : in out File_Type)
   is abstract;

    procedure Visit_Expr (V         : Visitor_Int;
                         Expr       : in out Expression'Class;
                         Backbone   : in out T_Buffer.AST_Backbone'Class)
   is abstract;

   procedure Visit_Expr (V    : Visitor_Int;
                         Expr : Expression'Class;
                         File : in out File_Type)
   is abstract;

   package Expr_List is new Ada.Containers.Indefinite_Vectors
     (Index_Type   => Positive,
      Element_Type => Expression'Class,
      "="          => "=");

private

   type Visitor_Int is abstract tagged null record;

end Expressions;