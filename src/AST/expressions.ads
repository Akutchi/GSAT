with Ada.Strings.Unbounded;
with Ada.Containers.Indefinite_Vectors;

with Constants; use Constants;

package Expressions is

   package SU renames Ada.Strings.Unbounded;

   type Expression is abstract tagged record
      Kind_T : Constants.Lex_Type;
   end record;

   package Expr_List is new Ada.Containers.Indefinite_Vectors
     (Index_Type   => Positive,
      Element_Type => Expression'Class,
      "="          => "=");

end Expressions;