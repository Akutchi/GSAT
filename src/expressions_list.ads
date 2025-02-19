with Ada.Containers.Indefinite_Vectors;

with Expressions;

package Expressions_List is

   package Expr_List is new Ada.Containers.Indefinite_Vectors
     (Index_Type   => Positive,
      Element_Type => Expressions.Expression'Class,
      "="          => Expressions."=");

end Expressions_List;