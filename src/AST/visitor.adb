package body Visitor is

   overriding
   procedure Visit_Expr (V : in out Visitor_Parse)
   is
   begin
      V.F.Parse (V.Backbone);
   end Visit_Expr;

   overriding
   procedure Visit_Expr (V    : Visitor_Print;
                         Expr : Expression'Class;
                         File : in out File_Type)
   is
   begin
      Expr.Print (V, File);
   end Visit_Expr;

end Visitor;