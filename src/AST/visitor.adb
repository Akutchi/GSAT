package body Visitor is

   overriding
   procedure Visit_Expr (V          : Visitor_Parse;
                         Expr       : in out Expression'Class;
                         Backbone   : in out T_Buffer.AST_Backbone'Class)
   is
   begin
      Expr.Parse (V, Backbone);
   end Visit_Expr;

   overriding
   procedure Visit_Expr (V    : Visitor_Print;
                         Expr : Expression'Class;
                         Tabs : Natural;
                         File : in out File_Type)
   is
   begin
      Expr.Print (V, Tabs, File);
   end Visit_Expr;

end Visitor;