package body Visitor is

   overriding
   procedure Visit_File (V : in out Visitor_Parse)
   is
   begin
      V.F.Parse (V.Backbone);
   end Visit_File;

   overriding
   procedure Visit_File (V    : Visitor_Print;
                         Expr : Expression'Class;
                         File : in out File_Type)
   is
   begin
      Expr.Print (V, File);
   end Visit_File;

   overriding
   procedure Visit_Dependency (V    : Visitor_Print;
                               Expr : Expression'Class;
                               File : in out File_Type)
   is
   begin
      Expr.Print (V, File);
   end Visit_Dependency;

   overriding
   procedure Visit_Container (V    : Visitor_Print;
                              Expr : Expression'Class;
                              File : in out File_Type)
   is
   begin
      Expr.Print (V, File);
   end Visit_Container;

end Visitor;