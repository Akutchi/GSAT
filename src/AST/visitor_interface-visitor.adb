package body Visitor_Interface.Visitor is

   overriding
   procedure Visit_File (V : in out Visitor_Parse)
   is
   begin
      V.F.Parse (V.Backbone);
   end Visit_File;

   overriding
   procedure Visit_File (V : in out Visitor_Print; File : in out File_Type)
   is
   begin
      V.AST.Print (File);
   end Visit_File;

end Visitor_Interface.Visitor;