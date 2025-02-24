with Expressions.File;        use Expressions.File;

with T_Buffer;

package Visitor_Interface.Visitor is

   --  Is public because F / Backbone are already private and have access
   --  methods
   type Visitor_Parse is new Visitor_Int with record

      F        : File_Expr;
      Backbone : T_Buffer.AST_Backbone;

   end record;

   overriding
   procedure Visit_File (V : in out Visitor_Parse);

   overriding
   procedure Visit_File (V : in out Visitor_Parse; File : in out File_Type)
   is null;

   type Visitor_Print is new Visitor_Int with record

      AST      : File_Expr;

   end record;

   overriding
   procedure Visit_File (V : in out Visitor_Print) is null;

   overriding
   procedure Visit_File (V : in out Visitor_Print; File : in out File_Type);

end Visitor_Interface.Visitor;