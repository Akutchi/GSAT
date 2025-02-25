with Ada.Text_IO; use Ada.Text_IO;

with Expressions; use Expressions;

with Expressions.File;        use Expressions.File;

with T_Buffer;

package Visitor is

   -----------
   -- Parse --
   -----------

   --  Is public because F / Backbone are already private and have access
   --  methods
   type Visitor_Parse is new Visitor_Int with record

      F        : File_Expr;
      Backbone : T_Buffer.AST_Backbone;

   end record;

   overriding
   procedure Visit_Expr (V : in out Visitor_Parse);

   overriding
   procedure Visit_Expr (V    : Visitor_Parse;
                         Expr : Expression'Class;
                         File : in out File_Type)
   is null;

   -----------
   -- Print --
   -----------

   type Visitor_Print is new Visitor_Int with null record;

   overriding
   procedure Visit_Expr (V : in out Visitor_Print) is null;

   overriding
   procedure Visit_Expr (V    : Visitor_Print;
                         Expr : Expression'Class;
                         File : in out File_Type);

end Visitor;