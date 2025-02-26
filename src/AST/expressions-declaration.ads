package Expressions.Declaration is

   type Declaration_Expr is new Expression with private;

   function Make
      (D          : Declaration_Expr;
       Kind       : Constants.Lex_Type;
       Name       : SU.Unbounded_String;
       Type_Expr  : SU.Unbounded_String;
       Value      : Expr_List.Vector)
   return Declaration_Expr;

   overriding
   procedure Parse (Expr      : in out Declaration_Expr;
                    V         : Visitor_Int'Class;
                    Backbone  : in out T_Buffer.AST_Backbone'Class);

   overriding
   procedure Print (Expr   : Declaration_Expr;
                    V      : Visitor_Int'Class;
                    Tabs   : Natural;
                    F      : in out File_Type);

   overriding
   procedure Accept_v (Expr      : in out Declaration_Expr;
                       V         : Visitor_Int'Class;
                       Backbone  : in out T_Buffer.AST_Backbone'Class);

   overriding
   procedure Accept_v (Expr   : Declaration_Expr;
                       V      : Visitor_Int'Class;
                       Tabs   : Natural;
                       F      : in out File_Type);

private

   type Declaration_Expr is new Expression with record

      Name           : SU.Unbounded_String;
      Type_Expr      : SU.Unbounded_String;
      Value          : Expr_List.Vector;

   end record;

end Expressions.Declaration;