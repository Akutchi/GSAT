with Ada.Strings.Unbounded;

with Constants;
with T_Buffer;

package Expressions.Dependency is

   package SU renames Ada.Strings.Unbounded;

   type Dependency_Expr is new Expression with private;

   function Make
      (D                   : Dependency_Expr;
       Kind_T              : Constants.Lex_Type;
       With_Str, Use_Str   : SU.Unbounded_String := SU.Null_Unbounded_String)
   return Dependency_Expr;

   overriding
   procedure Parse (Expr      : in out Dependency_Expr;
                    V         : Visitor_Int'Class;
                    Backbone  : in out T_Buffer.AST_Backbone'Class);

   overriding
   procedure Print (Expr   : Dependency_Expr;
                    V      : Visitor_Int'Class;
                    Tabs   : Natural;
                    F      : in out File_Type);

   overriding
   procedure Accept_v (Expr      : in out Dependency_Expr;
                       V         : Visitor_Int'Class;
                       Backbone  : in out T_Buffer.AST_Backbone'Class);

   overriding
   procedure Accept_v (Expr   : Dependency_Expr;
                       V      : Visitor_Int'Class;
                       Tabs   : Natural;
                       F      : in out File_Type);

private

   type Dependency_Expr is new Expression with record

      With_Str : SU.Unbounded_String;
      Use_Str  : SU.Unbounded_String;

   end record;

   procedure Get_Until_Semicolon
      (Str      : in out SU.Unbounded_String;
       Backbone : in out T_Buffer.AST_Backbone'Class);

end Expressions.Dependency;