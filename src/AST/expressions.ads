with Ada.Strings.Unbounded;
with Ada.Containers.Indefinite_Vectors;

with Constants; use Constants;

with T_Buffer;

package Expressions is

   package SU renames Ada.Strings.Unbounded;

   type Expression is tagged record
      Kind_T : Constants.Lex_Type;
   end record;

   package Expr_List is new Ada.Containers.Indefinite_Vectors
     (Index_Type   => Positive,
      Element_Type => Expression'Class,
      "="          => "=");

   type Visitor is tagged null record;

   procedure Parse (V : in out Visitor; Exp : in out Expression'Class;
                    Backbone : in out T_Buffer.AST_Backbone);

   procedure Print (V : in out Visitor; Exp : Expression'Class);

   procedure Parse (Exp : in out Expression'Class; V : in out Visitor;
                    Backbone : in out T_Buffer.AST_Backbone);

   procedure Print (Exp : Expression'Class; V : in out Visitor);

   type Dependency_Expr is new Expression with private;

   type Container_Expr is new Expression with private;

   type File_Expr is new Expression with private;

   function Make
      (D                   : Dependency_Expr;
       Kind_T              : Constants.Lex_Type;
       With_Str, Use_Str   : SU.Unbounded_String := SU.Null_Unbounded_String)
   return Dependency_Expr;

   function Make
      (C                   : Container_Expr;
       Kind_T              : Constants.Lex_Type;
       Name                : SU.Unbounded_String;
       Has_Body            : Boolean;
       Declarations        : Expr_List.Vector;
       Body_Expr           : Expr_List.Vector)
   return Container_Expr;

   function Make (F              : File_Expr;
                  Kind_T         : Constants.Lex_Type)
   return File_Expr;

   function Make (F              : File_Expr;
                  F_Name         : SU.Unbounded_String;
                  Dependencies   : Expr_List.Vector;
                  Container      : Container_Expr'Class)
   return File_Expr;

private

   type Dependency_Expr is new Expression with record

      With_Str : SU.Unbounded_String;
      Use_Str  : SU.Unbounded_String;

   end record;

   type Container_Expr is new Expression with record

      Name           : SU.Unbounded_String;
      Has_Body       : Boolean;
      Declarations   : Expr_List.Vector;
      Body_Expr      : Expr_List.Vector;

   end record;

   type File_Expr is new Expression with record

      File_Name      : SU.Unbounded_String;
      Dependencies   : Expr_List.Vector;
      Container      : Container_Expr;

   end record;

end Expressions;