with Ada.Text_IO; use Ada.Text_IO;

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

   procedure Parse (Exp       : in out Expression'Class;
                    Backbone  : in out T_Buffer.AST_Backbone);

   procedure Print (Exp : Expression'Class; F : in out File_Type);

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

   procedure Get_Until_Semicolon (Str      : in out SU.Unbounded_String;
                                  Backbone : in out T_Buffer.AST_Backbone);

   procedure Parse_Dependency (Exp        : in out Expression'Class;
                               Backbone   : in out T_Buffer.AST_Backbone);

   procedure Parse_Package (Exp       : in out Expression'Class;
                            Backbone  : in out T_Buffer.AST_Backbone);

   procedure Parse_File (Exp        : in out Expression'Class;
                         Backbone   : in out T_Buffer.AST_Backbone);

   procedure Print_With (Exp : Dependency_Expr; F : in out File_Type);

   procedure Print_Package (Exp : Container_Expr; F : in out File_Type);

   procedure Print_File (Exp : File_Expr; F : in out File_Type);

end Expressions;