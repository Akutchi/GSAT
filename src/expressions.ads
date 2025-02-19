with Ada.Text_IO; use Ada.Text_IO;

with Ada.Strings.Unbounded;

package Expressions is

   type Expr_Type is (P_SPEC, P_BODY, MAIN, IF_TYPE, FUNC_TYPE);

   type Expression is tagged private;

   function Kind (Self : Expression) return Expr_Type;

   overriding
   function "=" (Left, Right : Expression) return Boolean is
   (Left.Kind = Right.Kind);

   type Visitor is abstract tagged null record;

   procedure Visit (Self : in out Expression; F : File_Type;
                    V : in out Visitor'Class);

   type Dependency is new Expression with private;

   type File_Expr is new Expression with private;

   overriding
   procedure Visit (Self : in out File_Expr; F : File_Type;
                    V : in out Visitor'Class);

   procedure Visit_Expression (Self : in out Visitor; Obj : Expression'Class)
   is null;

   procedure Visit_File (Self : in out Visitor; Obj : File_Expr'Class);

   type Expr_Visitor is new Visitor with null record;

   overriding
   procedure Visit_File (Self : in out Expr_Visitor; F : File_Expr'Class);

private

   type Expression is tagged record
      Kind_T : Expr_Type;
   end record;

   type Dependency is new Expression with record

      Lib_Name : Ada.Strings.Unbounded.Unbounded_String;
      Has_Use : Boolean;

   end record;

   type File_Expr is new Expression with record

      Name : Ada.Strings.Unbounded.Unbounded_String;

   end record;

end Expressions;