with Ada.Strings.Unbounded;

with Constants; use Constants;

package Expressions is

   package SU renames Ada.Strings.Unbounded;

   type Expression is tagged private;

   type Visitor is tagged null record;

   procedure Parse (V : in out Visitor; Exp : Expression'Class);
   procedure Print (V : in out Visitor; Exp : Expression'Class);

   procedure Parse (Exp : in out Expression'Class; V : in out Visitor);
   procedure Print (Exp : in out Expression'Class; V : in out Visitor);

   type File_Expr is new Expression with private;

   function Make (Kind_T : Constants.Lex_Type; Name : SU.Unbounded_String)
   return File_Expr;

private

   type Expression is tagged record
      Kind_T : Constants.Lex_Type;
   end record;

   type File_Expr is new Expression with record

      Name : SU.Unbounded_String;

   end record;

end Expressions;