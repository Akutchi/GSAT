with Ada.Text_IO; use Ada.Text_IO;

package body Expressions is

   procedure Parse (Exp : in out Expression'Class; V : in out Visitor) is
   begin
      V.Parse (Exp);
   end Parse;

   procedure Print (Exp : in out Expression'Class; V : in out Visitor) is
   begin
      V.Print (Exp);
   end Print;

   procedure Parse (V : in out Visitor; Exp : Expression'Class)
   is
   begin

      if Exp.Kind_T = Constants.file_t then

         Put_Line ("In file");

      end if;
   end Parse;

   procedure Print (V : in out Visitor; Exp : Expression'Class)
   is
   begin

      if Exp.Kind_T = Constants.file_t then

         Put_Line ("In file");

      end if;
   end Print;

   function Make (Kind_T : Constants.Lex_Type; Name : SU.Unbounded_String)
   return File_Expr
   is
      F : File_Expr;
   begin

      F.Kind_T := Kind_T;
      F.Name := Name;

      return F;

   end Make;

end Expressions;