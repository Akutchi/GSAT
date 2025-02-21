with Ada.Text_IO; use Ada.Text_IO;

package body Expressions is

   -----------
   -- Parse --
   -----------

   procedure Parse_File (Exp : in out Expression'Class;
                         Backbone : T_Buffer.AST_Backbone)
   is
      F_Name : constant SU.Unbounded_String :=
                  SU.To_Unbounded_String
                     (T_Buffer.Buffer_To_String (Backbone.File.Get (1)));
   begin

      Exp := File_Expr'Class (Make (Constants.file_t, F_Name));

   end Parse_File;

   procedure Parse (V : in out Visitor; Exp : in out Expression'Class;
                    Backbone : in out T_Buffer.AST_Backbone)
   is
      Current_Token : constant T_Buffer.Char_Buffer :=
         Backbone.File.Get (Backbone.Pos);
   begin

      case Current_Token.Kind is

         when Constants.file_t =>
            Parse_File (Exp, Backbone);
            Backbone.Pos := Backbone.Pos + 1;

         when others => null;

      end case;
   end Parse;

   procedure Parse (Exp : in out Expression'Class; V : in out Visitor;
                    Backbone : in out T_Buffer.AST_Backbone)
   is
   begin
      V.Parse (Exp, Backbone);
   end Parse;

   -----------
   -- Print --
   -----------

   procedure Print_File (Exp : File_Expr)
   is
   begin
      Put_Line (SU.To_String (Exp.Name));
   end Print_File;

   procedure Print (V : in out Visitor; Exp : Expression'Class)
   is
   begin

      if Exp.Kind_T = Constants.file_t then

         Print_File (File_Expr (Exp));

      end if;

      if Exp.Kind_T = Constants.with_t then

         Put_Line ("with");

      end if;

   end Print;

   procedure Print (Exp : in out Expression'Class; V : in out Visitor)
   is
   begin
      V.Print (Exp);
   end Print;

   ----------
   -- Make --
   ----------

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