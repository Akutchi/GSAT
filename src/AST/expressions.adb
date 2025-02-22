with Ada.Text_IO; use Ada.Text_IO;

package body Expressions is

   -----------
   -- Parse --
   -----------

   procedure Parse_Dependency (Exp : in out Expression'Class;
                              Backbone : in out T_Buffer.AST_Backbone)
   is
      Current_Token : T_Buffer.Char_Buffer;

      With_Str, Use_Str : SU.Unbounded_String := SU.Null_Unbounded_String;

   begin

      Current_Token := Backbone.Next;

      while Current_Token.Kind /= Constants.semi_colon_t loop

         SU.Append
            (With_Str,
             Backbone.Current.Buffer_To_String);

         Current_Token := Backbone.Next;

      end loop;

      Current_Token := Backbone.Next;

      if Current_Token.Kind = Constants.use_t then

         while Current_Token.Kind /= Constants.semi_colon_t loop

            SU.Append
               (Use_Str,
                Backbone.Current.Buffer_To_String);

            Current_Token := Backbone.Next;

         end loop;

      end if;

      Exp := Expression'Class (Dependency_Expr (Exp).Make (Constants.with_t,
                                                           With_Str,
                                                           Use_Str));

   end Parse_Dependency;

   procedure Parse_File (V : in out Visitor;
                         Exp : in out Expression'Class;
                         Backbone : in out T_Buffer.AST_Backbone)
   is
      F_Name         : SU.Unbounded_String;
      Dependencies   : Expr_List.Vector := Expr_List.Empty_Vector;

      Current_Token : T_Buffer.Char_Buffer;

   begin

      F_Name := SU.To_Unbounded_String (T_Buffer.Buffer_To_String
                                                         (Backbone.Current));

      Current_Token := Backbone.Next;
      while Current_Token.Kind = Constants.with_t loop

         declare
            D : Dependency_Expr;
         begin

            D.Parse (V, Backbone);
            Dependencies.Append (D);
            Current_Token := Backbone.Current;

         end;
      end loop;

      Exp := Expression'Class
         (File_Expr (Exp).Make (Dependencies, F_Name));

   end Parse_File;

   procedure Parse (V : in out Visitor; Exp : in out Expression'Class;
                    Backbone : in out T_Buffer.AST_Backbone)
   is
      Current_Token : constant T_Buffer.Char_Buffer := Backbone.Current;

   begin

      case Current_Token.Kind is

         when Constants.file_t => Parse_File (V, Exp, Backbone);
         when Constants.with_t => Parse_Dependency (Exp, Backbone);

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

   procedure Print_With (Exp : Dependency_Expr)
   is
   begin

      if not SU."=" (Exp.With_Str, SU.Null_Unbounded_String) then
         Put ("with " & SU.To_String (Exp.With_Str) & "; ");
      end if;

      if not SU."=" (Exp.Use_Str, SU.Null_Unbounded_String) then
         Put_Line ("use " & SU.To_String (Exp.Use_Str) & "; ");
      end if;

      Put_Line ("");

   end Print_With;

   procedure Print_File (Exp : File_Expr; V : in out Visitor)
   is
   begin
      Put_Line (SU.To_String (Exp.Name));

      for Dependency of Exp.Dependencies loop
         Dependency.Print (V);
      end loop;

      Put_Line ("");

   end Print_File;

   procedure Print (V : in out Visitor; Exp : Expression'Class)
   is
   begin

      if Exp.Kind_T = Constants.file_t then

         Print_File (File_Expr (Exp), V);

      end if;

      if Exp.Kind_T = Constants.with_t then

         Print_With (Dependency_Expr (Exp));

      end if;

   end Print;

   procedure Print (Exp : Expression'Class; V : in out Visitor)
   is
   begin
      V.Print (Exp);
   end Print;

   ----------
   -- Make --
   ----------

   function Make (F              : File_Expr;
                  Kind_T         : Constants.Lex_Type)
   return File_Expr
   is
      F_New : File_Expr;
   begin

      F_New.Kind_T := Kind_T;

      return F_New;

   end Make;

   function Make (F              : File_Expr;
                  Dependencies   : Expr_List.Vector;
                  Name           : SU.Unbounded_String)
   return File_Expr
   is
      F_New : File_Expr;
   begin

      F_New.Kind_T := F.Kind_T;
      F_New.Dependencies := Dependencies;
      F_New.Name   := Name;

      return F_New;

   end Make;

   function Make
      (D                   : Dependency_Expr;
       Kind_T              : Constants.Lex_Type;
       With_Str, Use_Str   : SU.Unbounded_String := SU.Null_Unbounded_String)
   return Dependency_Expr
   is
   begin

      return (Kind_T, With_Str, Use_Str);

   end Make;

end Expressions;