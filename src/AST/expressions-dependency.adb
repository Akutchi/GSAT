package body Expressions.Dependency is

   procedure Parse (D          : in out Dependency_Expr;
                    Backbone   : in out T_Buffer.AST_Backbone'Class)
   is
   begin
      D.Parse_Dependency (Backbone);
   end Parse;

   procedure Print (D : Dependency_Expr; F : in out File_Type)
   is
   begin
      D.Print_With (F);
   end Print;

   ----------
   -- Make --
   ----------

   function Make
      (D                   : Dependency_Expr;
       Kind_T              : Constants.Lex_Type;
       With_Str, Use_Str   : SU.Unbounded_String := SU.Null_Unbounded_String)
   return Dependency_Expr
   is
   begin

      return (Kind_T, With_Str, Use_Str);

   end Make;

   -------------------------
   -- Get_Until_Semicolon --
   -------------------------

   procedure Get_Until_Semicolon
      (Str      : in out SU.Unbounded_String;
       Backbone : in out T_Buffer.AST_Backbone'Class)
   is
      Current_Token : T_Buffer.Char_Buffer;

   begin

      Current_Token := Backbone.Current;

      while Current_Token.Kind /= Constants.semi_colon_t loop

         SU.Append (Str, Backbone.Current.Buffer_To_String);
         Current_Token := Backbone.Next;

      end loop;

   end Get_Until_Semicolon;

   ----------------------
   -- Parse_Dependency --
   ----------------------

   procedure Parse_Dependency (D       : in out Dependency_Expr;
                              Backbone : in out T_Buffer.AST_Backbone'Class)
   is
      Current_Token : T_Buffer.Char_Buffer;
      With_Str, Use_Str : SU.Unbounded_String := SU.Null_Unbounded_String;

   begin

      Current_Token := Backbone.Next;
      Get_Until_Semicolon (With_Str, Backbone);

      Current_Token := Backbone.Next;
      if Current_Token.Kind = Constants.use_t then

         Current_Token := Backbone.Next;
         Get_Until_Semicolon (Use_Str, Backbone);

      end if;

      D := D.Make (Constants.with_t,  With_Str, Use_Str);

   end Parse_Dependency;

   ----------------
   -- Print_With --
   ----------------

   procedure Print_With (Exp : Dependency_Expr; F : in out File_Type)
   is
   begin

      if not SU."=" (Exp.With_Str, SU.Null_Unbounded_String) then
         Put (F, "with " & SU.To_String (Exp.With_Str) & "; ");
      end if;

      if not SU."=" (Exp.Use_Str, SU.Null_Unbounded_String) then
         Put_Line (F, "use " & SU.To_String (Exp.Use_Str) & "; ");
      end if;

      Put_Line (F, " ");

   end Print_With;

end Expressions.Dependency;