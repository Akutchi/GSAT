package body Expressions.Dependency is

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

   -----------
   -- Parse --
   -----------

   overriding
   procedure Parse (Expr   : in out Dependency_Expr;
                 V         : Visitor_Int'Class;
                 Backbone  : in out T_Buffer.AST_Backbone'Class)
   is
      Current_Token : T_Buffer.Char_Buffer;
      With_Str, Use_Str : SU.Unbounded_String := SU.Null_Unbounded_String;

   begin

      Current_Token := Backbone.Next;
      Get_Until_Semicolon (With_Str, Backbone);

      Current_Token := Backbone.Look_Ahead;
      if Current_Token.Kind = Constants.use_t then

         Current_Token := Backbone.Next;
         Current_Token := Backbone.Next;
         Get_Until_Semicolon (Use_Str, Backbone);

      end if;

      Expr := Expr.Make (Constants.with_t,  With_Str, Use_Str);

   end Parse;

   -----------
   -- Print --
   -----------

   overriding
   procedure Print (Expr : Dependency_Expr;
                    V    : Visitor_Int'Class;
                    F    : in out File_Type)
   is
   begin

      if not SU."=" (Expr.With_Str, SU.Null_Unbounded_String) then
         Put (F, "with " & SU.To_String (Expr.With_Str) & "; ");
      end if;

      if not SU."=" (Expr.Use_Str, SU.Null_Unbounded_String) then
         Put (F, "use " & SU.To_String (Expr.Use_Str) & "; ");
      end if;

      Put_Line (F, " ");

   end Print;

   --------------
   -- Accept_v --
   --------------

   overriding
   procedure Accept_v (Expr      : in out Dependency_Expr;
                       V         : Visitor_Int'Class;
                       Backbone  : in out T_Buffer.AST_Backbone'Class)
   is
   begin
      V.Visit_Expr (Expr, Backbone);
   end Accept_v;

   overriding
   procedure Accept_v (Expr   : Dependency_Expr;
                       V      : Visitor_Int'Class;
                       F      : in out File_Type)
   is
   begin
      V.Visit_Expr (Expr, F);
   end Accept_v;

end Expressions.Dependency;