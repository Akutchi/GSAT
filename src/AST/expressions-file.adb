with Expressions.Dependency; use Expressions.Dependency;

package body Expressions.File is

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
                  F_Name         : SU.Unbounded_String;
                  Dependencies   : Expr_List.Vector;
                  Container      : Container_Expr'Class)
   return File_Expr
   is
      F_New : File_Expr;
   begin

      F_New.Kind_T := F.Kind_T;
      F_New.File_Name   := F_Name;
      F_New.Dependencies := Dependencies;
      F_New.Container := Container_Expr (Container);

      return F_New;

   end Make;

   -----------
   -- Parse --
   -----------

   overriding
   procedure Parse (Expr      : in out File_Expr;
                 V         : Visitor_Int'Class;
                 Backbone  : in out T_Buffer.AST_Backbone'Class)
   is
      F_Name         : SU.Unbounded_String;
      Dependencies   : Expr_List.Vector := Expr_List.Empty_Vector;
      Container      : Container_Expr;

      Current_Token : T_Buffer.Char_Buffer;

   begin

      F_Name := SU.To_Unbounded_String (T_Buffer.Buffer_To_String
                                                         (Backbone.Current));

      Current_Token := Backbone.Next;
      while Current_Token.Kind = Constants.with_t loop

         declare
            D : Dependency_Expr;
         begin

            D.Accept_v (V, Backbone);
            Dependencies.Append (D);
            Current_Token := Backbone.Current;

         end;
      end loop;

      Container.Parse (V, Backbone);

      Expr := Expr.Make (F_Name, Dependencies, Container);

   end Parse;

   -----------
   -- Print --
   -----------

   overriding
   procedure Print (Expr   : File_Expr;
                    V      : Visitor_Int'Class;
                    F      : in out File_Type)
   is
      F_Name         : SU.Unbounded_String;
      Absolute_Name  : constant String := SU.To_String (Expr.File_Name);

   begin

      for I in reverse Absolute_Name'Range loop

         exit when Absolute_Name (I) = '/';
         F_Name := SU."&" (Absolute_Name (I), F_Name);

      end loop;

      Create (F,
              Out_File,
              Constants.Generation_Src_Folder & SU.To_String (F_Name));

      for Dependency of Expr.Dependencies loop
         Dependency.Accept_v (V, F);
      end loop;

      Put_Line (F, " ");
      Expr.Container.Print (V, F);
      Put_Line (F, " ");

      Close (F);

   end Print;

   --------------
   -- Accept_v --
   --------------

   overriding
   procedure Accept_v (Expr      : in out File_Expr;
                       V         : Visitor_Int'Class;
                       Backbone  : in out T_Buffer.AST_Backbone'Class)
   is
   begin
      V.Visit_Expr (Expr, Backbone);
   end Accept_v;

   overriding
   procedure Accept_v (Expr   : File_Expr;
                       V      : Visitor_Int'Class;
                       F      : in out File_Type)
   is
   begin
      V.Visit_Expr (Expr, F);
   end Accept_v;

end Expressions.File;