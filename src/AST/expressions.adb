with Ada.Text_IO; use Ada.Text_IO;

package body Expressions is

   -------------------------
   -- Get_Until_Semicolon --
   -------------------------

   procedure Get_Until_Semicolon (Str      : in out SU.Unbounded_String;
                                  Backbone : in out T_Buffer.AST_Backbone)
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

   procedure Parse_Dependency (Exp     : in out Expression'Class;
                              Backbone : in out T_Buffer.AST_Backbone)
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

      Exp := Expression'Class (Dependency_Expr (Exp).Make (Constants.with_t,
                                                           With_Str,
                                                           Use_Str));

   end Parse_Dependency;

   procedure Parse_Package (V          : in out Visitor;
                             Exp       : in out Expression'Class;
                             Backbone  : in out T_Buffer.AST_Backbone)
   is
      Name           : SU.Unbounded_String;
      Has_Body       : Boolean := False;
      Declarations   : constant Expr_List.Vector := Expr_List.Empty_Vector;
      Body_Expr      : constant Expr_List.Vector := Expr_List.Empty_Vector;

      Current_Token : T_Buffer.Char_Buffer;

   begin

      Current_Token := Backbone.Next;

      if Current_Token.Kind = Constants.identifier_t then

         Name := SU.To_Unbounded_String (Current_Token.Buffer_To_String);

      elsif Current_Token.Kind = Constants.body_t then

         Has_Body := True;
         Name := SU.To_Unbounded_String (Backbone.Next.Buffer_To_String);
      end if;

      Exp := Expression'Class (Container_Expr (Exp).Make (Constants.package_t,
                                                          Name,
                                                          Has_Body,
                                                          Declarations,
                                                          Body_Expr));

   end Parse_Package;

   ----------------
   -- Parse_File --
   ----------------

   procedure Parse_File (V          : in out Visitor;
                         Exp        : in out Expression'Class;
                         Backbone   : in out T_Buffer.AST_Backbone)
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

            D.Parse (V, Backbone);
            Dependencies.Append (D);
            Current_Token := Backbone.Current;

         end;
      end loop;

      Container.Parse (V, Backbone);

      Exp := Expression'Class (File_Expr (Exp).Make (F_Name,
                                                     Dependencies,
                                                     Container));

   end Parse_File;

   -----------
   -- Parse --
   -----------

   procedure Parse (V         : in out Visitor;
                    Exp       : in out Expression'Class;
                    Backbone  : in out T_Buffer.AST_Backbone)
   is
      Current_Token : constant T_Buffer.Char_Buffer := Backbone.Current;

   begin

      case Current_Token.Kind is

         when Constants.file_t    => Parse_File (V, Exp, Backbone);
         when Constants.with_t    => Parse_Dependency (Exp, Backbone);
         when Constants.package_t => Parse_Package (V, Exp, Backbone);

         when others => null;

      end case;
   end Parse;

   procedure Parse (Exp       : in out Expression'Class;
                    V         : in out Visitor;
                    Backbone  : in out T_Buffer.AST_Backbone)
   is
   begin
      V.Parse (Exp, Backbone);
   end Parse;

   ----------------
   -- Print_With --
   ----------------

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

   -------------------
   -- Print_Package --
   -------------------

   procedure Print_Package (Exp : Container_Expr)
   is
   begin

      Put ("package ");

      if Exp.Has_Body then
         Put ("body ");
      end if;

      Put_Line (SU.To_String (Exp.Name));
      Put_Line ("is");
      Put_Line ("begin");

      Put_Line ("end " & SU.To_String (Exp.Name) & ";");

   end Print_Package;

   ----------------
   -- Print_File --
   ----------------

   procedure Print_File (Exp : File_Expr; V : in out Visitor'Class)
   is
   begin
      Put_Line (SU.To_String (Exp.File_Name));

      for Dependency of Exp.Dependencies loop
         Dependency.Print (V);
      end loop;

      Put_Line (" ");
      Exp.Container.Print (V);
      Put_Line (" ");

   end Print_File;

   -----------
   -- Print --
   -----------

   procedure Print (V : in out Visitor; Exp : Expression'Class)
   is
   begin

      case Exp.Kind_T is

         when Constants.file_t      => Print_File (File_Expr (Exp), V);
         when Constants.with_t      => Print_With (Dependency_Expr (Exp));
         when Constants.package_t   => Print_Package (Container_Expr (Exp));

         when others => Put_Line ("Could not print " &
                                  Constants.Lex_Type'Image (Exp.Kind_T));

      end case;

   exception

      when Constraint_Error => Put_Line ("Rest of the file");

   end Print;

   procedure Print (Exp : Expression'Class; V : in out Visitor)
   is
   begin
      V.Print (Exp);
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

   function Make
      (C                   : Container_Expr;
       Kind_T              : Constants.Lex_Type;
       Name                : SU.Unbounded_String;
       Has_Body            : Boolean;
       Declarations        : Expr_List.Vector;
       Body_Expr           : Expr_List.Vector)
   return Container_Expr
   is
   begin

      return (Kind_T => Kind_T, Name => Name, Has_Body => Has_Body,
              Declarations => Declarations, Body_Expr => Body_Expr);

   end Make;

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

end Expressions;