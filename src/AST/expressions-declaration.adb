with Ada.Strings.Fixed; use Ada.Strings.Fixed;

with Ada.Strings.Unbounded;

with Expressions_Utils;

package body Expressions.Declaration is

   package SU renames Ada.Strings.Unbounded;

   ----------
   -- Make --
   ----------

   function Make
      (D          : Declaration_Expr;
       Kind       : Constants.Lex_Type;
       Name       : SU.Unbounded_String;
       Type_Expr  : SU.Unbounded_String;
       Value      : Expr_List.Vector)
   return Declaration_Expr
   is
   begin
      return (Kind, Name, Type_Expr, Value);
   end Make;

   -----------
   -- Parse --
   -----------

   overriding
   procedure Parse (Expr      : in out Declaration_Expr;
                    V         : Visitor_Int'Class;
                    Backbone  : in out T_Buffer.AST_Backbone'Class)
   is
      Current_Token : T_Buffer.Char_Buffer;

      Name, Type_Str : SU.Unbounded_String;
      Value : Expr_List.Vector := Expr_List.Empty_Vector;

      Is_Package_Renaming : Boolean := False;

   begin

      if Backbone.Current.Kind = Constants.comment_t then
         Current_Token := Backbone.Next;
         Name := Expressions_Utils.Get_Dotted_Name (Backbone,
                                                    Is_Comment => True);

         Expr := Expr.Make (Constants.comment_t, Name, Type_Str, Value);

      else

         if Backbone.Current.Kind = Constants.package_t then
            Current_Token := Backbone.Next;
            Is_Package_Renaming := True;
         end if;

         Name := SU.To_Unbounded_String (Backbone.Current.Buffer_To_String);
         Current_Token := Backbone.Next;
         Current_Token := Backbone.Next;
         Type_Str := Expressions_Utils.Get_Dotted_Name (Backbone);

         --  Value TBD
         if Is_Package_Renaming then
            Expr := Expr.Make (Constants.package_t, Name, Type_Str, Value);
         else
            Expr := Expr.Make
               (Constants.source_declaration_t, Name, Type_Str, Value);
         end if;
      end if;

   end Parse;

   -----------
   -- Print --
   -----------

   overriding
   procedure Print (Expr   : Declaration_Expr;
                    V      : Visitor_Int'Class;
                    Tabs   : Natural;
                    F      : in out File_Type)
   is
   begin

      if Expr.Kind_T = Constants.package_t then

         Put_Line (F, Tabs * " " & "package " & SU.To_String (Expr.Name) &
                      " renames " & SU.To_String (Expr.Type_Expr) & ";");

      elsif Expr.Kind_T = Constants.comment_t then

         Put_Line (F, Tabs * " " & "--  " & SU.To_String (Expr.Name));

      else

         Put (F, Tabs * " " & SU.To_String (Expr.Name) & " : " &
                 SU.To_String (Expr.Type_Expr));

         if Natural (Expr.Value.Length) > 0 then

            for Val of Expr.Value loop
               Val.Accept_v (V, Constants.NO_TAB, F);
            end loop;

         else
            Put_Line (F, ";");
         end if;
      end if;

   end Print;

   --------------
   -- Accept_v --
   --------------

   overriding
   procedure Accept_v (Expr      : in out Declaration_Expr;
                       V         : Visitor_Int'Class;
                       Backbone  : in out T_Buffer.AST_Backbone'Class)
   is
   begin
      V.Visit_Expr (Expr, Backbone);
   end Accept_v;

   overriding
   procedure Accept_v (Expr   : Declaration_Expr;
                       V      : Visitor_Int'Class;
                       Tabs   : Natural;
                       F      : in out File_Type)
   is
   begin
      V.Visit_Expr (Expr, Tabs, F);
   end Accept_v;

end Expressions.Declaration;