with Ada.Characters.Handling; use Ada.Characters.Handling;

with Expressions.Declaration; use Expressions.Declaration;

with Expressions_Utils;

package body Expressions.Container is

   ----------
   -- Make --
   ----------

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

   -----------
   -- Parse --
   -----------

   overriding
   procedure Parse (Expr      : in out Container_Expr;
                    V         : Visitor_Int'Class;
                    Backbone  : in out T_Buffer.AST_Backbone'Class)
   is
      Kind           : constant Constants.Lex_Type := Backbone.Current.Kind;
      Name           : SU.Unbounded_String;
      Has_Body       : Boolean := False;
      Declarations   : Expr_List.Vector := Expr_List.Empty_Vector;
      Body_Expr      : Expr_List.Vector := Expr_List.Empty_Vector;

      Current_Token : T_Buffer.Char_Buffer;

   begin

      Current_Token := Backbone.Next;

      if Current_Token.Kind = Constants.identifier_t then

         Name := Expressions_Utils.Get_Dotted_Name (Backbone);

      elsif Current_Token.Kind = Constants.body_t then

         Has_Body := True;
         Current_Token := Backbone.Next;
         Name := Expressions_Utils.Get_Dotted_Name (Backbone);
      end if;

      if Kind = Constants.procedure_t then

         Current_Token := Backbone.Next;
         while Current_Token.Kind /= Constants.begin_t loop

            declare
               D : Declaration_Expr;
            begin
               D.Accept_v (V, Backbone);
               Declarations.Append (D);
               Current_Token := Backbone.Next;
            end;

         end loop;
      end if;

      Expr := Expr.Make (Kind,
                         Name,
                         Has_Body,
                         Declarations,
                         Body_Expr);

   end Parse;

   -----------
   -- Print --
   -----------

   overriding
   procedure Print (Expr   : Container_Expr;
                    V      : Visitor_Int'Class;
                    Tabs   : Natural;
                    F      : in out File_Type)
   is

      Kind_Str : constant String := Constants.Lex_Type'Image (Expr.Kind_T);
   begin

      Put (F, To_Lower (Kind_Str (Kind_Str'First .. Kind_Str'Last - 2)) & " ");
      --  delete the " _t " part of the type

      if Expr.Has_Body then
         Put (F, "body ");
      end if;

      Put_Line (F, SU.To_String (Expr.Name) & " is");
      Put_Line (F, " ");

      for Declaration of Expr.Declarations loop
         Declaration.Accept_v (V, Tabs + 3, F);
      end loop;

      if Expr.Kind_T = Constants.procedure_t then
         Put_Line (F, " ");
         Put_Line (F, "begin");
         Put_Line (F, " ");
      end if;

      for Sub_Expr of Expr.Body_Expr loop
         Sub_Expr.Accept_v (V, Tabs + 3, F);
      end loop;

      Put_Line (F, "end " & SU.To_String (Expr.Name) & ";");

   end Print;

   --------------
   -- Accept_v --
   --------------

   overriding
   procedure Accept_v (Expr      : in out Container_Expr;
                       V         : Visitor_Int'Class;
                       Backbone  : in out T_Buffer.AST_Backbone'Class)
   is
   begin
      V.Visit_Expr (Expr, Backbone);
   end Accept_v;

   overriding
   procedure Accept_v (Expr   : Container_Expr;
                       V      : Visitor_Int'Class;
                       Tabs   : Natural;
                       F      : in out File_Type)
   is
   begin
      V.Visit_Expr (Expr, Tabs, F);
   end Accept_v;

end Expressions.Container;