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
      Name           : SU.Unbounded_String;
      Has_Body       : Boolean := False;
      Declarations   : constant Expr_List.Vector := Expr_List.Empty_Vector;
      Body_Expr      : constant Expr_List.Vector := Expr_List.Empty_Vector;

      Current_Token : T_Buffer.Char_Buffer;

   begin

      Current_Token := Backbone.Next;

      if Current_Token.Kind = Constants.identifier_t then

         Name := Expressions_Utils.Get_Dotted_Name (Backbone);

      elsif Current_Token.Kind = Constants.body_t then

         Has_Body := True;
         Name := Expressions_Utils.Get_Dotted_Name (Backbone);
      end if;

      Expr := Expr.Make (Constants.package_t,
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
                    F      : in out File_Type)
   is
   begin

      Put (F, "package ");

      if Expr.Has_Body then
         Put (F, "body ");
      end if;

      Put_Line (F, SU.To_String (Expr.Name) & " is");

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
                       F      : in out File_Type)
   is
   begin
      V.Visit_Expr (Expr, F);
   end Accept_v;

end Expressions.Container;