package body Expressions is

   function Kind (Self : Expression) return Expr_Type
   is
   begin

      return Self.Kind_T;
   end Kind;

   procedure Visit_File (Self : in out Visitor; Obj : File_Expr'Class)
   is
   begin

      Self.Visit_Expression (Obj);

   end Visit_File;

   overriding procedure Visit_File
      (Self : in out Expr_Visitor; F : File_Expr'Class)
   is
   begin
         Put_Line ("In " & Ada.Strings.Unbounded.To_String (F.Name));

   end Visit_File;

   procedure Visit (Self : in out Expression; F : File_Type;
                    V : in out Visitor'Class)
   is
   begin

      V.Visit_Expression (Self);

   end Visit;

   overriding
   procedure Visit (Self : in out File_Expr; F : File_Type;
                    V : in out Visitor'Class)
   is
   begin

      null;

   end Visit;

end Expressions;