with Constants; use Constants;

package body Expressions_Utils is

   function Get_Dotted_Name (Backbone : in out T_Buffer.AST_Backbone'Class)
   return SU.Unbounded_String
   is
      Name : SU.Unbounded_String := SU.Null_Unbounded_String;

      Current_Token : T_Buffer.Char_Buffer := Backbone.Current;
   begin

      while Current_Token.Kind = Constants.identifier_t or else
            Current_Token.Kind = Constants.dot_t
      loop

         Name := Name & Current_Token.Buffer_To_String;
         Current_Token := Backbone.Next;

      end loop;

      return Name;

   end Get_Dotted_Name;

end Expressions_Utils;