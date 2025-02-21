with Constants;
package Processing_Utils is

   procedure Toogle (In_Text : in out Boolean;
                     Char, Buffer_Last : Character);

   function EOL (Char : Character; In_Text : Boolean)
   return Boolean;

   function Is_Text (Str : String) return Boolean;

   function Lex_Edge_Case (Str : String; Is_Edge_Case : in out Boolean)
   return Constants.Lex_Type;

end Processing_Utils;