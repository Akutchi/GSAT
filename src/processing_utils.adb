package body Processing_Utils is

   function EOL (Char : Character) return Boolean
   is

      Is_Keyword_Sep : constant Boolean := Char = ' ' or else Char = LF;
      Is_Parenthesis : constant Boolean := Char = '(' or else Char = ')';

   begin

      return Is_Keyword_Sep or else Is_Parenthesis;

   end EOL;

end Processing_Utils;