with Ada.Characters.Latin_1;  use Ada.Characters.Latin_1;
with Ada.Characters.Handling; use Ada.Characters.Handling;

package body Processing_Utils is

   ------------
   -- Toogle --
   ------------

   procedure Toogle (In_Text : in out Boolean;
                     Char, Buffer_Last : Character)
   is
      Is_Quote     : constant Boolean := Char = ''' or else Char = '"';
      Is_Qualified : constant Boolean :=
         Char = ''' and then Is_Letter (Buffer_Last);

   begin

      if Is_Quote and then not Is_Qualified then
         In_Text := not In_Text;
      end if;

   end Toogle;

   ---------
   -- EOL --
   ---------

   function EOL (Char : Character; In_Text : Boolean) return Boolean
   is

      Is_Keyword_Sep : constant Boolean := Char = ' ' or else Char = LF;
      Is_Parenthesis : constant Boolean := Char = '(' or else Char = ')';
      Is_Colon       : constant Boolean := Char = ',' or else Char = ';';
      Is_Dot         : constant Boolean := Char = '.' and then not In_Text;

   begin

      return Is_Keyword_Sep
      or else Is_Parenthesis
      or else Is_Colon
      or else Is_Dot;

   end EOL;

end Processing_Utils;