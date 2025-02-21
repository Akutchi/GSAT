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

   -------------
   -- Is_Text --
   -------------

   function Is_Text (Str : String) return Boolean
   is
      Is_Double_Quoted : constant Boolean :=
         Str (Str'First) = '"' and then Str (Str'Last) = '"';
      Is_Single_Quoted : constant Boolean :=
         Str (Str'First) = ''' and then Str (Str'Last) = ''';
   begin

      return Is_Double_Quoted or else Is_Single_Quoted;
   end Is_Text;

   -------------------
   -- Lex_Edge_Case --
   -------------------

   function Lex_Edge_Case (Str : String; Is_Edge_Case : in out Boolean)
   return Constants.Lex_Type
   is
      First_Char : constant Character := Str (Str'First);

   begin

      if Str'Length >= 2 and then Str (Str'First .. Str'First + 1) = "--" then
            Is_Edge_Case := True;
            return Constants.comment_t;

      elsif Is_Text (Str) then
         Is_Edge_Case := True;
         return Constants.text_t;

      --  Using Open files names are absolute and thus always start with a /
      elsif First_Char = '/' then
         Is_Edge_Case := True;
         return Constants.file_t;

      elsif Str = "mod" then
         Is_Edge_Case := True;
         return Constants.operator_t;

      end if;

      return Constants.nil_t;

   end Lex_Edge_Case;

end Processing_Utils;