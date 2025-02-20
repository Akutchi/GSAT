package Processing_Utils is

   procedure Toogle (In_Text : in out Boolean;
                     Char, Buffer_Last : Character);

   function EOL (Char : Character; In_Text : Boolean)
   return Boolean;

end Processing_Utils;