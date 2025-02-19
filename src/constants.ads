with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Constants is

   KEYWORDS : constant array (1 .. 4) of Unbounded_String := (
      To_Unbounded_String ("loop"),
      To_Unbounded_String ("then"),
      To_Unbounded_String ("is"),
      To_Unbounded_String ("begin")
   );

end Constants;