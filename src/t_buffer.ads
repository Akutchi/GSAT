with Ada.Containers.Indefinite_Vectors;

package T_Buffer is

   type Char_Buffer is tagged private;

   procedure Append (Buffer : in out Char_Buffer; Char : Character);

   function Last (Buffer : Char_Buffer) return Character;

   procedure Clear (Buffer : in out Char_Buffer);

   function Has_Keyword (Buffer : Char_Buffer) return Boolean;

   procedure Print (Buffer : Char_Buffer);

private

   package Char_Buffer_V is new Ada.Containers.Indefinite_Vectors
      (Index_Type => Natural,
      Element_Type => Character,
      "=" => "="
   );

   type Char_Buffer is tagged record

      Buffer_V : Char_Buffer_V.Vector;

   end record;

   function Buffer_To_String
      (Buffer_V : Char_Buffer_V.Vector; First, Last : Natural)
   return String
   with Pre => (Last <= Natural (Buffer_V.Length));

end T_Buffer;