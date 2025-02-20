with Ada.Containers.Indefinite_Vectors;

package T_Buffer is

   type Char_Buffer is tagged private;

   package String_Buffer is new Ada.Containers.Indefinite_Vectors
     (Index_Type => Positive,
      Element_Type => String,
      "=" => "=");

   procedure Append (Buffer : in out Char_Buffer; Char : Character);

   procedure Append (Buffer : in out String_Buffer.Vector; Str : String);

   function Last (Buffer : Char_Buffer) return Character;

   procedure Clear (Buffer : in out Char_Buffer);

   function Buffer_To_String (Buffer : Char_Buffer) return String;

   procedure Print (Buffer : Char_Buffer);

   procedure Print (Buffer : String_Buffer.Vector);

private

   package Char_Buffer_V is new Ada.Containers.Indefinite_Vectors
      (Index_Type => Natural,
      Element_Type => Character,
      "=" => "="
   );

   type Char_Buffer is tagged record

      Buffer_V : Char_Buffer_V.Vector;

   end record;

end T_Buffer;