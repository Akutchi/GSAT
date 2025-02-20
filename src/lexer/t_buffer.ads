with Ada.Containers.Indefinite_Vectors;

package T_Buffer is

   type Char_Buffer is tagged private;

   procedure Append (Buffer : in out Char_Buffer; Char : Character);

   function Last (Buffer : Char_Buffer) return Character;

   procedure Clear (Buffer : in out Char_Buffer);

   function Buffer_To_String (Buffer : Char_Buffer) return String;

   procedure Print (Buffer : Char_Buffer);

   --  First element is always the file name, and "=" use that.
   package File_Buffer is new Ada.Containers.Indefinite_Vectors
     (Index_Type => Positive,
      Element_Type => String,
      "=" => "=");

   function "=" (Left, Right : File_Buffer.Vector) return Boolean is
   (Left (Left.First_Index) = Right (Right.First_Index));

   procedure Append (Buffer : in out File_Buffer.Vector; Str : String);

   procedure Print (Buffer : File_Buffer.Vector);

   package Source_Code is new Ada.Containers.Indefinite_Vectors
     (Index_Type => Positive,
      Element_Type => File_Buffer.Vector,
      "=" => T_Buffer."=");

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