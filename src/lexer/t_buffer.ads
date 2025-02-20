with Ada.Containers.Indefinite_Vectors;
with Ada.Strings.Unbounded;

with Constants;

package T_Buffer is

   package SU renames Ada.Strings.Unbounded;

   type Char_Buffer is tagged private;

   procedure Append (Buffer : in out Char_Buffer; Char : Character);
   procedure Append (Buffer : in out Char_Buffer; Str : String);

   function Last (Buffer : Char_Buffer) return Character;

   procedure Freeze (Buffer : in out Char_Buffer);
   --  Freeze the Char_Vector by transforming it in a string.
   --  Tag the token with the correct lexing category.
   --  When freezed, the buffer is not writable anymore

   function Buffer_To_String (Buffer : Char_Buffer) return String;

   procedure Print (Buffer : Char_Buffer);

   type File_Buffer is tagged private;

   procedure Append
      (Buffer : in out File_Buffer'Class; C_Buffer : Char_Buffer);

   procedure Print (Buffer : File_Buffer);

   type Code_Buffer is tagged private;

   procedure Append
      (Buffer : in out Code_Buffer'Class; F_Buffer : File_Buffer);

   procedure Print (Buffer : Code_Buffer);

private

   package Char_Buffer_V is new Ada.Containers.Indefinite_Vectors
      (Index_Type => Natural,
      Element_Type => Character,
      "=" => "="
   );

   type Char_Buffer is tagged record

      Kind     : Constants.Lex_Type;
      Str      : SU.Unbounded_String := SU.Null_Unbounded_String;
      --  Optimization, avoid nested Loops at t_buffer.adb:162 and is
      --  easier to get when parsing.

      Char_Vector : Char_Buffer_V.Vector;

   end record;

   function Tag (Str : String) return Constants.Lex_Type;

   procedure Clear (Buffer : in out Char_Buffer);

   --  First element is always the file name, and "=" use that.
   package File_Buffer_V is new Ada.Containers.Indefinite_Vectors
     (Index_Type => Positive,
      Element_Type => Char_Buffer,
      "=" => "=");

   type File_Buffer is tagged record

      Char_Buffer_Vector : File_Buffer_V.Vector;

   end record;

   function File_Buffer_Equal (Left, Right : File_Buffer) return Boolean is
   (Left.Char_Buffer_Vector (Left.Char_Buffer_Vector.First_Index) =
   Right.Char_Buffer_Vector (Right.Char_Buffer_Vector.First_Index));

   package Code_Buffer_V is new Ada.Containers.Indefinite_Vectors
     (Index_Type => Positive,
      Element_Type => File_Buffer,
      "=" => File_Buffer_Equal);

   type Code_Buffer is tagged record

      File_Buffer_Vector : Code_Buffer_V.Vector;

   end record;

end T_Buffer;