with Ada.Containers.Indefinite_Vectors;
with Ada.Strings.Unbounded;

with Constants;

package T_Buffer is

   package SU renames Ada.Strings.Unbounded;

   type Char_Buffer is tagged private;

   procedure Append (Buffer : in out Char_Buffer; Char : Character);
   procedure Append (Buffer : in out Char_Buffer; Str : String);

   function Last (Buffer : Char_Buffer) return Character;

   procedure Freeze (Buffer : in out Char_Buffer;
                     Using : Constants.Keyword.Map);
   --  Freeze the Char_Vector by transforming it in a string.
   --  Tag the token with the correct lexing category.
   --  When freezed, the buffer is not writable anymore.
   --  The keyword_map is here to map non textual keyword such as ;

   function Kind (Buffer : Char_Buffer) return Constants.Lex_Type;

   function Buffer_To_String (Buffer : Char_Buffer) return String;

   procedure Print (Buffer : Char_Buffer);

   type File_Buffer is tagged private;

   procedure Append
      (Buffer : in out File_Buffer'Class; C_Buffer : Char_Buffer);

   function Get (Buffer : File_Buffer'Class; I : Positive) return Char_Buffer;

   procedure Print (Buffer : File_Buffer);

   type Code_Buffer is tagged private;

   --  This allows me to get a Code_Buffer without needing to expose
   --  The vector package.
   type Code_Buffer_Freezed is array (Positive range <>) of File_Buffer;

   procedure Append
      (Buffer : in out Code_Buffer'Class; F_Buffer : File_Buffer);

   function Get_Files (Buffer : Code_Buffer) return Code_Buffer_Freezed;

   procedure Print (Buffer : Code_Buffer);

   type AST_Backbone is tagged private;

   function Make (File : File_Buffer'Class; Pos : Positive)
   return AST_Backbone;

   function Current (Backbone : AST_Backbone'Class) return Char_Buffer;

   function Next (Backbone : in out AST_Backbone'Class) return Char_Buffer;

   function Look_Ahead (Backbone : AST_Backbone'Class) return Char_Buffer;

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

   function Tag (Str : String; Non_Textual_Keywords : Constants.Keyword.Map)
   return Constants.Lex_Type;

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

   type AST_Backbone is tagged record

      File : File_Buffer;
      Pos  : Positive;

   end record;

end T_Buffer;