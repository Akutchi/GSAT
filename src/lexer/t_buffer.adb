with Ada.Text_IO;             use Ada.Text_IO;
with Ada.Characters.Latin_1;  use Ada.Characters.Latin_1;

package body T_Buffer is

   ------------
   -- Append --
   ------------

   procedure Append (Buffer : in out Char_Buffer; Char : Character)
   is

      Is_Tab : constant Boolean :=
         (Buffer.Char_Vector.Is_Empty and then Char = ' ');
   begin

      if Is_Tab or else Char = LF then
         return;
      end if;

      Char_Buffer_V.Append (Buffer.Char_Vector, Char);

   end Append;

   procedure Append (Buffer : in out Char_Buffer; Str : String)
   is
   begin

      for c of Str loop
         Char_Buffer_V.Append (Buffer.Char_Vector, c);
      end loop;

   end Append;

   procedure Append (Buffer : in out File_Buffer'Class; C_Buffer : Char_Buffer)
   is
      Str : constant String := Buffer_To_String (C_Buffer);
   begin

      if Str /= "" and then Str /= " " then

         File_Buffer_V.Append (Buffer.Char_Buffer_Vector, C_Buffer);

      end if;

   end Append;

   procedure Append (Buffer : in out Code_Buffer'Class; F_Buffer : File_Buffer)
   is
   begin

      Code_Buffer_V.Append (Buffer.File_Buffer_Vector, F_Buffer);

   end Append;

   ----------
   -- Last --
   ----------

   function Last (Buffer : Char_Buffer) return Character
   is
   begin

      if Buffer.Char_Vector.Is_Empty then
         return ' ';
      end if;

      return Buffer.Char_Vector.Last_Element;
   end Last;

   ---------
   -- Tag --
   ---------

   function Tag (Str : String) return Constants.Lex_Type
   is
      Prepared_Str : constant String (Str'First .. Str'Last + 2) := Str & "_t";
   begin

      if Str = ";_t" then
         return Constants.semi_colon_t;
      elsif Str = ",_t" then
         return Constants.colon_t;
      elsif Str = "._t" then
         return Constants.dot_t;
      end if;

      return Constants.Lex_Type'Value (Prepared_Str);

   exception

      when others => return Constants.nil_t;

   end Tag;

   ------------
   -- Freeze --
   ------------

   procedure Freeze (Buffer : in out Char_Buffer)
   is
      Str_Rep : constant String := Buffer_To_String (Buffer);

   begin

      Buffer.Str := SU.To_Unbounded_String (Str_Rep);
      Buffer.Kind := Tag (Str_Rep);
      Buffer.Clear;

   end Freeze;

   -----------
   -- Clear --
   -----------

   procedure Clear (Buffer : in out Char_Buffer)
   is
   begin

      Char_Buffer_V.Clear (Buffer.Char_Vector);
      Char_Buffer_V.Set_Length (Buffer.Char_Vector, 0);

   end Clear;

   ----------------------
   -- Buffer_To_String --
   ----------------------

   function Buffer_To_String (Buffer : Char_Buffer) return String
   is

      Char_Vector : constant Char_Buffer_V.Vector := Buffer.Char_Vector;

      Tmp : SU.Unbounded_String;

   begin

      if not SU."=" (Buffer.Str, SU.Null_Unbounded_String) then
         return SU.To_String (Buffer.Str);
      end if;

      for I in Char_Vector.First_Index .. Char_Vector.Last_Index loop
         SU.Append (Tmp, Char_Vector (I));
      end loop;

      return SU.To_String (Tmp);

   end Buffer_To_String;

   -----------
   -- Print --
   -----------

   procedure Print (Buffer : Char_Buffer)
   is
   begin

      if SU."=" (Buffer.Str, SU.Null_Unbounded_String) then
         Put_Line ("[Warning : Buffer was not freezed]");
      else
         Put_Line
            (Buffer_To_String (Buffer) &
            " [" &
            Constants.Lex_Type'Image (Buffer.Kind) &
            "]");
      end if;

   end Print;

   procedure Print (Buffer : File_Buffer)
   is

      C_Buffers : constant File_Buffer_V.Vector := Buffer.Char_Buffer_Vector;
      Buffer_Last : constant Natural := Natural (C_Buffers.Last_Index);

      First : constant Natural := Natural (C_Buffers.First_Index);
      Last  : constant Natural :=
         (if Buffer_Last > 20 then 20 else Buffer_Last);

   begin

      if Buffer_Last = 0 then

         Put_Line ("File empty");
         return;
      end if;

      Print (C_Buffers (First));

      --  Remember, the first element of a File_Buffer is always the file name
      for I in First + 1 .. Last loop
         Print (C_Buffers (I));
      end loop;

      if Buffer_Last > 20 then
         Put_Line ("...");
      end if;

      Put_Line ("");

   end Print;

   procedure Print (Buffer : Code_Buffer)
   is
   begin

      for F of Buffer.File_Buffer_Vector loop
         Print (F);
      end loop;

   end Print;

end T_Buffer;