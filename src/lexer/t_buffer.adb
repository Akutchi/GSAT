with Ada.Text_IO;             use Ada.Text_IO;
with Ada.Containers;          use Ada.Containers;
with Ada.Characters.Latin_1;  use Ada.Characters.Latin_1;

with Ada.Strings.Unbounded;

package body T_Buffer is

   package SU renames Ada.Strings.Unbounded;

   ------------
   -- Append --
   ------------

   procedure Append (Buffer : in out Char_Buffer; Char : Character)
   is

      Is_Tab : constant Boolean :=
         (Buffer.Buffer_V.Is_Empty and then Char = ' ');
   begin

      if Is_Tab or else Char = LF then
         return;
      end if;

      Char_Buffer_V.Append (Buffer.Buffer_V, Char);

   end Append;

   procedure Append (Buffer : in out String_Buffer.Vector; Str : String)
   is
   begin

      if Str /= "" and then Str /= " " then
         String_Buffer.Append (Buffer, Str);
      end if;

   end Append;

   ----------
   -- Last --
   ----------

   function Last (Buffer : Char_Buffer) return Character
   is
   begin

      if Buffer.Buffer_V.Is_Empty then
         return ' ';
      end if;

      return Buffer.Buffer_V.Last_Element;
   end Last;

   -----------
   -- Clear --
   -----------

   procedure Clear (Buffer : in out Char_Buffer)
   is
   begin

      Char_Buffer_V.Clear (Buffer.Buffer_V);
      Char_Buffer_V.Set_Length (Buffer.Buffer_V, 0);

   end Clear;

   ----------------------
   -- Buffer_To_String --
   ----------------------

   function Buffer_To_String (Buffer : Char_Buffer) return String
   is

      Buffer_V : constant Char_Buffer_V.Vector := Buffer.Buffer_V;

      Tmp : SU.Unbounded_String :=
         SU.Null_Unbounded_String;

   begin

      for I in Buffer_V.First_Index .. Buffer_V.Last_Index loop
         SU.Append (Tmp, Buffer_V (I));
      end loop;

      return SU.To_String (Tmp);

   end Buffer_To_String;

   -----------
   -- Print --
   -----------

   procedure Print (Buffer : Char_Buffer)
   is
   begin

      if Buffer.Buffer_V.Length /= 0 then
         Put_Line (Buffer_To_String (Buffer));
      end if;

   end Print;

   procedure Print (Buffer : String_Buffer.Vector)
   is

      Buffer_Last : constant Positive := Positive (Buffer.Last_Index);
      First : constant Positive := Positive (Buffer.First_Index) + 1;
      Last  : constant Positive :=
         (if Buffer_Last > 20 then 20 else Buffer_Last);

   begin

      Put_Line (Buffer (Buffer.First_Index));

      for I in First .. Last loop
         Put_Line (Buffer (I));
      end loop;

      if Buffer_Last > 20 then
         Put_Line ("...");
      end if;

      Put_Line ("");

   end Print;

end T_Buffer;