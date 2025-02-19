with Ada.Text_IO;    use Ada.Text_IO;
with Ada.Containers; use Ada.Containers;

with Ada.Strings.Unbounded;

with Constants;

package body T_Buffer is

   procedure Append (Buffer : in out Char_Buffer; Char : Character)
   is
   begin

      if Buffer.Buffer_V.Is_Empty and then Char = ' ' then
         return;
      end if;

      Char_Buffer_V.Append (Buffer.Buffer_V, Char);

   end Append;

   procedure Clear (Buffer : in out Char_Buffer)
   is
   begin

      Char_Buffer_V.Clear (Buffer.Buffer_V);
      Char_Buffer_V.Set_Length (Buffer.Buffer_V, 0);

   end Clear;

   function To_String
      (Buffer_V : Char_Buffer_V.Vector; First, Last : Natural)
   return String
   is

      Tmp : Ada.Strings.Unbounded.Unbounded_String :=
         Ada.Strings.Unbounded.Null_Unbounded_String;

   begin

      for I in First .. Last loop
         Ada.Strings.Unbounded.Append (Tmp, Buffer_V (I));
      end loop;

      return Ada.Strings.Unbounded.To_String (Tmp);

   end To_String;

   function Has_Keyword (Buffer : Char_Buffer) return Boolean
   is

      function Has (Keyword : String; Buffer : Char_Buffer) return Boolean
      is
         Last_Idx : constant Natural := Natural (Buffer.Buffer_V.Length);

      begin

         --  don't need to check Last_Idx > 0 because no keyword has 0 length
         if Last_Idx >= Keyword'Length then

            declare
               Cut : constant Natural := Last_Idx - Keyword'Length;

            begin
               return  To_String (Buffer.Buffer_V, Cut, Last_Idx-1) = Keyword;

            end;
         end if;

         return False;

      end Has;

   begin

      return Has (Constants.K_BEGIN, Buffer);

   end Has_Keyword;

   procedure Print (Buffer : Char_Buffer)
   is
   begin

      Put_Line (To_String (
                           Buffer.Buffer_V,
                           Buffer.Buffer_V.First_Index,
                           Buffer.Buffer_V.Last_Index
                           ));

   end Print;

end T_Buffer;