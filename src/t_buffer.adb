with Ada.Text_IO;             use Ada.Text_IO;
with Ada.Containers;          use Ada.Containers;
with Ada.Characters.Latin_1;  use Ada.Characters.Latin_1;

with Ada.Strings.Unbounded;

with Constants;

package body T_Buffer is

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

   function Buffer_To_String
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

   end Buffer_To_String;

   -----------------
   -- Has_Keyword --
   -----------------

   function Has_Keyword (Buffer : Char_Buffer) return Boolean
   is

      function Has (Keyword : String; Buffer : Char_Buffer) return Boolean
      is
         Last_Idx : constant Natural := Natural (Buffer.Buffer_V.Length);

      begin

         if Last_Idx >= Keyword'Length and then Last_Idx > 1 then

            declare
               Cut : constant Natural := Last_Idx - Keyword'Length;

            begin

               if Cut > 0 then

                  declare

                     Before_Split : constant String := Buffer_To_String
                        (Buffer.Buffer_V, Cut - 1, Cut);

                     Split : constant String := Buffer_To_String
                        (Buffer.Buffer_V, Cut, Last_Idx - 1);

                  begin

                     return Split = Keyword and then Before_Split = " ";

                  end;
               end if;
            end;
         end if;

         return False;

      end Has;

   begin

      for Unbound_Key of Constants.KEYWORDS loop

         declare
            Key : constant String :=
               Ada.Strings.Unbounded.To_String (Unbound_Key);

         begin

            if Has (Key, Buffer) then
               return True;
            end if;

         end;
      end loop;

      return False;

   end Has_Keyword;

   -----------
   -- Print --
   -----------

   procedure Print (Buffer : Char_Buffer)
   is
   begin

      if Buffer.Buffer_V.Length /= 0 then

         Put_Line (Buffer_To_String (
                                       Buffer.Buffer_V,
                                       Buffer.Buffer_V.First_Index,
                                       Buffer.Buffer_V.Last_Index
                                    ));
      end if;

   end Print;

end T_Buffer;