with Ada.Characters.Latin_1;  use Ada.Characters.Latin_1;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Containers;          use Ada.Containers;

with Processing_Utils;

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

   function Tag (Str : String; Non_Textual_Keywords : Constants.Keyword.Map)
   return Constants.Lex_Type
   is
      Prepared_Str : constant String (Str'First .. Str'Last + 2) := Str & "_t";
      First_Char   : constant Character := Str (Str'First);
      Is_Edge_Case : Boolean := False;

      Kind : Constants.Lex_Type;

   begin

      Kind := Processing_Utils.Lex_Edge_Case (Str, Is_Edge_Case);
      if Is_Edge_Case then
         return Kind;
      end if;

      if not Is_Letter (First_Char) then
         return Non_Textual_Keywords (Prepared_Str);
      end if;

      return Constants.Lex_Type'Value (Prepared_Str);

   exception

      when others => return Constants.identifier_t;

   end Tag;

   -----------
   -- Clear --
   -----------

   procedure Clear (Buffer : in out Char_Buffer)
   is
   begin

      Char_Buffer_V.Clear (Buffer.Char_Vector);
      Char_Buffer_V.Set_Length (Buffer.Char_Vector, 0);

   end Clear;

   ------------
   -- Freeze --
   ------------

   procedure Freeze (Buffer : in out Char_Buffer;
                     Using : Constants.Keyword.Map)
   is
      Str_Rep : constant String := Buffer_To_String (Buffer);

   begin

      if Str_Rep = "" or else Str_Rep = " " then

         Buffer.Str := SU.To_Unbounded_String ("");
         Buffer.Kind := Constants.nil_t;
         Buffer.Clear;

      else
         Buffer.Str := SU.To_Unbounded_String (Str_Rep);
         Buffer.Kind := Tag (Str_Rep, Non_Textual_Keywords => Using);
         Buffer.Clear;

      end if;

   end Freeze;

   ----------
   -- Kind --
   ----------

   function Kind (Buffer : Char_Buffer) return Constants.Lex_Type
   is
   begin

      if SU."=" (Buffer.Str, SU.Null_Unbounded_String) then
         return Constants.nil_t;
      end if;

      return Buffer.Kind;

   end Kind;

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

   ---------
   -- Get --
   ---------

   function Get (Buffer : File_Buffer'Class; I : Positive) return Char_Buffer
   is
   begin
      return Buffer.Char_Buffer_Vector (I);
   end Get;

   procedure Accept_v (F : File_Buffer; V : in out Visitor_Int'Class)
   is
   begin
      Visit_File (V);
   end Accept_v;

   procedure Accept_v
      (F : File_Buffer; V : in out Visitor_Int'Class; File : in out File_Type)
   is
   begin
      Visit_File (V, File);
   end Accept_v;

   ---------------
   -- Get_Files --
   ---------------

   function Get_Files (Buffer : Code_Buffer) return Code_Buffer_Freezed
   is
   begin

      if Buffer.File_Buffer_Vector.Length = 0 then
         raise Constraint_Error;
      end if;

      declare
         N : constant Positive := Positive (Buffer.File_Buffer_Vector.Length);
         Code_Freezed : Code_Buffer_Freezed (1 .. N);

         First : constant Positive := Buffer.File_Buffer_Vector.First_Index;
         Last  : constant Positive := Buffer.File_Buffer_Vector.Last_Index;
      begin

         for I in First .. Last loop

            Code_Freezed (I) := Buffer.File_Buffer_Vector (I);
         end loop;

         return Code_Freezed;

      end;

   end Get_Files;

   ----------
   -- Make --
   ----------

   function Make (File : File_Buffer'Class; Pos : Positive) return AST_Backbone
   is
   begin

      return (File_Buffer (File), Pos);

   end Make;

   -------------
   -- Current --
   -------------

   function Current (Backbone : AST_Backbone'Class) return Char_Buffer
   is
   begin

      return Backbone.File.Get (Backbone.Pos);

   end Current;

   ----------
   -- Next --
   ----------

   function Next (Backbone : in out AST_Backbone'Class) return Char_Buffer
   is
   begin

      Backbone.Pos := Backbone.Pos + 1;
      return Backbone.File.Get (Backbone.Pos);

   end Next;

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