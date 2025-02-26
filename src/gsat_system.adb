with Ada.Text_IO;                use Ada.Text_IO;
with Ada.Text_IO.Text_Streams;   use Ada.Text_IO.Text_Streams;
with Ada.Characters.Latin_1;     use Ada.Characters.Latin_1;
with Ada.Strings.Fixed;          use Ada.Strings.Fixed;

with Ada.Direct_IO;

with GNAT.SHA1;

with Exceptions.Arguments; use Exceptions.Arguments;

with Lexer;

package body Gsat_System is

   --------------------
   -- Show_Grey_Text --
   --------------------

   procedure Show_Grey_Text (Str : String)
   is
   begin
      Put_Line (ESC & "[90m" & Str & ESC & "[0m");
   end Show_Grey_Text;

   -------------------
   -- Show_Duration --
   -------------------

   procedure Show_Duration (Prog_Duration : Duration)
   is
      Duration_Str   : constant String := Duration'Image (Prog_Duration);
      First          : constant Natural := Duration_Str'First;
      Point_Pos      : constant Natural := Index (Duration_Str,
                                                  ".",
                                                  Duration_Str'First);
      Two_Commas     : constant Natural := Point_Pos + 2;

   begin

      Put_Line ("Transpiling finished successfully in" & ESC & "[1m" &
                Duration_Str (First .. Two_Commas) & " " & ESC & "[0m" &
                "seconds.");

   end Show_Duration;

   ---------------
   -- Parse_Src --
   ---------------

   function Parse_Src (Arg : String) return String
   is
      Command_Name  : constant String := Arg (Arg'First .. Arg'First + 1);
      Src_Candidate : constant String := Arg (Arg'First + 3 .. Arg'Last);

   begin

      if Arg (Arg'First + 2) /= '=' then
         raise Argument_Option_Error with
         "No option was specified for the source folder";
      end if;

      if Command_Name /= "-s" then
         raise Argument_Option_Error with
         Command_Name & " is not reckognized as an option.";
      end if;

      if not Exists (Src_Candidate) then
         raise Argument_Name_Error with
         Src_Candidate & " is not a valid source folder.";

      end if;

      return Src_Candidate;

   end Parse_Src;

   ---------------------------------
   -- Create_Generation_Directory --
   ---------------------------------

   procedure Create_Generation_Directory
   is
   begin

      Ada.Directories.Create_Path (Constants.Generation_Folder);
      Ada.Directories.Create_Path (Constants.Generation_Src_Folder);

   end Create_Generation_Directory;

   ------------------------
   -- Generate_Signature --
   ------------------------

   function Generate_Signature (Dir : Ada.Directories.Directory_Entry_Type)
   return Signature
   is

      Absolute_Name  : constant String := Ada.Directories.Full_Name (Dir);
      File_Size      : constant Natural := Natural (Ada.Directories.Size
                                                      (Absolute_Name));

      subtype File_String is String (1 .. File_Size);
      package FS_IO is new Ada.Direct_IO (File_String);

      Base_File   : FS_IO.File_Type;
      Content     : File_String;

      Ctx : GNAT.SHA1.Context;

   begin

      FS_IO.Open (Base_File, FS_IO.In_File, Absolute_Name);
      FS_IO.Read (Base_File, Content);
      FS_IO.Close (Base_File);
      GNAT.SHA1.Update (Ctx, Content);

      return Signature'(GNAT.SHA1.Digest (Ctx));

   end Generate_Signature;

   ---------------------------------------
   -- Get_Extension_From_Signature_File --
   ---------------------------------------

   function Get_Extension_From_Signature_File (Sgt_Str : String) return String
   is
      Str_Last : constant Integer := Integer (Sgt_Str'Last);
      Sgt_Last : constant Integer := Integer (Signature'Last);

   begin

      if Str_Last - Sgt_Last < 0 then
         return "";
      end if;

      return Sgt_Str (Str_Last - Sgt_Last - 3 .. Str_Last - Sgt_Last - 1);

   end Get_Extension_From_Signature_File;

   ----------------------------
   -- Prepare_Signature_File --
   ----------------------------

   procedure Prepare_Signature_File
      (Dir : Ada.Directories.Directory_Entry_Type)
   is

      Sgt_File : Ada.Text_IO.File_Type;

      Name_Ext     : constant String := Ada.Directories.Simple_Name (Dir);
      Just_Name    : constant String := Ada.Directories.Base_Name (Name_Ext);
      Sgt_File_Str : constant String := Constants.Generation_Folder &
                                        Just_Name & ".sgt";

      Another_Sgt  : constant Signature := Generate_Signature (Dir);

   begin

      if Ada.Directories.Exists (Sgt_File_Str) then

         Open (Sgt_File, In_File, Sgt_File_Str);

         declare
            Existing_Sgt : constant String := Get_Line (Sgt_File);
            File_Ext     : constant String :=
            Get_Extension_From_Signature_File (Existing_Sgt);

         begin

            Close (Sgt_File);
            Open (Sgt_File, Out_File, Sgt_File_Str);

            if File_Ext = "ads" then
               Put_Line (Sgt_File, Existing_Sgt);
               Put_Line (Sgt_File, Name_Ext & " " & Another_Sgt);

            elsif File_Ext = "adb" then
               Put_Line (Sgt_File, Name_Ext & " " & Another_Sgt);
               Put_Line (Sgt_File, Existing_Sgt);
            end if;

         end;

      else

         Create (Sgt_File, Out_File, Sgt_File_Str);
         Put_Line (Sgt_File, Name_Ext & " " & Another_Sgt);

      end if;

      Close (Sgt_File);

   end Prepare_Signature_File;

   -------------------------
   -- String_To_Signature --
   -------------------------

   function String_To_Signature (Str : String) return Signature
   is
      Sgt   : Signature;
      Start : constant Integer := Str'Last - Signature'Last;

   begin

      if Start < 0 then
         return Empty_Signature;
      end if;

      for I in Signature'Range loop
         Sgt (I) := Str (Start + I);
      end loop;

      return Sgt;

   end String_To_Signature;

   -------------------------
   -- Get_Number_Of_Lines --
   -------------------------

   function Get_Number_Of_Lines (F_Str : String) return Natural
   is
      Lines : Natural := 0;
      F     : File_Type;
   begin

      Open (F, In_File, F_Str);
      while not End_Of_File (F) loop

         Lines := Lines + 1;
         Skip_Line (F);
      end loop;
      Close (F);

      return Lines;

   exception

      when Ada.Text_IO.Name_Error => return 0;

   end Get_Number_Of_Lines;

   -------------------
   -- Get_Signature --
   -------------------

   function Get_Signature (Dir : Ada.Directories.Directory_Entry_Type)
   return Signature
   is
      F : Ada.Text_IO.File_Type;

      Name_Ext     : constant String := Ada.Directories.Simple_Name (Dir);
      Just_Name    : constant String := Ada.Directories.Base_Name (Name_Ext);
      Sgt_File_Str : constant String := Constants.Generation_Folder &
                                        Just_Name & ".sgt";

      Lines : Natural;

   begin

      Lines := Get_Number_Of_Lines (Sgt_File_Str);
      Open (F, In_File, Sgt_File_Str);

      loop

         declare
            Sgt_Str : constant String := Get_Line (F);
            File_Ext : constant String :=
               Get_Extension_From_Signature_File (Sgt_Str);
         begin

            if Extension (Name_Ext) = File_Ext then

               Close (F);
               return String_To_Signature (Sgt_Str);

            end if;
         end;

         Lines := Lines - 1;
         exit when Lines = 0;

      end loop;

      Close (F);

      return Empty_Signature;

   exception

      when Ada.Text_IO.Name_Error | Ada.Text_IO.End_Error =>
            return Empty_Signature;

   end Get_Signature;

   ---------------------------
   -- Has_Signature_Changed --
   ---------------------------

   function Has_Signature_Changed
      (Dir         : Ada.Directories.Directory_Entry_Type;
       Current_Sgt : Signature;
       New_Sgt     : in out Signature)
   return Status
   is
   begin

      return CHANGED;

      if Current_Sgt = Empty_Signature then
         return NOT_EXIST;
      end if;

      New_Sgt := Generate_Signature (Dir);

      if Current_Sgt = New_Sgt then
         return SAME;
      end if;

      return CHANGED;

   end Has_Signature_Changed;

   ----------------------
   -- Update_Signature --
   ----------------------

   procedure Update_Signature (Dir     : Ada.Directories.Directory_Entry_Type;
                               New_Sgt : Signature)
   is
      F : Ada.Text_IO.File_Type;

      Name_Ext     : constant String := Ada.Directories.Simple_Name (Dir);
      Just_Name    : constant String := Ada.Directories.Base_Name (Name_Ext);
      Sgt_File_Str : constant String := Constants.Generation_Folder &
                                        Just_Name & ".sgt";

      Lines : Natural;

   begin

      Lines := Get_Number_Of_Lines (Sgt_File_Str);

      if Lines = 1 then
         Open (F, Out_File, Sgt_File_Str);
         Put (F, Name_Ext & " " & String (String_To_Signature (New_Sgt)));

      else

         Open (F, In_File, Sgt_File_Str);
         declare
            Ads_Sgt_Str : constant String := Get_Line (F);
            Adb_Sgt_Str : constant String := Get_Line (F);

         begin

            Close (F);
            Open (F, Out_File, Sgt_File_Str);

            if Extension (Name_Ext) = "ads" then

               Put_Line (F,
                        Name_Ext &
                        " " &
                        String (String_To_Signature (New_Sgt)));

               Put (F, Adb_Sgt_Str);

            elsif Extension (Name_Ext) = "adb" then

               Put_Line (F, Ads_Sgt_Str);
               Put (F,
                  Name_Ext &
                  " " &
                  String (String_To_Signature (New_Sgt)));

            end if;
         end;
      end if;

      Close (F);

   end Update_Signature;

   ---------
   -- Lex --
   ---------

   procedure Lex
      (Dir                    : Ada.Directories.Directory_Entry_Type;
       Code                   : in out T_Buffer.Code_Buffer;
       Non_Textual_Keywords   : Constants.Keyword.Map)
   is
      F           : File_Type;
      Input       : Stream_Access;
      File_Tokens : T_Buffer.File_Buffer;

      File_Str : constant String := Ada.Directories.Full_Name (Dir);

   begin

      Show_Grey_Text ("  [SugarAda]    " & Ada.Directories.Simple_Name (Dir));

      Open (F, In_File, File_Str);
      Input := Stream (F);

      File_Tokens := Lexer.Lexing (F, Input, Non_Textual_Keywords);
      T_Buffer.Append (Code, File_Tokens);

      Close (F);

   end Lex;

   --------------------------
   -- Is_Not_Relative_Path --
   --------------------------

   function Is_Not_Relative_Path (Dir : Ada.Directories.Directory_Entry_Type)
   return Boolean
   is
      File_Str : constant String := Ada.Directories.Full_Name (Dir);
   begin

      return File_Str (File_Str'Last - 1 .. File_Str'Last) /= ".."
             and then File_Str (File_Str'Last) /= '.';

   end Is_Not_Relative_Path;

   ---------------
   -- Lex_Level --
   ---------------

   procedure Lex_Level
      (Dir_Str                : String;
       Code                   : in out T_Buffer.Code_Buffer;
       Non_Textual_Keywords   : Constants.Keyword.Map)
   is
      Dir : Ada.Directories.Directory_Entry_Type;
      Dir_Search : Ada.Directories.Search_Type;

   begin

      Ada.Directories.Start_Search
                              (Search    => Dir_Search,
                               Directory => Dir_Str,
                               Pattern   => "*");
      loop

         Ada.Directories.Get_Next_Entry (Dir_Search, Dir);

         declare

            File_Str : constant String := Ada.Directories.Full_Name (Dir);

            Sgt, New_Sgt   : Signature := Empty_Signature;
            Sgt_Status     : Status;

         begin

            if Ada.Directories.Kind (Dir) = Ada.Directories.Ordinary_File then

               Sgt         := Get_Signature (Dir);
               Sgt_Status  := Has_Signature_Changed (Dir, Sgt, New_Sgt);

               case Sgt_Status is

                  when SAME      => null;
                  when CHANGED   => Update_Signature (Dir, New_Sgt);
                  when NOT_EXIST => Prepare_Signature_File (Dir);
               end case;

               if Sgt_Status /= SAME then
                  Lex (Dir, Code, Non_Textual_Keywords);
               end if;

            elsif Is_Not_Relative_Path (Dir) then

               Lex_Level (File_Str, Code, Non_Textual_Keywords);

            end if;
         end;

         exit when not Ada.Directories.More_Entries (Dir_Search);

      end loop;

   end Lex_Level;

end Gsat_System;