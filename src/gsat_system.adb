with Ada.Text_IO;                use Ada.Text_IO;
with Ada.Text_IO.Text_Streams;   use Ada.Text_IO.Text_Streams;

with Ada.Direct_IO;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;

with GNAT.SHA1;

with Lexer;

package body Gsat_System is

   package S_F renames Ada.Strings.Fixed;
   package S_M renames Ada.Strings.Maps;

   ---------------------------------
   -- Create_Generation_Directory --
   ---------------------------------

   procedure Create_Generation_Directory
   is
   begin

      Ada.Directories.Create_Path ("./obj/gen");

   end Create_Generation_Directory;

   ----------------------
   -- Create_Signature --
   ----------------------

   procedure Create_Signature (Dir : Ada.Directories.Directory_Entry_Type)
   is
      Sgt_File    : Ada.Text_IO.File_Type;

      Absolute_Name  : constant String := Ada.Directories.Full_Name (Dir);
      Name_Ext       : constant String := Ada.Directories.Simple_Name (Dir);
      Name           : constant String := Ada.Directories.Base_Name (Name_Ext);

      File_Size      : constant Natural := Natural (Ada.Directories.Size
                                                      (Absolute_Name));

      Sgt_File_Str : constant String := "./obj/gen/" & Name & ".sgt";

      subtype File_String is String (1 .. File_Size);
      package FS_IO is new Ada.Direct_IO (File_String);

      Base_File   : FS_IO.File_Type;
      Content     : File_String;

      Ctx : GNAT.SHA1.Context;

   begin

      FS_IO.Open (Base_File, FS_IO.In_File, Absolute_Name);
      FS_IO.Read (Base_File, Content);
      GNAT.SHA1.Update (Ctx, Content);

      if Ada.Directories.Extension (Name_Ext) = "ads" then

         if Ada.Directories.Exists (Sgt_File_Str) then

            Ada.Text_IO.Open (Sgt_File, Ada.Text_IO.In_File, Sgt_File_Str);
            Skip_Line (Sgt_File);
            declare
               Line_adb : String := Get_Line (Sgt_File);
            begin
               Close (Sgt_File);
               Ada.Text_IO.Open (Sgt_File, Ada.Text_IO.Out_File, Sgt_File_Str);
               Put_Line (Sgt_File, Name_Ext & " " & GNAT.SHA1.Digest (Ctx));
               Put (Sgt_File, Line_adb);
            end;

         else

            Ada.Text_IO.Create
               (Sgt_File, Ada.Text_IO.Append_File, Sgt_File_Str);
            Put (Sgt_File, Name_Ext & " " & GNAT.SHA1.Digest (Ctx));

         end if;

      elsif Ada.Directories.Extension (Name_Ext) = "adb" then

         if Ada.Directories.Exists (Sgt_File_Str) then

            Ada.Text_IO.Open (Sgt_File, Ada.Text_IO.Append_File, Sgt_File_Str);
            Put (Sgt_File, Name_Ext & " " & GNAT.SHA1.Digest (Ctx));

         else

            Ada.Text_IO.Create (Sgt_File, Ada.Text_IO.Out_File, Sgt_File_Str);
            New_Line (Sgt_File, 1);
            Put (Sgt_File, Name_Ext & " " & GNAT.SHA1.Digest (Ctx));

         end if;
      end if;

      FS_IO.Close (Base_File);
      Ada.Text_IO.Close (Sgt_File);

   end Create_Signature;

   -------------------
   -- Get_Signature --
   -------------------

   function Get_Signature (Dir : Ada.Directories.Directory_Entry_Type)
   return Signature
   is
   begin

      return Empty_Signature;

   end Get_Signature;

   ---------------------------
   -- Has_Signature_Changed --
   ---------------------------

   function Has_Signature_Changed (Dir : Ada.Directories.Directory_Entry_Type;
                                   Current_Sgt : Signature)
   return Status
   is
   begin

      if Current_Sgt = Empty_Signature then
         return NOT_EXIST;
      end if;

      return CHANGED;

   end Has_Signature_Changed;

   ----------------------
   -- Update_Signature --
   ----------------------

   procedure Update_Signature (Dir : Ada.Directories.Directory_Entry_Type;
                               Current_Sgt : Signature)
   is
   begin
      null;
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

      Put_Line (Standard_Output, "  [SugarAda]    " &
                                 Ada.Directories.Simple_Name (Dir));

      Open (File => F, Mode => In_File, Name => File_Str);
      Input := Stream (F);

      File_Tokens := Lexer.Lexing (F, Input, Non_Textual_Keywords);
      T_Buffer.Append (Code, File_Tokens);

   end Lex;

   ---------------------
   -- Is_Not_Relative --
   ---------------------

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

            Sgt         : Signature;
            Sgt_Status  : Status;

         begin

            if Ada.Directories.Kind (Dir) = Ada.Directories.Ordinary_File then

               Sgt         := Get_Signature (Dir);
               Sgt_Status  := Has_Signature_Changed (Dir, Sgt);
               case Sgt_Status is

                  when SAME      => null;
                  when CHANGED   => Update_Signature (Dir, Sgt);
                  when NOT_EXIST => Create_Signature (Dir);

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