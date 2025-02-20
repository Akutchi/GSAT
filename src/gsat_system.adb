with Ada.Text_IO;                use Ada.Text_IO;
with Ada.Text_IO.Text_Streams;   use Ada.Text_IO.Text_Streams;
with Ada.Directories;            use Ada.Directories;

with Lexer;

package body Gsat_System is

   procedure Go_Trough_Level (Dir_Str : String;
                              Code : in out T_Buffer.Source_Code.Vector)
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

            F : File_Type;
            Input : Stream_Access;
            File_Tokens : T_Buffer.File_Buffer.Vector;

         begin

            if Ada.Directories.Kind (Dir) = Ada.Directories.Ordinary_File then

               Open (File => F, Mode => In_File, Name => File_Str);
               Input := Stream (F);
               File_Tokens := Lexer.Lexing (F, Input);
               T_Buffer.Source_Code.Append (Code, File_Tokens);

            elsif File_Str (File_Str'Last - 1 .. File_Str'Last) /= ".."
            and then File_Str (File_Str'Last) /= '.'
            then

               Go_Trough_Level (File_Str, Code);

            end if;

         end;

         exit when not Ada.Directories.More_Entries (Dir_Search);

      end loop;

   end Go_Trough_Level;

end Gsat_System;