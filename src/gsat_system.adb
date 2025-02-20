with Ada.Text_IO;       use Ada.Text_IO;
with Ada.Directories;   use Ada.Directories;

package body Gsat_System is

   procedure Go_Trough_Level (Dir_Str : String)
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

         begin

            if Ada.Directories.Kind (Dir) = Ada.Directories.Ordinary_File then

               Put_Line (File_Str);

            elsif File_Str (File_Str'Last - 1 .. File_Str'Last) /= ".."
            and then File_Str (File_Str'Last) /= '.'
            then

               Go_Trough_Level (File_Str);

            end if;

         end;

         exit when not Ada.Directories.More_Entries (Dir_Search);

      end loop;

   end Go_Trough_Level;

end Gsat_System;