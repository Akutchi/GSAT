with Ada.Directories; use Ada.Directories;

with T_Buffer;
with Constants;

package Gsat_System is

   procedure Show_Grey_Text (Str : String);

   procedure Show_Duration (Prog_Duration : Duration);

   function Parse_Src (Arg : String) return String;

   procedure Create_Generation_Directory;

   procedure Lex_Level
      (Dir_Str                : String;
       Code                   : in out T_Buffer.Code_Buffer;
       Non_Textual_Keywords   : Constants.Keyword.Map);

private

   subtype Signature is String (1 .. 40);
   Empty_Signature : constant Signature := (others => ' ');

   type Status is (CHANGED, SAME, NOT_EXIST);

   function Generate_Signature (Dir : Ada.Directories.Directory_Entry_Type)
   return Signature;

   function Get_Extension_From_Signature_File (Sgt_Str : String) return String;
   --  Return an empty string if the string is below a Signature length.

   procedure Prepare_Signature_File
      (Dir : Ada.Directories.Directory_Entry_Type);
   --  every XXX.ad[] are regrouped in the same XXX.sgt file.
   --  We want the file to be structured with the .ads signature
   --  on the first line, and .adb on the second.

   function String_To_Signature (Str : String) return Signature;
   --  If XXX.ad[] does not have a signature, return an empty signature.

   function Get_Number_Of_Lines (F_Str : String) return Natural;

   function Get_Signature (Dir : Ada.Directories.Directory_Entry_Type)
   return Signature;
   --  If XXX.sgt does not exist, an empty signature is returned.

   function Has_Signature_Changed
      (Dir         : Ada.Directories.Directory_Entry_Type;
       Current_Sgt : Signature;
       New_Sgt     : in out Signature)
   return Status;

   procedure Update_Signature (Dir     : Ada.Directories.Directory_Entry_Type;
                               New_Sgt : Signature);

   procedure Lex
      (Dir                    : Ada.Directories.Directory_Entry_Type;
       Code                   : in out T_Buffer.Code_Buffer;
       Non_Textual_Keywords   : Constants.Keyword.Map);

   function Is_Not_Relative_Path (Dir : Ada.Directories.Directory_Entry_Type)
   return Boolean;

end Gsat_System;