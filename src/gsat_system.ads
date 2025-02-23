with Ada.Directories; use Ada.Directories;

with Ada.Strings.Hash;

with T_Buffer;
with Constants;

package Gsat_System is

   procedure Create_Generation_Directory;

   procedure Lex_Level
      (Dir_Str                : String;
       Code                   : in out T_Buffer.Code_Buffer;
       Non_Textual_Keywords   : Constants.Keyword.Map);

private

   subtype Signature is String (1 .. 40);
   Empty_Signature : constant Signature := (others => ' ');

   type Status is (CHANGED, SAME, NOT_EXIST);

   procedure Create_Signature (Dir : Ada.Directories.Directory_Entry_Type);
   --  every XXX.ad[] are regrouped in the same XXX.sgt file.

   function Get_Signature (Dir : Ada.Directories.Directory_Entry_Type)
   return Signature;
   --  If XXX.sgt does not exist, an empty signature is returned.

   function Has_Signature_Changed (Dir : Ada.Directories.Directory_Entry_Type;
                                   Current_Sgt : Signature)
   return Status;

   procedure Update_Signature (Dir : Ada.Directories.Directory_Entry_Type;
                               Current_Sgt : Signature);

   procedure Lex
      (Dir                    : Ada.Directories.Directory_Entry_Type;
       Code                   : in out T_Buffer.Code_Buffer;
       Non_Textual_Keywords   : Constants.Keyword.Map);

   function Is_Not_Relative_Path (Dir : Ada.Directories.Directory_Entry_Type)
   return Boolean;

end Gsat_System;