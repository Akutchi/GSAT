with T_Buffer;
with Constants;

package Gsat_System is

   procedure Lex_Level (Dir_Str : String;
                        Code : in out T_Buffer.Code_Buffer;
                        Non_Textual_Keywords : Constants.Keyword.Map);

end Gsat_System;