with Ada.Text_IO; use Ada.Text_IO;

package Visitor_Interface is

   type Visitor_Int is abstract tagged private;

   procedure Visit_File (V : in out Visitor_Int)
   is abstract;

   procedure Visit_File (V : in out Visitor_Int; File : in out File_Type)
   is abstract;

private

   type Visitor_Int is abstract tagged null record;

end Visitor_Interface;