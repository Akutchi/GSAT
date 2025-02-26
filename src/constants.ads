with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;

package Constants is

   Generation_Folder       : constant String := "./obj/gen/";
   Generation_Src_Folder   : constant String := "./obj/gen/src/";

   NO_TAB   : constant Natural := 0;
   TAB      : constant Natural := 3;

   type Lex_Type is (

      with_t,        --  package
      use_t,
      package_t,
      body_t,
      renames_t,

      procedure_t,   --  functions
      function_t,
      in_t,
      out_t,
      is_t,
      declare_t,
      begin_t,
      end_t,

      for_t,         --  control flow
      if_t,
      elsif_t,
      else_t,
      while_t,
      loop_t,
      then_t,

      task_t,        --  tasks
      entry_t,
      accept_t,
      select_t,
      terminate_t,

      operator_t,    --  operator
      assign_t,
      arrow_t,

      type_t,        --  types
      constant_t,
      new_t,
      access_t,
      all_t,
      string_t,
      integer_t,
      float_t,
      character_t,

      record_t,      --  record
      tagged_t,
      abstract_t,

      semi_colon_t,  --  punctuation
      colon_t,
      comma_t,
      left_parenthesis_t,
      right_parenthesis_t,
      dot_t,

      text_t,        --  text
      comment_t,

      source_file_t, --  misc
      identifier_t,
      source_declaration_t,
      nil_t
   );

   package Keyword is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => String,
      Element_Type    => Lex_Type,
      Hash            => Ada.Strings.Hash,
      Equivalent_Keys => "=",
      "="             => "=");

   procedure Init_Map (K_Map : in out Keyword.Map);

end Constants;