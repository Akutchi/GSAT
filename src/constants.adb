package body Constants is

   procedure Init_Map (K_Map : in out Keyword.Map)
   is
   begin

      K_Map.Include (";_t", semi_colon_t);
      K_Map.Include (",_t", comma_t);
      K_Map.Include (":_t", colon_t);
      K_Map.Include ("(_t", left_parenthesis_t);
      K_Map.Include (")_t", right_parenthesis_t);
      K_Map.Include ("._t", dot_t);
      K_Map.Include (":=_t", assign_t);
      K_Map.Include ("'_t", text_t);
      K_Map.Include ("""_t", text_t);
      K_Map.Include ("+_t", operator_t);
      K_Map.Include ("-_t", operator_t);
      K_Map.Include ("*_t", operator_t);
      K_Map.Include ("**_t", operator_t);
      K_Map.Include ("/_t", operator_t);
      K_Map.Include (">_t", operator_t);
      K_Map.Include ("<_t", operator_t);
      K_Map.Include (">=_t", operator_t);
      K_Map.Include ("<=_t", operator_t);
      K_Map.Include ("=_t", operator_t);
      K_Map.Include ("=>_t", arrow_t);

   end Init_Map;

end Constants;