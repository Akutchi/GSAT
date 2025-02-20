# GSAT - the Gnu Sugar Ada Transpilor

## Description

This project is a transpilor from Sugar Ada (a custom version of Ada) to Plain Ada.
Sugar Ada is born from my desir to implement some new features to Ada, but without
the need to modify GCC as a consequence (might try that in the future).

The features that I want to develop as of now are thus,

- Map initialization
```ada
declare
    M : Map := (1 to "Ada", 2 to "C", 3 to "Rust"); --  equivalent to doing several M.Include
begin
    --  ...
end;
```

- Array Inlooping
```ada
declare
    A  : array (1 .. 3) of Positive;
begin
    A.ForEach (it => Put_Line (it));
end;
```

- Null Ternary operator
```ada
declare
    S : Ada.Strings.Unbounded.Unbounded_String := ...
    B : Boolean := S ?: "Default value" --  equivalent to (if S = null then "Default_Value" else S);
begin
    --  ...
end;
```
