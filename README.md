# GSAT - the Gnu Sugar Ada Transpilor

![Logo](./doc/SugarAda_v2.png)

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
## Progress
As of the 25/02/25, I can re-generate this much of the [gsat.adb](./src/gsat.adb) file : 
```ada
with Ada.Text_IO; use Ada.Text_IO;  
with Ada.Exceptions; use Ada.Exceptions;  
with Ada.Calendar; use Ada.Calendar;  
with Ada.Command_Line;  
with Expressions.File; use Expressions.File;  
with Exceptions.Arguments; use Exceptions.Arguments;  
with T_Buffer;  
with Gsat_System;  
with Constants;  
with Visitor;  
 
procedure Gsat is
 
   package CLI renames Ada.Command_Line;
   Code_Tokens : T_Buffer.Code_Buffer;
   Non_Textual_Keywords : Constants.Keyword.Map;
   --   Defined here so that It ought not to be re-Init at each file .
   V_Parse : Visitor.Visitor_Parse;
   V_Print : Visitor.Visitor_Print;
 
begin
 
end Gsat;
```
