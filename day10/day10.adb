-- adbr 2021-12-17

with Ada.Text_IO;            use Ada.Text_IO;
with Ada.Command_Line;       use Ada.Command_Line;
with Ada.Containers.Vectors; use Ada.Containers;

procedure Day10 is
   
   Value_Error : exception;
   
   type Token_Type is ('(', '[', '{', '<', ')', ']', '}', '>');
   subtype Open_Token  is Token_Type range '(' .. '<';
   subtype Close_Token is Token_Type range ')' .. '>';
   
   package Token_Stack is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Token_Type);
   
   type Status_Type is (Ok, Corrupted, Incomplete);
   type Error_Type is
      record
         Status : Status_Type;
         Token  : Token_Type;
         Stack  : Token_Stack.Vector;
      end record;
   
   procedure Print_Stack (Stack : Token_Stack.Vector) is
   begin
      for T of Stack loop 
         Put (T'Img);
         Put (", ");
      end loop;
      New_Line;
   end Print_Stack;

   procedure Check_Line (Line : String; Error : out Error_Type) is
      
      function To_Token (C : Character) return Token_Type is
        (Token_Type'Value (Character'Image (C)));
      
      function To_Character (T : Token_Type) return Character is
        (Character'Value (Token_Type'Image (T)));
      
      To_Open_Token : constant array (Close_Token) of Open_Token :=
        (')' => '(', ']' => '[', '}' => '{', '>' => '<');
      
      T     : Token_Type;
      Stack : Token_Stack.Vector;
   begin
      for I in Line'Range loop
         T := To_Token (Line (I));
         case T is
            when Open_Token =>
               Stack.Append (T);
            when Close_Token =>
               if Stack.Last_Element /= To_Open_Token (T) then
                  Error.Status := Corrupted;
                  Error.Token := T;
                  return;
               else
                  Stack.Delete_Last;
               end if;
         end case;
      end loop;
      if Stack.Length > 0 then
         Error.Status := Incomplete;
         Error.Stack := Stack;
      else
         Error.Status := Ok;
      end if;
   end Check_Line;

   procedure Part_1 (File_Name : String) is
      Corrupted_Value : constant array (Close_Token) of Positive :=
        (')' => 3, ']' => 57, '}' => 1197, '>' => 25137);
      Result : Natural := 0;
      File : File_Type;
   begin
      Open (File, In_File, File_Name);
      while not End_Of_File (File) loop
         declare
            Line  : String := Get_Line (File);
            Error : Error_Type;
         begin
            Check_Line (Line, Error);
            if Error.Status = Corrupted then
               Result := Result + Corrupted_Value (Error.Token);
            end if;
         end;
      end loop;
      Close (File);
      
      Put_Line ("Part 1:");
      Put_Line ("  Syntax error score:" & Result'Img);
   end Part_1;
   
   procedure Part_2 (File_Name : String) is
      
      type Score_Type is range 0 .. Long_Integer'Last;
      
      function Incomplete_Score (Stack : Token_Stack.Vector) return Score_Type
      is
         
         To_Close_Token : constant array (Open_Token) of Close_Token :=
           ('(' => ')', '[' => ']', '{' => '}', '<' => '>');
         
         Incomplete_Value : constant array (Close_Token) of Score_Type :=
           (')' => 1, ']' => 2, '}' => 3, '>' => 4);
         
         Result : Score_Type := 0;
      begin
         for I in reverse Stack.First_Index .. Stack.Last_Index loop
            declare
               OT : Open_Token := Stack.Element (I);
               CT : Close_Token := To_Close_Token (OT);
            begin
               Result := Result * 5 + Incomplete_Value (CT);
            end;
         end loop;
         return Result;
      end Incomplete_Score;
      
      package Score_Vector is new Ada.Containers.Vectors
        (Index_Type => Positive,
         Element_Type => Score_Type);
      
      package Score_Sorting is new Score_Vector.Generic_Sorting;
      
   -- Start of processing for Part_2
      
      Result : Score_Type := 0;
      File : File_Type;
      Scores : Score_Vector.Vector;
   begin
      Open (File, In_File, File_Name);
      while not End_Of_File (File) loop
         declare
            Line : String := Get_Line (File);
            Error : Error_Type;
         begin
            Check_Line(Line, Error);
            if Error.Status = Incomplete then
               Scores.Append (Incomplete_Score (Error.Stack));
            end if;
         end;
      end loop;
      Close (File);
      
      Score_Sorting.Sort (Scores);
      
      if (Scores.Length mod 2) = 0 then
         raise Value_Error with "Not odd number of scores";
      end if;
      declare
         Middle_Index : Positive :=
           (Scores.First_Index + Scores.Last_Index) / 2;
      begin
         Result := Scores.Element (Middle_Index);
      end;
      
      Put_Line ("Part 2:");
      Put_Line ("  Middle score:" & Result'Img);
   end Part_2;

begin
   if Argument_Count /= 1 then
      Put_Line ("Usage: ./day10 <inputfile>");
      return;
   end if;
   Part_1 (Argument (1));
   Part_2 (Argument (1));
end Day10;

--  Part 1:
--    Syntax error score: 362271
--  Part 2:
--    Middle score: 1698395182
