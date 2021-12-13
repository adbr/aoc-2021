-- adbr 2021-12-12

with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Command_Line;      use Ada.Command_Line;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings;           use Ada.Strings;
with Ada.Strings.Fixed;     use Ada.Strings.Fixed;
with Ada.Containers.Vectors;

procedure Day08 is
   
   type Pattern_Type is new Unbounded_String;
   type Input_Patterns_Type is array (1 .. 10) of Pattern_Type;
   type Output_Patterns_Type is array (1 .. 4) of Pattern_Type;
   
   type Entry_Type is
      record
         Input_Patterns  : Input_Patterns_Type;
         Output_Patterns : Output_Patterns_Type;
      end record;
   
   package Entry_Vector is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Entry_Type);
   
   procedure Read_Data (File_Name : String;
                        Entries : out Entry_Vector.Vector)
   is
      
      function Parse_Pattern (Line : String; Idx : in out Natural)
                             return Unbounded_String is
         
         function Is_Segment_Code (C : Character) return Boolean is
         begin
            case C is
               when 'a' | 'b' | 'c' | 'd' | 'e' | 'f' | 'g' =>
                  return True;
               when others =>
                  return False;
            end case;
         end Is_Segment_Code;
         
         Result : Unbounded_String := To_Unbounded_String ("");
      begin
         while Line (Idx) = ' ' loop
            Idx := Idx + 1;
         end loop;
         while Idx <= Line'Last and then Is_Segment_Code (Line (Idx)) loop
            Result := Result & Line (Idx);
            Idx := Idx + 1;
         end loop;
         return Result;
      end Parse_Pattern;
      
      procedure Skip_Separator (Line : String; Idx : in out Natural) is
      begin
         while Line (Idx) = ' '  or Line (Idx) = '|' loop
            Idx := Idx + 1;
         end loop;
      end Skip_Separator;
      
      function Parse_Entry (Line : String) return Entry_Type is
         Result : Entry_Type;
         Pattern : Unbounded_String;
         Idx : Natural := Line'First;
      begin
         for I in Result.Input_Patterns'Range loop
            Pattern := Parse_Pattern (Line, Idx);
            Result.Input_Patterns (I) := Pattern_Type (Pattern);
         end loop;
         
         Skip_Separator (Line, Idx);
         
         for I in Result.Output_Patterns'Range loop
            Pattern := Parse_Pattern (Line, Idx);
            Result.Output_Patterns (I) := Pattern_Type (Pattern);
         end loop;
         
         return Result;
      end Parse_Entry;
      
   --  Start of processing for Read_Data
      
      File : File_Type;
   begin
      Open (File, In_File, File_Name);
      while not End_Of_File (File) loop
         declare
            Line : String     := Get_Line (File);
            E    : Entry_Type := Parse_Entry (Line);
         begin
            Entries.Append (E);
         end;
      end loop;
      Close (File);
   end Read_Data;

   function Number_Of_Digits (File_Name : String) return Natural is
      
      function Is_Digit_1478 (Pattern : Pattern_Type) return Boolean is
      begin
         case Length (Pattern) is
            when
              -- number of segments
              2 | -- digit 1
              4 | -- digit 4
              3 | -- digit 7
              7   -- digit 8
              => return True;
            when others
              => return False;
         end case;
      end Is_Digit_1478;

      Entries : Entry_Vector.Vector;
      Result : Natural := 0;
   begin
      Read_Data (File_Name, Entries);
      for E of Entries loop
         for I in E.Output_Patterns'Range loop
            if Is_Digit_1478 (E.Output_Patterns (I)) then
               Result := Result + 1;
            end if;
         end loop;
      end loop;
      return Result;
   end Number_Of_Digits;

begin
   if Argument_Count /= 1 then
      Put_Line ("Usage: ./day08 <filename>");
      return;
   end if;

   Put_Line ("Part 1:");
   Put_Line ("  Number of 1, 4, 7, or 8 digits:" &
               Number_Of_Digits (Argument (1))'Img);
end Day08;

--  Part 1:
--    Number of 1, 4, 7, or 8 digits: 470
