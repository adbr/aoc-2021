--  adbr 2021-12-01
--
--  $ ./day01 input.txt
--  Part 1:
--    Numeber of increased measurements: 1602
--  Part 2:
--    Numeber of increased sums: 1633


with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Command_Line;    use Ada.Command_Line;

procedure Day01 is
   
   function Num_Of_Increased_Values (File_Name : String) return Natural is
      Result   : Natural := 0;
      File     : File_Type;
      Current  : Integer;
      Previous : Integer;
   begin
      Open (File, In_File, File_Name);
      
      Get (File, Previous);
      while not End_Of_File (File) loop
         Get (File, Current);
         if Current > Previous then
            Result := Result + 1;
         end if;
         Previous := Current;
      end loop;
      
      Close (File);
      return Result;
   end Num_Of_Increased_Values;
   
   -- Compare sums of 3 element windows.
   function Num_Of_Increased_Sums (File_Name : String) return Natural is
      Result         : Natural := 0;
      File           : File_Type;
      V1, V2, V3, V4 : Integer;
   begin
      Open (File, In_File, File_Name);
      
      Get (File, V1);
      Get (File, V2);
      Get (File, V3);
      while not End_Of_File (File) loop
         Get (File, V4);
         if (V2 + V3 + V4) > (V1 + V2 + V3) then
            Result := Result + 1;
         end if;
         V1 := V2;
         V2 := V3;
         V3 := V4;
      end loop;
      
      Close (File);
      return Result;
   end Num_Of_Increased_Sums;
   
begin
   if Argument_Count /= 1 then
      Put_Line ("Usage: ./day01 <filename>");
      Set_Exit_Status (Failure);
      return;
   end if;
   
   Put_Line ("Part 1:");
   Put_Line ("  Numeber of increased measurements:"
               & Natural'Image (Num_Of_Increased_Values (Argument (1))));
   
   Put_Line ("Part 2:");
   Put_Line ("  Numeber of increased sums:"
               & Natural'Image (Num_Of_Increased_Sums (Argument (1))));
end Day01;
