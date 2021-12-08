-- adbr 2021-12-07

with Ada.Text_IO;      use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Containers.Vectors;

procedure Day06 is
   
   type Timer_Type is range 0 .. 8;

   package Timer_IO is new Ada.Text_IO.Integer_IO (Timer_Type);
   use Timer_IO;
      
   package Fish_Vector is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Timer_Type);
   
   function Num_Lanternfish_Part1
     (File_Name : String;
      Days      : Positive) return Natural
   is
      File   : File_Type;
      Timer  : Timer_Type;
      Fishes : Fish_Vector.Vector;
      Sep    : Character;
   begin
      Open (File, In_File, File_Name);
      Get (File, Timer);
      Fishes.Append (Timer);
      while not End_Of_File (File) loop
         Get (File, Sep);
         Get (File, Timer);
         Fishes.Append (Timer);
      end loop;
      Close (File);
      
      for Day in 1 .. Days loop
         for I in Fishes.First_Index .. Fishes.Last_Index loop
            declare
               E : Timer_Type := Fishes.Element (I);
            begin
               if E = 0 then
                  Fishes.Replace_Element (I, 6);
                  Fishes.Append (8);
               else
                  Fishes.Replace_Element (I, E - 1);
               end if;
            end;
         end loop;
      end loop;
      
      return Natural (Fishes.Length);
   end Num_Lanternfish_Part1;
   
   function Num_Lanternfish_Part2
     (File_Name : String;
      Days      : Positive) return Long_Integer
   is
      
      function Childs (Timer : Timer_Type; Days : Natural)
                      return Long_Integer
      is
         Result : Long_Integer := 0;
         T      : Timer_Type   := Timer;
      begin
         -- Put_Line ("Childs: Timer = " & Timer'Img & ", Days = " & Days'Img);
         for D in 1 .. Days loop
            if T = 0 then
               Result := Result + 1;
               Result := Result + Childs (8, Days - D);
               T := 6;
            else
               T := T - 1;
            end if;
         end loop;
         -- Put_Line ("Childs: Result: " & Result'Img);
         return Result;
      end Childs;
      
      use Timer_IO;
      
      File   : File_Type;
      Timer  : Timer_Type;
      Fishes : Fish_Vector.Vector;
      Sep    : Character;
      Result : Long_Integer := 0;
   begin
      Open (File, In_File, File_Name);
      Get (File, Timer);
      Fishes.Append (Timer);
      while not End_Of_File (File) loop
         Get (File, Sep);
         Get (File, Timer);
         Fishes.Append (Timer);
      end loop;
      Close (File);
      
      for F of Fishes loop
         Result := Result + 1;
         Result := Result + Childs (F, Days);
      end loop;
      
      return Result;
   end Num_Lanternfish_Part2;
   
begin
   if Argument_Count /= 1 then
      Put_Line ("Usage: ./day06 <filename>");
      Set_Exit_Status (Failure);
      return;
   end if;
   
   Put_Line ("Part 1:");
   Put_Line ("  Number lanternfish after 80 days:" &
                Num_Lanternfish_Part1 (File_Name => Argument (1),
                                       Days      => 80)'Img);
   Put_Line ("Part 2:");
   Put_Line ("  Number lanternfish:" &
                Num_Lanternfish_Part2 (File_Name => Argument (1),
                                       Days      => 80)'Img);
end Day06;

--  Part 1:
--    Number lanternfish: 374994

--  Using vector:
--    adbr@kwarc:~/.../day06$ ./day06 example.txt 
--    Part 1:
--      Number lanternfish after 80 days: 5934
--    Part 2:
--    
--    raised CONSTRAINT_ERROR : Day06.Fish_Vector.Append_Slow_Path:
--    vector is already at its maximum length

--  Using recursive childs counting:
--  adbr@kwarc:~/.../day06$ ./day06 example.txt 
--  Part 1:
--    Number lanternfish after 80 days: 5934
--  Part 2:
--    Number lanternfish: 26984457539
