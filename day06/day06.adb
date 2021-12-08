-- adbr 2021-12-07

with Ada.Text_IO;            use Ada.Text_IO;
with Ada.Command_Line;       use Ada.Command_Line;
with Ada.Containers.Vectors; use Ada.Containers;
with Ada.Containers.Hashed_Maps;

procedure Day06 is
   
   type Fish_Timer is range 0 .. 8;  -- reproduction timer
   type Counter is new Long_Integer; -- fish counter
   type Period is new Natural;       -- number of days

   package Timer_IO is new Ada.Text_IO.Integer_IO (Fish_Timer);
   use Timer_IO;
      
   package Fish_Vector is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Fish_Timer);
   
   type Child_Key is
      record
         Timer : Fish_Timer;
         Days  : Period;
      end record;
   
   function Hash (Key : Child_Key) return Hash_Type is
   begin
      return Hash_Type (Integer (Key.Timer) * Integer (Key.Days));
   end Hash;
   
   function Equivalent_Keys (Left, Right : Child_Key) return Boolean is
   begin
      return Left.Timer = Right.Timer and Left.Days = Right.Days;
   end Equivalent_Keys;
   
   package Child_Map is new Ada.Containers.Hashed_Maps
     (Key_Type        => Child_Key,
      Element_Type    => Counter,
      Hash            => Hash,
      Equivalent_Keys => Equivalent_Keys);
   
   Child_Cache : Child_Map.Map;
   
   function Num_Lanternfish
     (File_Name : String;
      Days : Period) return Counter
   is
      
      procedure Read_Data (File_Name : String;
                           Fishes : out Fish_Vector.Vector)
      is
         File      : File_Type;
         Timer     : Fish_Timer;
         Separator : Character;
      begin
         Open (File, In_File, File_Name);
         Get (File, Timer);
         Fishes.Append (Timer);
         while not End_Of_File (File) loop
            Get (File, Separator);
            Get (File, Timer);
            Fishes.Append (Timer);
         end loop;
         Close (File);
      end Read_Data;
      
      function Childs (Timer : Fish_Timer; Days : Period) return Counter
      is
         Result : Counter := 0;
         T : Fish_Timer := Timer;
         Key : Child_Key;
      begin
         Key := (Timer => Timer, Days => Days);
         if Child_Cache.Contains (Key) then
            return Child_Cache.Element (Key);
         end if;
         
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
         
         Child_Cache.Insert (Key, Result);
         return Result;
      end Childs;
      
   -- Start of processing for Num_Lanternfish
      
      Fishes : Fish_Vector.Vector;
      Result : Counter := 0;
   begin
      Read_Data (File_Name, Fishes);
      
      for F of Fishes loop
         Result := Result + 1;
         Result := Result + Childs (F, Days);
      end loop;
      
      return Result;
   end Num_Lanternfish;
   
begin
   if Argument_Count /= 1 then
      Put_Line ("Usage: ./day06 <filename>");
      Set_Exit_Status (Failure);
      return;
   end if;
   
   Put_Line ("Part 1:");
   Put_Line ("  Number lanternfish after 80 days:" &
               Num_Lanternfish (File_Name => Argument (1),
                                Days      => 80)'Img);
   Put_Line ("Part 2:");
   Put_Line ("  Number lanternfish after 256 days:" &
               Num_Lanternfish (File_Name => Argument (1),
                                Days      => 256)'Img);
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
--    adbr@kwarc:~/.../day06$ time ./day06 example.txt 
--    Part 1:
--      Number lanternfish after 80 days: 5934
--    Part 2:
--      Number lanternfish after 256 days: 26984457539
--    
--    real	0m0.014s
--    user	0m0.013s
--    sys	0m0.001s

--    adbr@kwarc:~/.../day06$ time ./day06 input.txt 
--    Part 1:
--      Number lanternfish after 80 days: 374994
--    Part 2:
--      Number lanternfish after 256 days: 1686252324092
--
--    real	0m0.015s
--    user	0m0.007s
--    sys	0m0.007s
