-- adbr 2021-12-03

--  Part 1:
--    Gamma: 1816
--    Epsilon: 2279
--    Multiplication: 4138664

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;    use Ada.Command_Line;

procedure Day03 is
   
   procedure Get_Gamma_Epsilon (File_Name : String;
                                Gamma     : out Integer;
                                Epsilon   : out Integer) is
      
      subtype Bit_Number is Integer range 1 .. 12;
      type Bit_Counter is array (Bit_Number) of Natural;
      subtype Bit is Integer range 0 .. 1;
      type Binary is array (Bit_Number) of Bit;
      
      function Binary_To_Integer (V : Binary) return Integer is
         Result : Integer := 0;
      begin
         for I in V'Range loop
            Result := Result * 2 + V(I);
         end loop;
         return Result;
      end Binary_To_Integer;
      
      Invalid_Value : exception;
      
      File         : File_Type;
      Line         : String (Bit_Number);
      One_Counter  : Bit_Counter := (others => 0);
      Zero_Counter : Bit_Counter := (others => 0);
      
      Gamma_Bin   : Binary;
      Epsilon_Bin : Binary;
   begin
      Gamma   := 0;
      Epsilon := 0;
      
      Open (File, In_File, File_Name);
      while not End_Of_File (File) loop
         Get (File, Line);
         for I in Bit_Number loop
            case Line (I) is
               when '1' =>
                  One_Counter (I) := One_Counter (I) + 1;
               when '0' =>
                  Zero_Counter (I) := Zero_Counter (I) + 1;
               when others =>
                  raise Invalid_Value with "" & Line (I);
            end case;
         end loop;
      end loop;
      
      for I in Bit_Number loop
         if One_Counter (I) > Zero_Counter (I) then
            Gamma_Bin (I)   := 1;
            Epsilon_Bin (I) := 0;
         else
            Epsilon_Bin (I) := 1;
            Gamma_Bin (I)   := 0;
         end if;
      end loop;
      Close (File);
      
      Gamma := Binary_To_Integer (Gamma_Bin);
      Epsilon := Binary_To_Integer (Epsilon_Bin);
   end Get_Gamma_Epsilon;
   
   Gamma, Epsilon : Integer;
begin
   if Argument_Count /= 1 then
      Put_Line ("Usage: ./day03 <filename>");
      Set_Exit_Status (Failure);
      return;
   end if;
   
   Get_Gamma_Epsilon (Argument (1), Gamma, Epsilon);
   Put_Line ("Part 1:");
   Put_Line ("  Gamma:" & Integer'Image (Gamma));
   Put_Line ("  Epsilon:" & Integer'Image (Epsilon));
   Put_Line ("  Multiplication:" & Integer'Image (Gamma * Epsilon));
end Day03;
