-- adbr 2021-12-03

--  Part 1:
--    Gamma: 1816
--    Epsilon: 2279
--    Multiplication: 4138664
--  Part 2:
--    Oxygen: 2031
--    CO2: 2104
--    Multiplication: 4273224

with Ada.Text_IO;            use Ada.Text_IO;
with Ada.Command_Line;       use Ada.Command_Line;
with Ada.Containers.Vectors; use Ada.Containers;

procedure Day03 is
      
   subtype Bit_Number is Integer range 1 .. 12;
   --  subtype Bit_Number is Integer range 1 .. 5;
   type Bit_Counter is array (Bit_Number) of Natural;
   subtype Bit is Integer range 0 .. 1;
   type Binary is array (Bit_Number) of Bit;
   subtype Binary_String is String (Bit_Number);
      
   package Binary_Vector is new Ada.Containers.Vectors
     (Index_Type => Positive,
      Element_Type => Binary );
   use Binary_Vector;
   
   Invalid_Value : exception;
      
   function Binary_To_Integer (B : Binary) return Integer is
      Result : Integer := 0;
   begin
      for I in B'Range loop
         Result := Result * 2 + B(I);
      end loop;
      return Result;
   end Binary_To_Integer;
   
   function String_To_Binary (S : Binary_String) return Binary is
      Result : Binary;
   begin
      for I in S'Range loop
         case S (I) is
            when '0' => Result (I) := 0;
            when '1' => Result (I) := 1;
            when others =>
               raise Invalid_Value with "" & S (I);
         end case;
      end loop;
      return Result;
   end String_To_Binary;
   
   procedure Get_Gamma_Epsilon (File_Name : String;
                                Gamma     : out Integer;
                                Epsilon   : out Integer) is
      One_Counter  : Bit_Counter := (others => 0);
      Zero_Counter : Bit_Counter := (others => 0);
   begin
      declare
         File : File_Type;
         Line : Binary_String;
      begin
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
         Close (File);
      end;
      
      declare
         Gamma_Bin   : Binary;
         Epsilon_Bin : Binary;
      begin
         for I in Bit_Number loop
            if One_Counter (I) > Zero_Counter (I) then
               Gamma_Bin (I)   := 1;
               Epsilon_Bin (I) := 0;
            else
               Epsilon_Bin (I) := 1;
               Gamma_Bin (I)   := 0;
            end if;
         end loop;
         Gamma := Binary_To_Integer (Gamma_Bin);
         Epsilon := Binary_To_Integer (Epsilon_Bin);
      end;
   end Get_Gamma_Epsilon;
   
   function Oxygen_Rating (File_Name : String) return Integer is
      
      function Most_Common_Bit (Report : Vector;
                                Bit_Position : Bit_Number) return Bit is
         Zeros, Ones : Natural := 0;
         C : Cursor := First (Report);
         E : Binary;
      begin
         while C /= No_Element loop
            E := Element (C);
            case E (Bit_Position) is
               when 0 => Zeros := Zeros + 1;
               when 1 => Ones  := Ones + 1;
            end case;
            C := Next (C);
         end loop;
         
         if Ones >= Zeros then
            return 1;
         else
            return 0;
         end if;
      end Most_Common_Bit;
      
      Report : Vector;
   begin
      declare
         File : File_Type;
         Line : Binary_String;
      begin
         Open (File, In_File, File_Name);
         while not End_Of_File (File) loop
            Get (File, Line);
            Append (Report, String_To_Binary (Line));
         end loop;
         Close (File);
      end;
      
      for Bit_Position in Bit_Number loop
         declare
            Bit_Criteria : Bit := Most_Common_Bit (Report, Bit_Position);
            Report_New   : Vector;
         begin
            for E of Report loop
               if E (Bit_Position) = Bit_Criteria then
                  Append (Report_New, E);
               end if;
            end loop;
            
            if Length (Report_New) = 1 then
               return Binary_To_Integer (First_Element (Report_New));
            end if;
            
            Assign (Report, Report_New);
         end;
      end loop;
      raise Invalid_Value with "Missing oxygen rating";
   end Oxygen_Rating;
   
   function CO2_Rating (File_Name : String) return Integer is
      
      function Least_Common_Bit (Report : Vector;
                                 Bit_Position : Bit_Number) return Bit is
         Zeros, Ones : Natural := 0;
      begin
         for E of Report loop
            case E (Bit_Position) is
               when 0 => Zeros := Zeros + 1;
               when 1 => Ones  := Ones + 1;
            end case;
         end loop;
         
         if Zeros <= Ones then
            return 0;
         else
            return 1;
         end if;
      end Least_Common_Bit;
      
      Report : Vector;
   begin
      declare
         File : File_Type;
         Line : Binary_String;
      begin
         Open (File, In_File, File_Name);
         while not End_Of_File (File) loop
            Get (File, Line);
            Append (Report, String_To_Binary (Line));
         end loop;
         Close (File);
      end;
      
      for Bit_Position in Bit_Number loop
         declare
            Bit_Criteria : Bit := Least_Common_Bit (Report, Bit_Position);
            Report_New   : Vector;
         begin
            for E of Report loop
               if E (Bit_Position) = Bit_Criteria then
                  Append (Report_New, E);
               end if;
            end loop;
            
            if Length (Report_New) = 1 then
               return Binary_To_Integer (First_Element (Report_New));
            end if;
            
            Assign (Report, Report_New);
         end;
      end loop;
      raise Invalid_Value with "Missing CO2 rating";
   end CO2_Rating;
   
   Gamma, Epsilon : Integer;
   Oxygen, CO2    : Integer;
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
   
   Oxygen := Oxygen_Rating (Argument (1));
   CO2 := CO2_Rating (Argument (1));
   Put_Line ("Part 2:");
   Put_Line ("  Oxygen:" & Integer'Image (Oxygen));
   Put_Line ("  CO2:" & Integer'Image (CO2));
   Put_Line ("  Multiplication:" & Integer'Image (Oxygen * CO2));
end Day03;
