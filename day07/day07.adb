--  adbr 2021-12-11

with Ada.Text_IO;            use Ada.Text_IO;
with Ada.Command_Line;       use Ada.Command_Line;
with Ada.Containers.Vectors; use Ada.Containers;
  
procedure Day07 is
   
   Value_Error : exception;
   
   type Position_Type is new Natural;
   type Fuel_Type is new Natural;
   
   package Position_IO is new Ada.Text_IO.Integer_IO
     (Num => Position_Type);
   use Position_IO;

   package Position_Vector is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Position_Type);
   
   procedure Read_Data
     (File_Name : String;
      Positions : out Position_Vector.Vector)
   is
      File      : File_Type;
      Position  : Position_Type;
      Separator : String := ",";
   begin
      Open (File, In_File, File_Name);
      Get (File, Position);
      Positions.Append (Position);
      while not End_Of_File (File) loop
         Get (File, Separator);
         Get (File, Position);
         Positions.Append (Position);
      end loop;
      Close (File);
   end Read_Data;
      
   function Min (Positions : Position_Vector.Vector) return Position_Type is
      Result : Position_Type;
   begin
      if Positions.Length > 0 then
         Result := Positions.First_Element;
      else
         raise Value_Error with "Min: vector is empty";
      end if;
      
      declare
         use Position_Vector;
         C : Cursor := Positions.First;
      begin
         while C /= No_Element loop
            if Element (C) < Result then
               Result := Element (C);
            end if;
            Next (C);
         end loop;
      end;
      return Result;
   end Min;
   
   function Max (Positions : Position_Vector.Vector) return Position_Type is
      Result : Position_Type;
   begin
      if Positions.Length > 0 then
         Result := Positions.First_Element;
      else
         raise Value_Error with "Max: vector is empty";
      end if;
      
      declare
         use Position_Vector;
         C : Cursor := Positions.First;
      begin
         while C /= No_Element loop
            if Element (C) > Result then
               Result := Element (C);
            end if;
            Next (C);
         end loop;
      end;
      return Result;
   end Max;
   
   function Least_Fuel (File_Name : String) return Fuel_Type is
      
      function Distance_Fuel (Distance : Position_Type) return Fuel_Type is
      begin
         return Fuel_Type (Distance);
      end Distance_Fuel;
      
      function Fuel_Cost (Positions   : Position_Vector.Vector;
                          Destination : Position_Type) return Fuel_Type
      is
         Fuel : Fuel_Type := 0;
      begin
         for P of Positions loop
            Fuel := Fuel + Distance_Fuel (abs (Destination - P));
         end loop;
         return Fuel;
      end Fuel_Cost;
      
      --  Start of processing for Least_Fuel
      
      Fuel_Min  : Fuel_Type;
      First     : Boolean := True;
      Positions : Position_Vector.Vector;
   begin
      Read_Data (File_Name, Positions);
      for P in Min (Positions) .. Max (Positions) loop
         declare
            F : Fuel_Type := Fuel_Cost (Positions, P);
         begin
            -- Put_Line ("pos:" & P'Img & " fuel:" & F'Img);
            if First then
               Fuel_Min := F;
               First  := False;
            else
               if F < Fuel_Min then
                  Fuel_Min := F;
               end if;
            end if;
         end;
      end loop;
      return Fuel_Min;
   end Least_Fuel;
   
   function Least_Fuel_2 (File_Name : String) return Fuel_Type is
      
      -- TODO: cache for Distance_Fuel
      
      function Distance_Fuel (Distance : Position_Type) return Fuel_Type is
         Step_Cost : Fuel_Type := 1;
         Fuel : Fuel_Type := 0;
      begin
         for I in 1 .. Distance loop
            Fuel := Fuel + Step_Cost;
            Step_Cost := Step_Cost + 1;
         end loop;
         return Fuel;
      end Distance_Fuel;
      
      function Fuel_Cost (Positions   : Position_Vector.Vector;
                          Destination : Position_Type) return Fuel_Type
      is
         Fuel : Fuel_Type := 0;
      begin
         for P of Positions loop
            Fuel := Fuel + Distance_Fuel (abs (Destination - P));
         end loop;
         return Fuel;
      end Fuel_Cost;
      
      --  Start of processing for Least_Fuel_2
      
      Fuel_Min  : Fuel_Type;
      First     : Boolean := True;
      Positions : Position_Vector.Vector;
   begin
      Read_Data (File_Name, Positions);
      for P in Min (Positions) .. Max (Positions) loop
         declare
            F : Fuel_Type := Fuel_Cost (Positions, P);
         begin
            -- Put_Line ("pos:" & P'Img & " fuel:" & F'Img);
            if First then
               Fuel_Min := F;
               First  := False;
            else
               if F < Fuel_Min then
                  Fuel_Min := F;
               end if;
            end if;
         end;
      end loop;
      return Fuel_Min;
   end Least_Fuel_2;
   
begin
   if Argument_Count /= 1 then
      Put_Line ("Usage: ./day07 <filename>");
      return;
   end if;
   
   Put_Line ("Part 1:");
   Put_Line ("  Least fuel:" & Least_Fuel (Argument (1))'Img);
   
   Put_Line ("Part 2:");
   Put_Line ("  Least fuel:" & Least_Fuel_2 (Argument (1))'Img);
end Day07;

--  $ time ./day07 input.txt 
--  Part 1:
--    Least fuel: 336040
--  Part 2:
--    Least fuel: 94813675

--  real	0m5.889s
--  user	0m5.885s
--  sys	0m0.004s
