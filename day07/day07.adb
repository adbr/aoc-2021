--  adbr 2021-12-11

with Ada.Text_IO;            use Ada.Text_IO;
with Ada.Command_Line;       use Ada.Command_Line;
with Ada.Containers.Vectors; use Ada.Containers;
  
procedure Day07 is
   
   Value_Error : exception;
   
   type Position is new Natural;
   type Fuel_Type is new Natural;
   
   package Position_IO is new Ada.Text_IO.Integer_IO (Num => Position);
   use Position_IO;

   package Position_Vector is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Position);
   
   procedure Read_Data
     (File_Name : String;
      Positions : out Position_Vector.Vector)
   is
      File      : File_Type;
      Pos       : Position;
      Separator : String := ",";
   begin
      Open (File, In_File, File_Name);
      Get (File, Pos);
      Positions.Append (Pos);
      while not End_Of_File (File) loop
         Get (File, Separator);
         Get (File, Pos);
         Positions.Append (Pos);
      end loop;
      Close (File);
   end Read_Data;
   
   function Least_Fuel (File_Name : String) return Fuel_Type is
      
      function Min (Positions : Position_Vector.Vector)return Position is
         Result : Position;
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
      
      function Max (Positions : Position_Vector.Vector) return Position is
         Result : Position;
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
      
      function Fuel (Positions : Position_Vector.Vector; Dest : Position)
                    return Fuel_Type is
         Result : Fuel_Type := 0;
      begin
         for P of Positions loop
            Result := Result + Fuel_Type (abs (Dest - P));
         end loop;
         return Result;
      end Fuel;
      
      --  Start of processing for Least_Fuel
      
      Fuel_Min  : Fuel_Type;
      First     : Boolean := True;
      Positions : Position_Vector.Vector;
   begin
      Read_Data (File_Name, Positions);
      for P in Min (Positions) .. Max (Positions) loop
         declare
            F : Fuel_Type := Fuel (Positions, P);
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
   
begin
   if Argument_Count /= 1 then
      Put_Line ("Usage: ./day07 <filename>");
      return;
   end if;
   
   Put_Line ("Part 1:");
   Put_Line ("  Least fuel:" & Least_Fuel (Argument (1))'Img);
end Day07;

--  Part 1:
--    Least fuel: 336040
