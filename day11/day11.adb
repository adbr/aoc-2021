-- adbr 2021-12-21

with Ada.Text_IO;      use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;

procedure Day11 is
   
   Steps : constant Positive := 100;
   
   type Energy_Type is new Natural;
   Min_Energy : constant Energy_Type := 0;
   Max_Energy : constant Energy_Type := 9;
   
   type Octopus_Type is
      record
         Energy  : Energy_Type;
         Flashed : Boolean := False;
      end record;
   
   subtype Index_Type is Integer range 1 .. 10;
   type Grid_Type is array (Index_Type, Index_Type) of Octopus_Type;
   
   package Energy_IO is new Ada.Text_IO.Integer_IO (Energy_Type);
   
   procedure Print_Grid (Grid : Grid_Type) is
   begin
      for R in Grid'Range (1) loop
         for C in Grid'Range (2) loop
            Energy_IO.Put (Grid (R, C).Energy, Width => 2);
         end loop;
         New_Line;
      end loop;
   end Print_Grid;

   procedure Part_1 (File_Name : String) is
      
      procedure Read_Data (File_Name : String; Grid : out Grid_Type) is
         File : File_Type;
         E : Energy_Type;
      begin
         Open (File, In_File, File_Name);
         while not End_Of_File (File) loop
            for R in Grid'Range (1) loop
               for C in Grid'Range (2) loop
                  Energy_IO.Get (File, E, Width => 1);
                  Grid (R, C) := (Energy => E, Flashed => False);
               end loop;
               Skip_Line (File);
            end loop;
         end loop;
         Close (File);
      end Read_Data;
      
      procedure Flash (Grid : in out Grid_Type; Row, Col : Index_Type) is
         
         subtype Extended_Index is Index_Type'Base range
           Index_Type'First - 1 .. Index_Type'Last + 1;
         
         function Is_Valid_Index (Grid : Grid_Type; R, C : Extended_Index)
                                 return Boolean
         is (R >= Grid'First (1) and R <= Grid'Last (1) and
             C >= Grid'First (2) and C <= Grid'Last (2));

      begin
         Grid (Row, Col).Flashed := True;
         
         -- check neighbours
         for I in -1 .. 1 loop
            for J in -1 .. 1 loop
               if (I /= 0 or J /= 0) and
                 Is_Valid_Index (Grid, Row + I, Col + J)
               then
                  declare
                     R : Index_Type := Row + I;
                     C : Index_Type := Col + J;
                     O : Octopus_Type renames Grid(R, C);
                  begin
                     O.Energy := O.Energy + 1;
                     if O.Energy > Max_Energy and not O.Flashed then
                        Flash (Grid, R, C);
                     end if;
                  end;
               end if;
            end loop;
         end loop;
         
      end Flash;
      
      -- Local variables
      
      Result : Natural := 0;
      Grid : Grid_Type;
      
   -- Start of processing for Part_1
      
   begin
      Read_Data (File_Name, Grid);
      
      Put_Line ("Initial:");
      Print_Grid (Grid);
      
      for I in 1 .. Steps loop
         
         --  increase energy level:
         for O of Grid loop
            O.Energy := O.Energy + 1;
         end loop;
         
         --  flash:
         for R in Grid'Range (1) loop
            for C in Grid'Range (2) loop
               if Grid(R, C).Energy > Max_Energy and
                 not Grid(R, C).Flashed
               then
                  Flash (Grid, R, C);
               end if;
            end loop;
         end loop;
         
         --  reset flashed:
         for O of Grid loop
            if O.Flashed then
               O.Energy := Min_Energy;
               O.Flashed := False;
               Result := Result + 1;
            end if;
         end loop;
      end loop;
      
      New_Line;
      Put_Line ("After" & Steps'Img & " steps:");
      Print_Grid (Grid);
      
      Put_Line ("Part 1:");
      Put_Line ("  Total flashes after" & Steps'Img & " steps:" & Result'Img);
   end Part_1;

begin
   if Argument_Count /= 1 then
      Put_Line ("Usage: ./day11 <inputfile>");
      return;
   end if;
   
   Part_1 (Argument (1));
end Day11;

--  Part 1:
--    Total flashes after 100 steps: 1627
