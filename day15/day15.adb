-- adbr 2022-01-08

with Ada.Text_IO;                use Ada.Text_IO;
with Ada.Command_Line;           use Ada.Command_Line;
with Ada.Containers.Hashed_Maps; use Ada.Containers;
with Ada.Containers.Vectors;
with Ada.Strings.Fixed;

procedure Day15 is

   pragma Assertion_Policy (Check);

   type Coordinate_Type is range 1 .. Positive'Last;

   type Position_Type is
      record
         Row : Coordinate_Type;
         Col : Coordinate_Type;
      end record;

   subtype Risk_Type is Positive range 1 .. 9;

   -- Risk_Maps package

   function Hash (Key : Position_Type) return Hash_Type is
     (Hash_Type ((Key.Row + Key.Col) * Key.Row));

   function Equivalent_Keys (Left, Right : Position_Type) return Boolean is
     (Left.Row = Right.Row and Left.Col = Right.Col);

   package Risk_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Position_Type,
      Element_Type    => Risk_Type,
      Hash            => Hash,
      Equivalent_Keys => Equivalent_Keys);

   subtype Risk_Map is Risk_Maps.Map;
   
   
   package Total_Risk_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Position_Type,
      Element_Type    => Natural,
      Hash            => Hash,
      Equivalent_Keys => Equivalent_Keys);
   
   subtype Total_Risk_Map is Total_Risk_Maps.Map;
   
   Total_Risk_Cache : Total_Risk_Map;

   
   package Risk_IO is new Ada.Text_IO.Integer_IO (Risk_Type);


   package Position_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Position_Type);

   subtype Position_Vector is Position_Vectors.Vector;


   package Natural_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Natural);

   subtype Natural_Vector is Natural_Vectors.Vector;

   -- Subprograms decalrations

   function Min_Position (Cavern : Risk_Map) return Position_Type;
   function Max_Position (Cavern : Risk_Map) return Position_Type;

   -- Subprograms

   function Image (Position : Position_Type) return String is
      use Ada.Strings;
      use Ada.Strings.Fixed;
      Row : String := Trim (Position.Row'Img, Left);
      Col : String := Trim (Position.Col'Img, Left);
   begin
      return "(" & Row & ", " & Col & ")";
   end Image;

   procedure Print_Cavern (Cavern : Risk_Map) is
      Min   : Position_Type := Min_Position (Cavern);
      Max   : Position_Type := Max_Position (Cavern);
      Image : array (Min.Row .. Max.Row, Min.Col .. Max.Col) of Risk_Type;
   begin
      for R in Image'Range (1) loop
         for C in Image'Range (2) loop
            Image (R, C) := Cavern.Element (Position_Type'(R, C));
         end loop;
      end loop;

      for R in Image'Range (1) loop
         for C in Image'Range (2) loop
            Put (Image (R, C)'Img);
         end loop;
         New_Line;
      end loop;
   end Print_Cavern;

   procedure Read_Data (File_Name : String; Cavern : out Risk_Map) is
      File : File_Type;
      Row  : Coordinate_Type := Coordinate_Type'First;
      Col  : Coordinate_Type := Coordinate_Type'First;
      Risk : Risk_Type;
   begin
      Open (File, In_File, File_Name);

      while not End_Of_File (File) loop
         Risk_IO.Get (File, Risk, Width => Risk_Type'Width - 1);
         -- Width: number digits of type without sign
         Cavern.Insert (Position_Type'(Row, Col), Risk);

         if End_Of_Line (File) then
            Col := Coordinate_Type'First;
            Row := Row + 1;
            Skip_Line (File);
         else
            Col := Col + 1;
         end if;
      end loop;

      Close (File);
   end Read_Data;

   function Min_Position (Cavern : Risk_Map) return Position_Type is
   begin
      return Position_Type'(Row => Coordinate_Type'First,
                            Col => Coordinate_Type'First);
   end Min_Position;

   function Max_Position (Cavern : Risk_Map) return Position_Type
   is
      Max : Position_Type := Min_Position (Cavern);
   begin
      for C in Cavern.Iterate loop
         declare
            Pos : Position_Type := Risk_Maps.Key (C);
         begin
            if Pos.Row > Max.Row or Pos.Col > Max.Col then
               Max := Pos;
            end if;
         end;
      end loop;
      return Max;
   end Max_Position;

   function Neighbours (Pos : Position_Type; Cavern : Risk_Map)
                       return Position_Vector is

      function Is_Valid (Pos : Position_Type) return Boolean is
        (Cavern.Contains (Pos));

      Result : Position_Vector;
      Pos1   : Position_Type;
   begin
      Pos1 := (Row => Pos.Row, Col => Pos.Col + 1);
      if Is_Valid (Pos1) then
         Result.Append (Pos1);
      end if;

      Pos1 := (Row => Pos.Row + 1, Col => Pos.Col);
      if Is_Valid (Pos1) then
         Result.Append (Pos1);
      end if;

      return Result;
   end Neighbours;
   
   function Min (V : Natural_Vector) return Natural
   with Pre => (V.Length > 0)
   is
      Result : Natural;
      First  : Boolean := True;
   begin
      for N of V loop
         if First then
            Result := N;
            First := False;
         else
            if N < Result then
               Result := N;
            end if;
         end if;
      end loop;
      return Result;
   end Min;
   
   function Lowest_Total_Risk
     (Pos    : Position_Type;
      Dst    : Position_Type;
      Cavern : Risk_Map) return Natural
   is
      Result : Natural;
   begin
      if Pos = Dst then
         return Cavern.Element (Pos);
      end if;
      
      if Total_Risk_Cache.Contains (Pos) then
         return Total_Risk_Cache.Element (Pos);
      end if;

      declare
         Nbs : Position_Vector := Neighbours(Pos, Cavern);
         Risks : Natural_Vector;
      begin
         for N of Nbs loop
            Risks.Append (Lowest_Total_Risk (N, Dst, Cavern));
         end loop;
         Result := Cavern.Element (Pos) + Min (Risks);
      end;
      
      Total_Risk_Cache.Insert (Pos, Result);
      return Result;
   end Lowest_Total_Risk;

   procedure Part_1 (Cavern : Risk_Map) is
      Result : Natural := 0;
   begin
      Result := Lowest_Total_Risk (Min_Position (Cavern),
                                   Max_Position (Cavern),
                                   Cavern);
      
      -- "the starting position is never entered, so its risk is not
      -- counted"
      Result := Result - Cavern.Element (Min_Position (Cavern));

      Put_Line ("Part 1:");
      Put_Line ("  Lowest total risk of path:" & Result'Img);
   end Part_1;

   -- Variables

   Cavern : Risk_Map;

begin
   if Argument_Count /= 1 then
      Put_Line ("Usage: ./day15 <inputfile>");
      return;
   end if;

   Read_Data (Argument (1), Cavern);

   Put_Line ("Num of positions:" & Cavern.Length'Img);
   Put_Line ("Min Position: " & Image (Min_Position (Cavern)));
   Put_Line ("Max Position: " & Image (Max_Position (Cavern)));

   Print_Cavern (Cavern);

   Part_1 (Cavern);
end Day15;
