-- adbr 2021-12-16

with Ada.Text_IO;            use Ada.Text_IO;
with Ada.Command_Line;       use Ada.Command_Line;
with Ada.Containers.Vectors; use Ada.Containers;

procedure Day09 is
   
   Value_Error : exception;
   
   type Height_Type is range 0 .. 9;
   type Point_Type is
      record
         X      : Positive;
         Y      : Positive;
         Height : Height_Type;
      end record;
   
   package Height_IO is new Ada.Text_IO.Integer_IO (Height_Type);
   use Height_IO;
   
   package Height_Vector is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Height_Type);
   
   package Height_Map_Vector is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Height_Vector.Vector,
      "="          => Height_Vector."=");
   
   package Point_Vector is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Point_Type);
   
   package Point_Map_Vector is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Point_Vector.Vector,
      "="          => Point_Vector."=");
   
   procedure Print_Height_Map (Map : Height_Map_Vector.Vector) is
   begin
      for R of Map loop
         for C of R loop
            Put (C);
            Put (",");
         end loop;
         New_Line;
      end loop;
   end Print_Height_Map;
   
   procedure Print_Point_Map (Map : Point_Map_Vector.Vector) is
      
      function Point_Image (P : Point_Type) return String is
      begin
         return "(" & P.X'Img & "," & P.Y'Img & "," & P.Height'Img & ")";
      end Point_Image;
      
   begin
      for R of Map loop
         Put ("size:" & R.Length'Img & " => ");
         for C of R loop
            Put (Point_Image (C));
            Put (", ");
         end loop;
         New_Line;
      end loop;
   end Print_Point_Map;

   procedure Read_Data (File_Name  : String;
                        Height_Map : out Height_Map_Vector.Vector)
   is
      File   : File_Type;
      Height : Height_Type;
      Row    : Height_Vector.Vector;
   begin
      Open (File, In_File, File_Name);
      while not End_Of_File (File) loop
         if End_Of_Line (File) then
            Height_Map.Append (Row);
            Row.Clear;
            Skip_Line (File);
         end if;
         Get (File, Height, Width => 1);
         Row.Append (Height);
      end loop;
      Height_Map.Append (Row);
      Close (File);
   end Read_Data;
      
   function Valid_Index (Height_Map : Height_Map_Vector.Vector;
                         Row        : Natural;
                         Col        : Natural) return Boolean is
   begin
      if Row < Height_Map.First_Index or
        Row > Height_Map.Last_Index
      then
         return False;
      end if;
      if Col < Height_Map.Element(Row).First_Index or
        Col > Height_Map.Element(Row).Last_Index
      then
         return False;
      end if;
      return True;
   end Valid_Index;
   
   function Element (Height_Map : Height_Map_Vector.Vector;
                     Row        : Positive;
                     Col        : Positive) return Height_Type is
   begin
      return Height_Map.Element(Row).Element(Col);
   end Element;
   
   function Adjacents
     (Height_Map : Height_Map_Vector.Vector;
      Row        : Positive;
      Col        : Positive) return Height_Vector.Vector
   is
      Adjacents : Height_Vector.Vector;
   begin
      if Valid_Index (Height_Map, Row, Col - 1) then
         Adjacents.Append (Element (Height_Map, Row, Col - 1));
      end if;
      if Valid_Index (Height_Map, Row, Col + 1) then
         Adjacents.Append (Element (Height_Map, Row, Col + 1));
      end if;
      if Valid_Index (Height_Map, Row - 1, Col) then
         Adjacents.Append (Element (Height_Map, Row - 1, Col));
      end if;
      if Valid_Index (Height_Map, Row + 1, Col) then
         Adjacents.Append (Element (Height_Map, Row + 1, Col));
      end if;
      return Adjacents;
   end Adjacents;

   function Is_Low_Point (Height_Map : Height_Map_Vector.Vector;
                          Row : Positive;
                          Col : Positive) return Boolean is
      Height    : Height_Type;
   begin
      Height := Height_Map.Element(Row).Element(Col);
      for A of Adjacents (Height_Map, Row, Col) loop
         if A <= Height then
            return False;
         end if;
      end loop;
      return True;
   end Is_Low_Point;
   
   procedure Find_Low_Points (Height_Map : Height_Map_Vector.Vector;
                              Low_Points : out Point_Vector.Vector) is
   begin
      for R in Height_Map.First_Index .. 
               Height_Map.Last_Index
      loop
         for C in Height_Map.Element(R).First_Index ..
                  Height_Map.Element(R).Last_Index
         loop
            if Is_Low_Point (Height_Map, R, C) then
               declare
                  Point : Point_Type;
               begin
                  Point := (X => R,
                            Y => C,
                            Height => Height_Map.Element(R).Element(C));
                  Low_Points.Append (Point);
               end;
            end if;
         end loop;
      end loop;
   end Find_Low_Points;

   procedure Part_1 (File_Name : String) is
      Height_Map : Height_Map_Vector.Vector;
      Low_Points : Point_Vector.Vector;
      Result     : Natural := 0;
   begin
      Read_Data (File_Name, Height_Map);
      Find_Low_Points (Height_Map, Low_Points);
      
      for P of Low_Points loop
         Result := Result + Natural (P.Height) + 1;
      end loop;
      
      Put_Line ("Part 1:");
      Put_Line ("  Sum of the risk levels: " & Result'Img);
   end Part_1;
   
   procedure Find_Basin
     (Height_Map : Height_Map_Vector.Vector;
      Row, Col   : Positive;
      Basin      : in out Point_Vector.Vector) is
      
      procedure Process_Adjacent (R, C : Natural) is
      begin
         if Valid_Index (Height_Map, R, C) then
            if Element (Height_Map, R, C) > Element (Height_Map, Row, Col) and
               Element (Height_Map, R, C) < Height_Type'Last
            then
               Find_Basin (Height_Map, R, C, Basin);
            end if;
         end if;
      end Process_Adjacent;

   begin
      declare
         Point : Point_Type := (X => Row, Y => Col, 
                                Height => Element (Height_Map, Row, Col));
      begin
         if not Basin.Contains (Point) then
            Basin.Append (Point);
         end if;
      end;
      
      Process_Adjacent (Row, Col - 1);
      Process_Adjacent (Row, Col + 1);
      Process_Adjacent (Row - 1, Col);
      Process_Adjacent (Row + 1, Col);
   end Find_Basin;

   procedure Part_2 (File_Name : String) is
      Height_Map : Height_Map_Vector.Vector;
      Low_Points : Point_Vector.Vector;
      Basins     : Point_Map_Vector.Vector;
      Result     : Natural := 0;
      
      function "<" (Left, Right : Point_Vector.Vector) return Boolean is
      begin
         return Left.Length < Right.Length;
      end "<";
      package Basin_Sorting is new Point_Map_Vector.Generic_Sorting
        ("<" => "<");
      
   begin
      Read_Data (File_Name, Height_Map);
      Find_Low_Points (Height_Map, Low_Points);
      
      for Point of Low_Points loop
         declare
            Basin : Point_Vector.Vector;
         begin
            Find_Basin (Height_Map, Point.X, Point.Y, Basin);
            Basins.Append (Basin);
         end;
      end loop;
      
      Basin_Sorting.Sort (Basins);
      if Basins.Length < 3 then
         raise Value_Error with "Not enough basins: " & Basins.Length'Img;
      end if;
      declare
         Last : Positive := Basins.Last_Index;
      begin
         Result := Natural (Basins.Element(Last).Length) *
                   Natural (Basins.Element(Last - 1).Length) *
                   Natural (Basins.Element(Last - 2).Length);
      end;
      
      Put_Line ("Part 2:");
      Put_Line ("  Multiplication of three largest basins: " & Result'Img);
   end Part_2;

begin
   if Argument_Count /= 1 then
      Put_Line ("Usage: ./day09 <inputfile>");
      return;
   end if;
   
   Part_1 (Argument (1));
   Part_2 (Argument (1));
   
end Day09;

--  Part 1:
--    Sum of the risk levels:  600
--  Part 2:
--    Multiplication of three largest basins:  987840
