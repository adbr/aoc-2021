-- adbr 2021-12-16

with Ada.Text_IO;            use Ada.Text_IO;
with Ada.Command_Line;       use Ada.Command_Line;
with Ada.Containers.Vectors; use Ada.Containers;

procedure Day09 is
   
   type Height_Type is range 0 .. 9;
   
   package Height_IO is new Ada.Text_IO.Integer_IO (Height_Type);
   use Height_IO;
   
   package Height_Vector is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Height_Type);
   
   package Height_Map_Vector is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Height_Vector.Vector,
      "="          => Height_Vector."=");
   
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
   
   function Adjacent_Heights
     (Height_Map : Height_Map_Vector.Vector;
      Row        : Positive;
      Col        : Positive) return Height_Vector.Vector
   is
      
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
      
   --  Start of processing for Adjacent_Heights

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
   end Adjacent_Heights;

   function Is_Low_Point (Height_Map : Height_Map_Vector.Vector;
                          Row : Positive;
                          Col : Positive) return Boolean is
      Adjacents : Height_Vector.Vector;
      Height    : Height_Type;
   begin
      Adjacents := Adjacent_Heights (Height_Map, Row, Col);
      Height := Height_Map.Element(Row).Element(Col);
      for A of Adjacents loop
         if A <= Height then
            return False;
         end if;
      end loop;
      return True;
   end Is_Low_Point;

   function Risk_Levels (File_Name : String) return Natural is
      Height_Map : Height_Map_Vector.Vector;
      Low_Points : Height_Vector.Vector;
   begin
      Read_Data (File_Name, Height_Map);
      
      for R in Height_Map.First_Index .. Height_Map.Last_Index loop
         for C in Height_Map.Element(R).First_Index ..
                  Height_Map.Element(R).Last_Index
         loop
            if Is_Low_Point (Height_Map, R, C) then
               Low_Points.Append (Height_Map.Element(R).Element(C));
            end if;
         end loop;
      end loop;
      
      declare
         Result : Natural := 0;
      begin
         for H of Low_Points loop
            Result := Result + Natural (H) + 1;
         end loop;
         return Result;
      end;
   end Risk_Levels;

begin
   if Argument_Count /= 1 then
      Put_Line ("Usage: ./day09 <inputfile>");
      return;
   end if;
   
   Put_Line ("Part 1:");
   Put_Line ("  Sum of the risk levels: " & Risk_Levels (Argument (1))'Img);
end Day09;

--  Part 1:
--    Sum of the risk levels:  600
