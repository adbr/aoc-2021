-- adbr 2021-12-05

--  Part 1:
--    Overlaped points: 7414
--  Part 2:
--    Overlaped points: 19676

with Ada.Text_IO;       use Ada.Text_IO;
with Ada.Command_Line;  use Ada.Command_Line;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Containers.Vectors;
with Ada.Containers.Hashed_Maps;

procedure Day05 is
   
   type Number is new Natural;
   type Point_Type is
      record
         X, Y : Number;
      end record;
   type Line_Type is
      record
         P1, P2 : Point_Type;
      end record;
   
   package Line_Vector is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Line_Type);
   
   function Hash (P : Point_Type) return Ada.Containers.Hash_Type is
   begin
      return Ada.Containers.Hash_Type (P.X * P.Y);
   end Hash;
   
   function Equivalent_Keys (P1, P2 : Point_Type) return Boolean is
   begin
      return P1.X = P2.X and P1.Y = P2.Y;
   end Equivalent_Keys;
   
   package Point_Map is new Ada.Containers.Hashed_Maps
     (Key_Type        => Point_Type,
      Element_Type    => Natural,
      Hash            => Hash,
      Equivalent_Keys => Equivalent_Keys);
   
   package Number_IO is new Ada.Text_IO.Integer_IO (Number);
   use Number_IO;
   
   ----------------
   -- Line_Image --
   ----------------
   
   function Line_Image (Line : Line_Type) return String is
      X1 : constant String := Trim (Line.P1.X'Img, Ada.Strings.Left);
      Y1 : constant String := Trim (Line.P1.Y'Img, Ada.Strings.Left);
      X2 : constant String := Trim (Line.P2.X'Img, Ada.Strings.Left);
      Y2 : constant String := Trim (Line.P2.Y'Img, Ada.Strings.Left);
   begin
      return "((" & X1 & ", " & Y1 & "), (" & X2 & ", " & Y2 & "))";
   end Line_Image;
   
   -----------------
   -- Point_Image --
   -----------------
   
   function Point_Image (Point : Point_Type) return String is
      X : constant String := Trim (Point.X'Img, Ada.Strings.Left);
      Y : constant String := Trim (Point.Y'Img, Ada.Strings.Left);
   begin
      return "(" & X & ", " & Y & ")";
   end Point_Image;
   
   ---------------
   -- Read_Data --
   ---------------
   
   procedure Read_Data (File_Name : String;
                        Lines     : out Line_Vector.Vector) is
      
      --------------
      -- Get_Line --
      --------------
      
      procedure Get_Line (File : File_Type; Line : out Line_Type) is
         X1, Y1, X2, Y2 : Number;
         Num_Separator  : String := ",";    -- for skip ','
         Line_Separator : String := " -> "; -- for skip ' -> '
      begin
         Get (File, X1);
         Get (File, Num_Separator);
         Get (File, Y1);
         Get (File, Line_Separator);
         Get (File, X2);
         Get (File, Num_Separator);
         Get (File, Y2);
         Line := ((X1, Y1), (X2, Y2));
      end Get_Line;
      
   --  Start of processing for Read_Data
      
      File : File_Type;
      Line : Line_Type;
   begin
      Open (File, In_File, File_Name);
      while not End_Of_File (File) loop
         Get_Line (File, Line);
         Lines.Append (Line);
      end loop;
      Close (File);
   end Read_Data;
   
   -------------------------
   -- Overlaped_Points_HV --
   -------------------------
   
   function Overlaped_Points_HV (File_Name : String) return Integer is
      -- Return overlaped points in horizontal and vertical lines
      
      ----------------------------
      -- Is_Horizontal_Vertical --
      ----------------------------
      
      function Is_Horizontal_Vertical (Line : Line_Type) return Boolean is
      begin
         return (Line.P1.X = Line.P2.X) or (Line.P1.Y = Line.P2.Y);
      end Is_Horizontal_Vertical;
      
      --------------------------
      -- Print_Points_Of_Line --
      --------------------------
      
      procedure Print_Points_Of_Line (Line : Line_Type) is
         
         function Min (A, B : Number) return Number is
         begin
            return (if A < B then A else B);
         end Min;
         
         function Max (A, B : Number) return Number is
         begin
            return (if A > B then A else B);
         end Max;
         
         --  Start of processing for Print_Points_Of_Line
         
         P      : Point_Type;
         X1, X2 : Number;
         Y1, Y2 : Number;
      begin
         Put_Line ("** " & Line_Image (Line));
         
         if Line.P1.X = Line.P2.X then
            Y1 := Min (Line.P1.Y, Line.P2.Y);
            Y2 := Max (Line.P1.Y, Line.P2.Y);
            for Y in Y1 .. Y2 loop
               P := (Line.P1.X, Y);
               Put_Line (Point_Image (P));
            end loop;
         end if;
         
         if Line.P1.Y = Line.P2.Y then
            X1 := Min (Line.P1.X, Line.P2.X);
            X2 := Max (Line.P1.X, Line.P2.X);
            for X in X1 .. X2 loop
               P := (X, Line.P1.Y);
               Put_Line (Point_Image (P));
            end loop;
         end if;
      end Print_Points_Of_Line;
      
      ---------------------------
      -- Insert_Points_Of_Line --
      ---------------------------
      
      procedure Insert_Points_Of_Line (Points : in out Point_Map.Map;
                                       Line   : Line_Type) is
         
         function Min (A, B : Number) return Number is
         begin
            return (if A < B then A else B);
         end Min;
         
         function Max (A, B : Number) return Number is
         begin
            return (if A > B then A else B);
         end Max;
         
         ------------------
         -- Insert_Point --
         ------------------
         
         procedure Insert_Point (Points : in out Point_Map.Map;
                                 Point  : Point_Type) is
         begin
            if Points.Contains (Point) then
               declare
                  E : Natural := Points.Element (Point);
               begin
                  Points.Replace (Point, E + 1);
               end;
            else
               Points.Insert (Point, 1);
            end if;
         end Insert_Point;
         
      --  Start of processing for Insert_Points_Of_Line
         
         P      : Point_Type;
         X1, X2 : Number;
         Y1, Y2 : Number;
      begin
         if Line.P1.X = Line.P2.X then
            Y1 := Min (Line.P1.Y, Line.P2.Y);
            Y2 := Max (Line.P1.Y, Line.P2.Y);
            for Y in Y1 .. Y2 loop
               P := (Line.P1.X, Y);
               Insert_Point (Points, P);
            end loop;
         end if;
         
         if Line.P1.Y = Line.P2.Y then
            X1 := Min (Line.P1.X, Line.P2.X);
            X2 := Max (Line.P1.X, Line.P2.X);
            for X in X1 .. X2 loop
               P := (X, Line.P1.Y);
               Insert_Point (Points, P);
            end loop;
         end if;
      end Insert_Points_Of_Line;
      
   -- Start of processing for Overlaped_Points_HV
      
      Lines     : Line_Vector.Vector;
      Points    : Point_Map.Map;
      Overlaped : Natural := 0;
   begin
      Read_Data (File_Name, Lines);
      
      for Line of Lines loop
         if Is_Horizontal_Vertical (Line) then
            -- Print_Points_Of_Line (Line);
            Insert_Points_Of_Line (Points, Line);
         end if;
      end loop;
      
      declare
         use Point_Map;
         C : Point_Map.Cursor;
      begin
         C := Points.First;
         while C /= No_Element loop
            if Element(C) >= 2 then
               Overlaped := Overlaped + 1;
            end if;
            Next (C);
         end loop;
      end;
      
      return Overlaped;
   end Overlaped_Points_HV;
   
   -------------------------------
   -- Overlaped_Points_Diagonal --
   -------------------------------
   
   function Overlaped_Points_Diagonal (File_Name : String) return Integer is
      -- Return overlaped points in horizontal, vertical and diagonal lines
      
      ---------------------------
      -- Insert_Points_Of_Line --
      ---------------------------
      
      procedure Insert_Points_Of_Line (Points : in out Point_Map.Map;
                                       Line   : Line_Type) is
         
         function Min (A, B : Number) return Number is
         begin
            return (if A < B then A else B);
         end Min;
         
         function Max (A, B : Number) return Number is
         begin
            return (if A > B then A else B);
         end Max;
         
         ------------------
         -- Insert_Point --
         ------------------
         
         procedure Insert_Point (Points : in out Point_Map.Map;
                                 Point  : Point_Type) is
         begin
            if Points.Contains (Point) then
               declare
                  E : Natural := Points.Element (Point);
               begin
                  Points.Replace (Point, E + 1);
               end;
            else
               Points.Insert (Point, 1);
            end if;
         end Insert_Point;
         
      --  Start of processing for Insert_Points_Of_Line
         
         P      : Point_Type;
         X1, X2 : Integer;
         Y1, Y2 : Integer;
      begin
         --  TODO: add operations on points: increment, decrement
         --  point: (1,1), (1,-1), ...
         
         --  Vertical line
         
         if Line.P1.X = Line.P2.X then
            Y1 := Integer (Min (Line.P1.Y, Line.P2.Y));
            Y2 := Integer (Max (Line.P1.Y, Line.P2.Y));
            for Y in Y1 .. Y2 loop
               P := (Line.P1.X, Number (Y));
               Insert_Point (Points, P);
            end loop;
         end if;
         
         --  Horizontal line
         
         if Line.P1.Y = Line.P2.Y then
            X1 := Integer (Min (Line.P1.X, Line.P2.X));
            X2 := Integer (Max (Line.P1.X, Line.P2.X));
            for X in X1 .. X2 loop
               P := (Number (X), Line.P1.Y);
               Insert_Point (Points, P);
            end loop;
         end if;
         
         --  Diagonal right-up
         
         if Line.P1.X < Line.P2.X and Line.P1.Y < Line.P2.Y then
            X1 := Integer (Line.P1.X);
            Y1 := Integer (Line.P1.Y);
            X2 := Integer (Line.P2.X);
            Y2 := Integer (Line.P2.Y);
            while X1 <= X2 and Y1 <= Y2 loop
               P := (Number (X1), Number (Y1));
               Insert_Point (Points, P);
               X1 := X1 + 1;
               Y1 := Y1 + 1;
            end loop;
         end if;
         
         --  Diagonal left-down
         
         if Line.P1.X > Line.P2.X and Line.P1.Y > Line.P2.Y then
            X1 := Integer (Line.P1.X);
            Y1 := Integer (Line.P1.Y);
            X2 := Integer (Line.P2.X);
            Y2 := Integer (Line.P2.Y);
            while X1 >= X2 and Y1 >= Y2 loop
               P := (Number (X1), Number (Y1));
               Insert_Point (Points, P);
               X1 := X1 - 1;
               Y1 := Y1 - 1;
            end loop;
         end if;
         
         --  Diagonal left-up
         
         if Line.P1.X > Line.P2.X and Line.P1.Y < Line.P2.Y then
            X1 := Integer (Line.P1.X);
            Y1 := Integer (Line.P1.Y);
            X2 := Integer (Line.P2.X);
            Y2 := Integer (Line.P2.Y);
            while X1 >= X2 and Y1 <= Y2 loop
               P := (Number (X1), Number (Y1));
               Insert_Point (Points, P);
               X1 := X1 - 1;
               Y1 := Y1 + 1;
            end loop;
         end if;
         
         --  Diagonal right-down
         
         if Line.P1.X < Line.P2.X and Line.P1.Y > Line.P2.Y then
            X1 := Integer (Line.P1.X);
            Y1 := Integer (Line.P1.Y);
            X2 := Integer (Line.P2.X);
            Y2 := Integer (Line.P2.Y);
            while X1 <= X2 and Y1 >= Y2 loop
               P := (Number (X1), Number (Y1));
               Insert_Point (Points, P);
               X1 := X1 + 1;
               Y1 := Y1 - 1;
            end loop;
         end if;
         
      end Insert_Points_Of_Line;
      
   -- Start of processing for Overlaped_Points_Diagonal
      
      Lines     : Line_Vector.Vector;
      Points    : Point_Map.Map;
      Overlaped : Natural := 0;
   begin
      Read_Data (File_Name, Lines);
      
      for Line of Lines loop
         Insert_Points_Of_Line (Points, Line);
      end loop;
      
      declare
         use Point_Map;
         C : Point_Map.Cursor;
      begin
         C := Points.First;
         while C /= No_Element loop
            if Element(C) >= 2 then
               Overlaped := Overlaped + 1;
            end if;
            Next (C);
         end loop;
      end;
      
      return Overlaped;
   end Overlaped_Points_Diagonal;
   
begin
   if Argument_Count /= 1 then
      Put_Line ("Usage: ./day05 <filename>");
      Set_Exit_Status (Failure);
      return;
   end if;

   Put_Line ("Part 1:");
   Put_Line ("  Overlaped points:" &
               Overlaped_Points_HV (Argument (1))'Img);

   Put_Line ("Part 2:");
   Put_Line ("  Overlaped points:" &
               Overlaped_Points_Diagonal (Argument (1))'Img);
end Day05;
