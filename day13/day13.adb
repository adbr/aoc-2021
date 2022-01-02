-- adbr 2021-12-29

with Ada.Text_IO;            use Ada.Text_IO;
with Ada.Command_Line;       use Ada.Command_Line;
with Ada.Containers.Vectors; use Ada.Containers;
with Ada.Strings.Fixed;      use Ada.Strings.Fixed;

procedure Day13 is
   
   Value_Error : exception;
   
   type Dot_Type is
      record
         X : Natural;
         Y : Natural;
      end record;
   
   type Fold_Direction is (Up, Left);
   type Fold_Type is
      record
         Direction : Fold_Direction;
         Line      : Natural;
      end record;
   
   package Dots_Vector is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Dot_Type);
   
   package Folds_Vector is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Fold_Type);
   
   package Natural_IO is new Ada.Text_IO.Integer_IO
     (Num => Natural);
   use Natural_IO;
   
   procedure Print_Dots (Dots : Dots_Vector.Vector) is
      
      function Dot_Image (Dot : Dot_Type) return String is
      begin
         return "(" & Dot.X'Img & "," & Dot.Y'Img & ")";
      end Dot_Image;
      
   begin
      for D of Dots loop
         Put_Line (Dot_Image (D));
      end loop;
   end Print_Dots;
   
   procedure Print_Folds (Folds : Folds_Vector.Vector) is
      
      function Fold_Image (Fold : Fold_Type) return String is
      begin
         return "fold: " & Fold.Direction'Img & ":" & Fold.Line'Img;
      end Fold_Image;
      
   begin
      for F of Folds loop
         Put_Line (Fold_Image (F));
      end loop;
   end Print_Folds;

   procedure Read_Data
     (File_Name : String;
      Dots      : out Dots_Vector.Vector;
      Folds     : out Folds_Vector.Vector)
   is
      File   : File_Type;
      Dot    : Dot_Type;
      Fold   : Fold_Type;
      Sep    : String := " ";           -- for skip separators ',' and '='
      Prefix : String := "fold along "; -- for skip prefix
      XY     : Character;               -- folding direction
   begin
      Open (File, In_File, File_Name);
      
      -- Dots section
      while not End_Of_File (File) loop
         Get (File, Dot.X);
         Get (File, Sep);
         Get (File, Dot.Y);
         Skip_Line (File);
         Dots.Append (Dot);
         if End_Of_Line (File) then
            exit;
         end if;
      end loop;
      
      -- Folds section
      while not End_Of_File (File) loop
         Get (File, Prefix);
         Get (File, XY);
         case XY is
            when 'x' | 'X' =>
               Fold.Direction := Left;
            when 'y' | 'Y' =>
               Fold.Direction := Up;
            when others =>
               raise Value_Error with "Unknown direction: " & XY'Img &
                 " in line: " & Line(File)'Img;
         end case;
         Get (File, Sep);
         Get (File, Fold.Line);
         Folds.Append (Fold);
      end loop;
      
      Close (File);
   end Read_Data;
   
   procedure Make_Fold (Dots : in out Dots_Vector.Vector; Fold : Fold_Type) is
      Dots1 : Dots_Vector.Vector;
      D1    : Dot_Type;
   begin
      case Fold.Direction is
         when Up =>
            for D of Dots loop
               if D.Y < Fold.Line then
                  D1 := D;
               else
                  D1 := (D.X, Fold.Line - (D.Y - Fold.Line));
               end if;
               if not Dots1.Contains (D1) then
                  Dots1.Append (D1);
               end if;
            end loop;
            
         when Left =>
            for D of Dots loop
               if D.X < Fold.Line then
                  D1 := D;
               else
                  D1 := (Fold.Line - (D.X - Fold.Line), D.Y);
               end if;
               if not Dots1.Contains (D1) then
                  Dots1.Append (D1);
               end if;
            end loop;
      end case;
      
      Dots := Dots1;
   end Make_Fold;

   procedure Part_1 (File_Name : String) is
      Dots   : Dots_Vector.Vector;
      Folds  : Folds_Vector.Vector;
      Result : Natural := 0;
   begin
      Read_Data (File_Name, Dots, Folds);
      
      Make_Fold (Dots, Folds.Element (1));
      Result := Natural (Dots.Length);
      
      Put_Line ("Part 1:");
      Put_Line ("  Number of dots after first fold:" & Result'Img);
   end Part_1;

begin
   if Argument_Count /= 1 then
      Put_Line ("Usage: ./day13 <inputfile>");
      return;
   end if;
   
   Part_1 (Argument (1));
end Day13;
