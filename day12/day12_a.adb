-- adbr 2021-12-24
-- Graph implementation: vectors and indices

with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Command_Line;         use Ada.Command_Line;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;   use Ada.Containers;
with Ada.Characters.Handling;  use Ada.Characters.Handling;

procedure Day12_A is
   
   Value_Error : exception;
   
   subtype Node_Index is Positive;
   
   package Neighbor_Vector is new Ada.Containers.Vectors
     (Index_Type => Positive,
      Element_Type => Node_Index);
   
   type Node_Type is
      record
         Name : Unbounded_String;
         Visited : Boolean := False;
         Neighbors : Neighbor_Vector.Vector;
      end record;
   
   package Node_Vector is new Ada.Containers.Vectors
     (Index_Type => Node_Index,
      Element_Type => Node_Type);
   
   procedure Print_Node (Node : Node_Type) is
   begin
      Put_Line ("Name: " & To_String(Node.Name));
      Put_Line ("Num of neighbors:" & Node.Neighbors.Length'Img);
   end Print_Node;
   
   procedure Print_Nodes (Nodes : Node_Vector.Vector) is
   begin
      for I in Nodes.First_Index .. Nodes.Last_Index loop
         Put_Line ("Node index:" & I'Img);
         Put_Line ("Node name: " & To_String (Nodes.Element(I).Name));
         Put_Line ("Node neighbors:");
         for N of Nodes.Element(I).Neighbors loop
            Put (N'Img);
         end loop;
         New_Line;
      end loop;
   end Print_Nodes;
   
   function Is_Small (Node : Node_Type) return Boolean is
   begin
      for I in 1 .. Length (Node.Name) loop
         if Is_Upper (Element (Node.Name, I)) then
            return False;
         end if;
      end loop;
      return True;
   end Is_Small;

   procedure Read_Data (File_Name : String; Nodes : out Node_Vector.Vector) is
      File : File_Type;
      
      procedure Parse_Edge (Line : String; N1, N2 : out Node_Type) is
         Sep : constant Character := '-';
         Idx : Positive := Line'First;
      begin
         while Line(Idx) /= Sep loop
            N1.Name := N1.Name & Line (Idx);
            Idx := Idx + 1;
         end loop;
         
         Idx := Idx + 1; -- skip separator
         
         while Idx <= Line'Last loop
            N2.Name := N2.Name & Line (Idx);
            Idx := Idx + 1;
         end loop;
      end Parse_Edge;
      
      function Contains (Nodes : Node_Vector.Vector;
                         Node : Node_Type) return Boolean is
      begin
         for N of Nodes loop
            if N.Name = Node.Name then
               return True;
            end if;
         end loop;
         return False;
      end Contains;
      
      function Index (Nodes : Node_Vector.Vector;
                      Node : Node_Type) return Natural is
      begin
         for I in Nodes.First_Index .. Nodes.Last_Index loop
            if Nodes.Element(I).Name = Node.Name then
               return I;
            end if;
         end loop;
         return 0;
      end Index;
      
      procedure Add_Edge (Nodes : in out Node_Vector.Vector;
                          N1, N2 : Node_Type)
      is
         N1_Idx, N2_Idx : Node_Index;
      begin
         if Contains (Nodes, N1) then
            N1_Idx := Index (Nodes, N1);
         else
            Nodes.Append (N1);
            N1_Idx := Nodes.Last_Index;
         end if;
         
         if Contains (Nodes, N2) then
            N2_Idx := Index (Nodes, N2);
         else
            Nodes.Append (N2);
            N2_Idx := Nodes.Last_Index;
         end if;
         
         Nodes.Reference(N1_Idx).Neighbors.Append (N2_Idx);
         Nodes.Reference(N2_Idx).Neighbors.Append (N1_Idx);
      end Add_Edge;
      
   -- Start of processing for Read_Data

   begin
      Open (File, In_File, File_Name);
      while not End_Of_File (File) loop
         declare
            Line : String := Get_Line (File);
            N1, N2 : Node_Type;
         begin
            Parse_Edge (Line, N1, N2);
            Add_Edge (Nodes, N1, N2);
         end;
      end loop;
      Close (File);
   end Read_Data;

   procedure Part_1 (File_Name : String) is
      
      function Start_Node (Nodes : Node_Vector.Vector)
                          return Node_Index is
      begin
         for I in Nodes.First_Index .. Nodes.Last_Index loop
            if Nodes.Element(I).Name = "start" then
               return I;
            end if;
         end loop;
         raise Value_Error with "Missing 'start' node";
      end Start_Node;
      
      procedure Search_Paths (Nodes : in out Node_Vector.Vector;
                              Index : Node_Index;
                              Num_Paths : in out Natural) is
      begin
         if Nodes.Element(Index).Name = "end" then
            Num_Paths := Num_Paths + 1;
            return;
         end if;
         
         if Nodes.Element(Index).Visited then
            return;
         end if;
         
         if Is_Small (Nodes.Element(Index)) then
            Nodes.Reference(Index).Visited := True;
         end if;
         
         for I of Nodes.Element(Index).Neighbors loop
            if Nodes.Element(I).Name /= "start" then
               Search_Paths (Nodes, I, Num_Paths);
            end if;
         end loop;
         Nodes.Reference(Index).Visited := False;
      end Search_Paths;
      
      -- Local variables
      
      Nodes : Node_Vector.Vector;
      Num_Paths : Natural := 0;
      
   -- Start of processing for Part_1
      
   begin
      Read_Data (File_Name, Nodes);
      -- Print_Nodes (Nodes);
      Search_Paths (Nodes, Start_Node (Nodes), Num_Paths);
      
      Put_Line ("Part 1:");
      Put_Line ("  Number of paths:" & Num_Paths'Img);
   end Part_1;

begin
   if Argument_Count /= 1 then
      Put_Line ("Usage: ./day12 <inputfile>");
   end if;
   
   Part_1 (Argument (1));
end Day12_A;

--  Part 1:
--    Number of paths: 4912
