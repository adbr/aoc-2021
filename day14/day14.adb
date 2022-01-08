-- adbr 2022-01-03

with Ada.Text_IO;                use Ada.Text_IO;
with Ada.Command_Line;           use Ada.Command_Line;
with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with Ada.Containers.Hashed_Maps; use Ada.Containers;
with Ada.Containers.Vectors;

procedure Day14 is
   
   subtype Element_Type is Character range 'A' .. 'Z';
   type Pair_Type is array (1 .. 2) of Element_Type;
   subtype Counter_Type is Long_Integer range 0 .. Long_Integer'Last;
   
   -- Polymers_Vectors
   
   package Polymers_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Element_Type);
   
   subtype Polymer_Type is Polymers_Vectors.Vector;
   
   -- Rules_Maps
   
   function Hash (Key : Pair_Type) return Hash_Type is
   begin
      return (Character'Pos (Key (1)) + Character'Pos (Key (2))) *
        Character'Pos (Key (1));
   end Hash;
   
   function Equivalent_Keys (Left, Right : Pair_Type) return Boolean is
   begin
      return Left (1) = Right (1) and Left (2) = Right (2);
   end Equivalent_Keys;

   package Rules_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Pair_Type,
      Element_Type    => Element_Type,
      Hash            => Hash,
      Equivalent_Keys => Equivalent_Keys);
   
   subtype Rules_Type is Rules_Maps.Map;
   
   -- Counters_Maps
   
   function Hash (Key : Element_Type) return Hash_Type is
   begin
      return Character'Pos (Key);
   end Hash;
   
   function Equivalent_Keys (Left, Right : Element_Type) return Boolean is
   begin
      return Left = Right;
   end Equivalent_Keys;
   
   package Counters_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type     => Element_Type,
      Element_Type => Counter_Type,
      Hash => Hash,
      Equivalent_Keys => Equivalent_Keys);
   
   subtype Counters_Type is Counters_Maps.Map;
   
   -- Counters_Cache_Maps
   
   type Counters_Cache_Key is
      record
         Pair : Pair_Type;
         Step : Natural;
      end record;
   
   function Hash (Key : Counters_Cache_Key) return Hash_Type is
   begin
      return Hash_Type ((Character'Pos (Key.Pair (1)) +
                           Character'Pos (Key.Pair (2))) *
                          Character'Pos (Key.Pair (1)) *
                          Key.Step);
   end Hash;
   
   function Equivalent_Keys (Left, Right : Counters_Cache_Key)
                            return Boolean is
   begin
      return Left = Right;
   end Equivalent_Keys;

   package Counters_Cache_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Counters_Cache_Key,
      Element_Type    => Counters_Type,
      Hash            => Hash,
      Equivalent_Keys => Equivalent_Keys,
      "="             => Counters_Maps."=");
   
   Counters_Cache : Counters_Cache_Maps.Map;
   
   -- Subprograms
   
   procedure Print_Polymer (Polymer : Polymer_Type) is
   begin
      for E of Polymer loop
         Put (E);
      end loop;
      New_Line;
   end Print_Polymer;
   
   procedure Print_Rules (Rules : Rules_Type) is
   begin
      declare
         use Rules_Maps;
         C : Cursor := Rules.First;
         K : Pair_Type;
         E : Element_Type;
      begin
         while C /= No_Element loop
            K := Key (C);
            E := Element (C);
            Put (K (1));
            Put (K (2));
            Put (" => ");
            Put (E);
            New_Line;
            Next (C);
         end loop;
      end;
   end Print_Rules;
   
   procedure Read_Data
     (File_Name : String;
      Polymer   : out Polymer_Type;
      Rules     : out Rules_Type)
   is
      
      function To_Polymer (S : String) return Polymer_Type
      is
         Result : Polymer_Type;
      begin
         for I in S'Range loop
            Result.Append (S (I));
         end loop;
         return Result;
      end To_Polymer;
      
      procedure Parse_Rule
        (Line    : String;
         Pair    : out Pair_Type;
         Element : out Element_Type) is
      begin
         Pair (1) := Line (Line'First);
         Pair (2) := Line (Line'First + 1);
         Element  := Line (Line'First + 6);
      end Parse_Rule;
      
      -- Local variables
      
      File : File_Type;
      
      -- Start of processing for Read_Data
      
   begin
      Open (File, In_File, File_Name);
      
      Polymer := To_Polymer (Get_Line (File));
      Skip_Line (File);
      
      while not End_Of_File (File) loop
         declare
            Line    : String := Get_Line (File);
            Pair    : Pair_Type;
            Element : Element_Type;
         begin
            Parse_Rule (Line, Pair, Element);
            Rules.Insert (Pair, Element);
         end;
      end loop;
      
      Close (File);
   end Read_Data;
   
   procedure Increment (Counters : in out Counters_Type; E : Element_Type) is
   begin
      if Counters.Contains (E) then
         Counters.Reference (E) := Counters.Reference (E) + 1;
      else
         Counters.Insert (E, 1);
      end if;
   end Increment;
   
   procedure Find_Min_Max (Counters : Counters_Type;
                           Min, Max : out Counter_Type) is
   begin
      declare
         use Counters_Maps;
         C     : Cursor  := Counters.First;
         First : Boolean := True;
      begin
         while C /= No_Element loop
            if First then
               Min := Element (C);
               Max := Element (C);
               First := False;
            else
               if Element (C) < Min then
                  Min := Element (C);
               end if;
               if Element (C) > Max then
                  Max := Element (C);
               end if;
            end if;
            Next (C);
         end loop;
      end;
   end Find_Min_Max ;
   
   procedure Merge (Target : in out Counters_Type;
                    Source : Counters_Type) is
   begin
      declare
         use Counters_Maps;
         C : Cursor := Source.First;
      begin
         while C /= No_Element loop
            declare
               K : Element_Type := Key (C);
               V : Counter_Type := Element (C);
            begin
               if Target.Contains (K) then
                  Target.Reference (K) := Target.Reference (K) + V;
               else
                  Target.Insert (K, V);
               end if;
            end;
            Next (C);
         end loop;
      end;
   end Merge;
   
   function Count_Inserted_Elements
     (Pair  : Pair_Type;
      Step  : Natural;
      Rules : Rules_Type) return Counters_Type
   is
      Counters : Counters_Type;
   begin
      if Step = 0 then
         return Counters;
      end if;
      
      if not Rules.Contains (Pair) then
         return Counters;
      end if;
      
      declare
         Key : Counters_Cache_Key := (Pair, Step);
      begin
         if Counters_Cache.Contains (Key) then
            return Counters_Cache.Element (Key);
         end if;
      end;
      
      declare
         E : Element_Type := Rules.Element (Pair);
         Counters1 : Counters_Type;
      begin
         Increment (Counters, E);
         Counters1 := Count_Inserted_Elements (Pair_Type'(Pair (1), E),
                                               Step - 1,
                                               Rules);
         Merge (Counters, Counters1);
         Counters1 := Count_Inserted_Elements (Pair_Type'(E, Pair (2)),
                                               Step - 1,
                                               Rules);
         Merge (Counters, Counters1);
      end;
      
      Counters_Cache.Insert ((Pair, Step), Counters);
      return Counters;
   end Count_Inserted_Elements;
   
   procedure Compute_Insertions
     (Polymer :     Polymer_Type;
      Rules   :     Rules_Type;
      Steps   :     Natural;
      Result  : out Counter_Type)
   is
      Counters : Counters_Type;
   begin
      for I in Polymer.First_Index .. Polymer.Last_Index - 1 loop
         declare
            E1   : Element_Type := Polymer (I);
            E2   : Element_Type := Polymer (I + 1);
            Counters1 : Counters_Type := 
              Count_Inserted_Elements (Pair_Type'(E1, E2), Steps, Rules);
         begin
            Merge (Counters, Counters1);
            Increment (Counters, E1);
         end;
      end loop;
      Increment (Counters, Polymer.Last_Element);
      
      declare
         Min, Max : Counter_Type;
      begin
         Find_Min_Max (Counters, Min, Max);
         Result := Max - Min;
      end;
   end Compute_Insertions;

   procedure Part_1 (File_Name : String) is
      Polymer : Polymer_Type;
      Rules   : Rules_Type;
      Result  : Counter_Type;
      Steps   : constant Positive := 10;
   begin
      Read_Data (File_Name, Polymer, Rules);
      Compute_Insertions (Polymer, Rules, Steps, Result);
      
      Put_Line ("Part 1:");
      Put_Line ("  Steps:" & Steps'Img & ", Max - Min =" & Result'Img);
   end Part_1;

   procedure Part_2 (File_Name : String) is
      Polymer : Polymer_Type;
      Rules   : Rules_Type;
      Result  : Counter_Type;
      Steps   : constant Positive := 40;
   begin
      Read_Data (File_Name, Polymer, Rules);
      Compute_Insertions (Polymer, Rules, Steps, Result);
      
      Put_Line ("Part 2:");
      Put_Line ("  Steps:" & Steps'Img & ", Max - Min =" & Result'Img);
   end Part_2;
   
begin
   if Argument_Count /= 1 then
      Put_Line ("Usage: ./day14 <inputfile>");
      return;
   end if;
   
   Part_1 (Argument (1));
   Part_2 (Argument (1));
end Day14;

--  Part 1:
--    Steps: 10, Max - Min = 3095
--  Part 2:
--    Steps: 40, Max - Min = 3152788426516
