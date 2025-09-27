/**
 * Ada Code Base Generator
 * Generates boilerplate code for Ada projects
 */

function generateAdaCodeBase() {
    return `-- Basic Ada program

with Ada.Text_IO; use Ada.Text_IO;

procedure Hello_World is
   -- Record type definition
   type TSI_Application is record
      Message : String (1..50) := "Hello, World!                    ";
      Version : String (1..10) := "1.0       ";
   end record;

   -- Application instance
   App : TSI_Application;

   -- Array example
   type Language_Array is array (1..3) of String (1..20);
   Languages : Language_Array := ("Ada                 ", "C                   ", "C++                 ");

begin
   -- Main program logic
   Put_Line (App.Message);
   Put_Line ("This is a basic Ada program.");
   Put_Line ("Version: " & App.Version);

   -- Loop through languages
   for I in Languages'Range loop
      Put_Line ("Language: " & Languages(I));
   end loop;

end Hello_World;`;
}

module.exports = {
    generateAdaCodeBase
};