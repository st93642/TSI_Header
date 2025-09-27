/**
 * OCaml Code Base Generator
 * Generates boilerplate code for OCaml projects
 */

function generateOcamlCodeBase() {
    return `\n(* Basic OCaml program *)\n\n(* Main function - entry point of the program *)\nlet main () =\n  print_endline "Hello, World!";\n  print_endline "This is a basic OCaml program."\n\n(* Execute main function *)\nlet () = main ()\n\n(* Alternative direct approach *)\n(* let () = *)\n(*   print_endline "Hello, World!"; *)\n(*   print_endline "This is a basic OCaml program." *)\n\n(* Example function with pattern matching *)\n(* let rec factorial n = *)\n(*   match n with *)\n(*   | 0 | 1 -> 1 *)\n(*   | n -> n * factorial (n - 1) *)\n\n(* Example usage *)\n(* let result = factorial 5 in *)\n(* Printf.printf "5! = %d\\n" result *)\n\n(* Example with lists *)\n(* let numbers = [1; 2; 3; 4; 5] in *)\n(* let doubled = List.map (fun x -> x * 2) numbers in *)\n(* List.iter (Printf.printf "%d ") doubled *)\n`;
}

module.exports = {
    generateOcamlCodeBase
};