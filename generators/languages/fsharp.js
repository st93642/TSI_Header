/**
 * F# Code Base Generator
 * Generates boilerplate code for F# projects
 */

function generateFSharpCodeBase() {
    return `// Basic F# program

// Main module
module TSIApplication

// Types
type Config = {
    Debug: bool
    Port: int
    Features: string list
}

// Main function - entry point
[<EntryPoint>]
let main argv =
    printfn "Hello, World!"
    printfn "This is a basic F# program."

    // Run application
    runApp ()
    0 // return exit code

// Application logic
and runApp () =
    let version = "1.0"
    printfn "Version: %s" version

    // Example with lists
    let languages = ["F#"; "C#"; "VB.NET"]
    printfn "Languages:"
    languages |> List.iter (fun lang -> printfn "  %s" lang)

    // Example with records
    let config = {
        Debug = true
        Port = 8080
        Features = ["logging"; "caching"; "auth"]
    }

    printfn "Configuration:"
    printfn "  Debug: %b" config.Debug
    printfn "  Port: %d" config.Port

// Utility functions
let greet name =
    sprintf "Hello, %s!" name

let getInfo () =
    ("TSI Application", "1.0")

// Example usage (uncomment to test)
// printfn "%s" (greet "TSI Student")
// let (name, version) = getInfo ()
// printfn "%s v%s" name version`;
}

module.exports = {
    generateFSharpCodeBase
};