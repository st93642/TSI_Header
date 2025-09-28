/**
 * Verse Code Base Generator
 * Generates boilerplate Verse code
 */

function generateVerseCodeBase() {
    return `\n// Basic Verse code\n// Note: Verse is Epic Games' programming language for Fortnite/UEFN\n\nusing { /Fortnite.com/Devices }\nusing { /Verse.org/Simulation }\nusing { /UnrealEngine.com/Temporary/Diagnostics }\n\n# Main device class\nhello_world_device := class(creative_device):\n    \n    # Main function - entry point\n    OnBegin<override>()<suspends>: void =\n        Print("Hello, World!")\n        Print("This is a basic Verse program.")\n        \n    # Example function with parameters\n    Greet(Name: string): string =\n        "Hello, {Name}!"\n    \n    # Example async function\n    WaitAndGreet<public>(Name: string, Seconds: float)<suspends>: void =\n        Sleep(Seconds)\n        Print(Greet(Name))\n`;
}

module.exports = {
    generateVerseCodeBase
};