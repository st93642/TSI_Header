/**
 * APL Code Base Generator
 * Generates boilerplate code for APL projects
 */

function generateAplCodeBase() {
    return `⍝ Basic APL program

∇Hello_World
  ⍝ Main function
  ⎕←'Hello, World!'
  ⎕←'This is a basic APL program.'

  ⍝ Example with arrays
  languages ← 'APL' 'J' 'K'
  ⎕←'Languages:' languages

  ⍝ Mathematical operations
  numbers ← 1 2 3 4 5
  doubled ← numbers × 2
  ⎕←'Numbers:' numbers
  ⎕←'Doubled:' doubled
∇

⍝ Execute main function
Hello_World`;
}

module.exports = {
    generateAplCodeBase
};