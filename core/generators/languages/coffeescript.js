/**
 * CoffeeScript Code Base Generator
 * Generates boilerplate code for CoffeeScript projects
 */

function generateCoffeeScriptCodeBase() {
    return `# Basic CoffeeScript program

# Class definition
class TSIApplication
  # Constructor
  constructor: ->
    @message = "Hello, World!"
    @version = "1.0"

  # Main execution method
  run: ->
    console.log @message
    console.log "This is a basic CoffeeScript program."
    console.log "Version: #{@version}"

    # Array example
    languages = ["CoffeeScript", "JavaScript", "TypeScript"]
    console.log "Languages:"
    console.log "  #{lang}" for lang in languages

    # Object example
    config =
      debug: true
      port: 3000
      features: ["logging", "caching"]

    console.log "Configuration:"
    console.log "  Debug: #{config.debug}"
    console.log "  Port: #{config.port}"

  # Utility method
  getInfo: ->
    "TSI Application v#{@version}"

  # Class method
  @createDefault: ->
    app = new TSIApplication()
    app.message = "Welcome to TSI!"
    app

# Main execution
app = new TSIApplication()
app.run()

# Example usage
defaultApp = TSIApplication.createDefault()
console.log defaultApp.getInfo()`;
}

module.exports = {
    generateCoffeeScriptCodeBase
};