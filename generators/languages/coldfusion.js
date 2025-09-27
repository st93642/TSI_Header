/**
 * ColdFusion Code Base Generator
 * Generates boilerplate code for ColdFusion projects
 */

function generateColdFusionCodeBase() {
    return `<cfscript>
// Basic ColdFusion program

// Main component
component name="TSIApplication" {

    // Properties
    property name="message" type="string" default="Hello, World!";
    property name="version" type="string" default="1.0";

    // Constructor
    public function init() {
        this.message = "Hello, World!";
        this.version = "1.0";
        return this;
    }

    // Main execution method
    public function run() {
        writeOutput(this.message & "<br>");
        writeOutput("This is a basic ColdFusion program.<br>");
        writeOutput("Version: " & this.version & "<br>");

        // Array example
        var languages = ["ColdFusion", "Java", "SQL"];
        writeOutput("<br>Languages:<br>");
        for (var lang in languages) {
            writeOutput("  " & lang & "<br>");
        }

        return this;
    }

    // Utility method
    public string function getInfo() {
        return "TSI Application v" & this.version;
    }
}

// Create and run application
application = new TSIApplication().init();
application.run();
</cfscript>`;
}

module.exports = {
    generateColdFusionCodeBase
};