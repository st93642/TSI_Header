/**
 * Awk Code Base Generator
 * Generates boilerplate code for Awk scripts
 */

function generateAwkCodeBase() {
    return `#!/usr/bin/awk -f

# Basic Awk program

BEGIN {
    print "Hello, World!"
    print "This is a basic Awk script."

    # Array example
    languages["lang1"] = "Awk"
    languages["lang2"] = "Sed"
    languages["lang3"] = "Grep"

    print "Languages:"
    for (key in languages) {
        print "  " languages[key]
    }

    # Process input files (if any)
    if (ARGC > 1) {
        print "Processing input files..."
    }
}

# Pattern-action rules
/^#/ {
    # Skip comments
    next
}

/.*/ {
    # Process lines
    if (length($0) > 0) {
        print "Processing line: " $0
    }
}

END {
    print "Script execution completed."
}`;
}

module.exports = {
    generateAwkCodeBase
};