/**
 * Diff Language Code Base Generator
 * Generates diff/patch code base/boilerplate code
 */

/**
 * Generates diff code base
 * @returns {string} Diff code base template
 */
function generateDiffCodeBase() {
    return `\n# Example diff/patch file
# This shows a typical unified diff format

# Index: example.txt
# ===================================================================
# --- example.txt	(revision 1)
# +++ example.txt	(revision 2)
# @@ -1,3 +1,4 @@
#  Line 1: Original content
#  Line 2: More original content
# +Line 3: Added content
#  Line 4: Final content
#
# This diff shows:
# - Original file with 3 lines
# - Modified file with 4 lines
# - One line added at line 3
`;
}

module.exports = {
    generateDiffCodeBase
};