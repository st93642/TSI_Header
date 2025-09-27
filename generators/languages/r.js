/**
 * R Code Base Generator
 * Generates boilerplate R code
 */

function generateRCodeBase() {
    return `\n# Basic R program\n\n# Main function - entry point of the program\nmain <- function() {\n  cat("Hello, World!\\n")\n  cat("This is a basic R script.\\n")\n}\n\n# Execute main function\nmain()\n\n# Alternative direct approach\n# cat("Hello, World!\\n")\n# cat("This is a basic R script.\\n")\n\n# Example function with parameters\ngreet <- function(name = "World") {\n  return(paste("Hello,", name, "!"))\n}\n\n# Example usage\n# cat(greet("TSI Student"), "\\n")\n\n# Example with vectors and data frames\n# languages <- c("R", "Python", "JavaScript")\n# versions <- c(4.2, 3.9, 16)\n# \n# df <- data.frame(\n#   language = languages,\n#   version = versions\n# )\n# \n# print(df)\n# \n# # Plot example (uncomment to use)\n# # plot(df$language, df$version, \n# #      main = "Language Versions",\n# #      xlab = "Language", \n# #      ylab = "Version")\n\n# Example statistical operations\n# data <- rnorm(100, mean = 0, sd = 1)  # Generate random data\n# summary(data)  # Summary statistics\n# hist(data)     # Histogram\n`;
}

module.exports = {
    generateRCodeBase
};