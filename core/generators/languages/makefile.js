/**
 * Makefile Code Base Generator
 * Generates boilerplate Makefile code
 */

function generateMakefileCodeBase() {
    return `\n# Basic Makefile template\n\n# Compiler and flags\nCC = gcc\nCFLAGS = -Wall -Wextra -std=c99 -O2\nLDFLAGS = \n\n# Target executable\nTARGET = hello_world\n\n# Source files\nSRCS = main.c utils.c\n\n# Object files\nOBJS = $(SRCS:.c=.o)\n\n# Default target\nall: $(TARGET)\n\n# Link object files to create executable\n$(TARGET): $(OBJS)\n\t$(CC) $(LDFLAGS) -o $@ $^ \n\n# Compile source files to object files\n%.o: %.c\n\t$(CC) $(CFLAGS) -c $< -o $@\n\n# Clean build artifacts\nclean:\n\trm -f $(OBJS) $(TARGET)\n\n# Run the program\nrun: $(TARGET)\n\t./$(TARGET)\n\n# Debug build\ndebug: CFLAGS += -g -DDEBUG\ndebug: clean all\n\n# Install (example)\ninstall: $(TARGET)\n\tinstall -m 755 $(TARGET) /usr/local/bin/\n\n# Uninstall\nuninstall:\n\trm -f /usr/local/bin/$(TARGET)\n\n# Help target\nhelp:\n\t@echo "Available targets:"\n\t@echo "  all      - Build the program (default)"\n\t@echo "  clean    - Remove build artifacts"\n\t@echo "  run      - Build and run the program"\n\t@echo "  debug    - Build with debug symbols"\n\t@echo "  install  - Install the program"\n\t@echo "  help     - Show this help"\n\n.PHONY: all clean run debug install uninstall help\n`;
}

module.exports = {
    generateMakefileCodeBase
};