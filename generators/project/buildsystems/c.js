/**
 * C Build System Generator
 * Generates Makefiles for C projects
 */

/**
 * Generate C Makefile content
 */
function generateCMakefile(projectName, tsiHeader) {
    return `${tsiHeader}

# TSI Header C Project Makefile
# Project: ${projectName}

# Compiler and flags
CC = gcc
CFLAGS = -Wall -Wextra -std=c99 -Iinclude -g
LDFLAGS =

# Directories
SRCDIR = src
INCDIR = include
BUILDDIR = build
DOCSDIR = docs

# Source and object files
SOURCES = \$(wildcard \$(SRCDIR)/*.c)
OBJECTS = \$(SOURCES:\$(SRCDIR)/%.c=\$(BUILDDIR)/%.o)
TARGET = \$(BUILDDIR)/${projectName}

# Default target
.PHONY: all
all: \$(TARGET)

# Link object files to create executable
\$(TARGET): \$(OBJECTS) | \$(BUILDDIR)
	\$(CC) \$(OBJECTS) -o \$(TARGET) \$(LDFLAGS)
	@echo "✅ Built \$(TARGET) successfully!"

# Compile source files to object files
\$(BUILDDIR)/%.o: \$(SRCDIR)/%.c | \$(BUILDDIR)
	\$(CC) \$(CFLAGS) -c $< -o $@

# Create build directory
\$(BUILDDIR):
	mkdir -p \$(BUILDDIR)

# Clean build artifacts
.PHONY: clean
clean:
	rm -rf \$(BUILDDIR)
	@echo "🧹 Cleaned build artifacts"

# Run the program
.PHONY: run
run: \$(TARGET)
	@echo "🚀 Running \$(TARGET)..."
	@./\$(TARGET)

# Debug build
.PHONY: debug
debug: CFLAGS += -DDEBUG -O0
debug: \$(TARGET)

# Release build
.PHONY: release
release: CFLAGS += -DNDEBUG -O2
release: \$(TARGET)

# Install program
.PHONY: install
install: \$(TARGET)
	cp \$(TARGET) /usr/local/bin/
	@echo "📦 Installed \$(TARGET) to /usr/local/bin/"

# Show help
.PHONY: help
help:
	@echo "TSI Header C Project - Available targets:"
	@echo "  all       - Build the project (default)"
	@echo "  clean     - Remove build artifacts"
	@echo "  run       - Build and run the project"
	@echo "  debug     - Build with debug flags"
	@echo "  release   - Build with release optimizations"
	@echo "  install   - Install to system"
	@echo "  help      - Show this help message"
	@echo ""
	@echo "Project: ${projectName}"
	@echo "Compiler: \$(CC) \$(CFLAGS)"
`;
}

module.exports = {
    generateCMakefile
};