/**
 * C++ Build System Generator
 * Generates Makefiles for C++ projects
 */

/**
 * Generate C++ Makefile content
 */
function generateCppMakefile(projectName, tsiHeader) {
    return `${tsiHeader}

# TSI Header C++ Project Makefile
# Project: ${projectName}

# Compiler and flags
CXX = g++
CXXFLAGS = -Wall -Wextra -std=c++17 -Iinclude -g
LDFLAGS =

# Directories
SRCDIR = src
INCDIR = include
BUILDDIR = build
DOCSDIR = docs

# Source and object files
SOURCES = \$(wildcard \$(SRCDIR)/*.cpp)
OBJECTS = \$(SOURCES:\$(SRCDIR)/%.cpp=\$(BUILDDIR)/%.o)
TARGET = \$(BUILDDIR)/${projectName}

# Default target
.PHONY: all
all: \$(TARGET)

# Link object files to create executable
\$(TARGET): \$(OBJECTS) | \$(BUILDDIR)
	\$(CXX) \$(OBJECTS) -o \$(TARGET) \$(LDFLAGS)
	@echo "✅ Built \$(TARGET) successfully!"

# Compile source files to object files
\$(BUILDDIR)/%.o: \$(SRCDIR)/%.cpp | \$(BUILDDIR)
	\$(CXX) \$(CXXFLAGS) -c $< -o $@

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
debug: CXXFLAGS += -DDEBUG -O0
debug: \$(TARGET)

# Release build
.PHONY: release
release: CXXFLAGS += -DNDEBUG -O2
release: \$(TARGET)

# Install program
.PHONY: install
install: \$(TARGET)
	cp \$(TARGET) /usr/local/bin/
	@echo "📦 Installed \$(TARGET) to /usr/local/bin/"

# Show help
.PHONY: help
help:
	@echo "TSI Header C++ Project - Available targets:"
	@echo "  all       - Build the project (default)"
	@echo "  clean     - Remove build artifacts"
	@echo "  run       - Build and run the project"
	@echo "  debug     - Build with debug flags"
	@echo "  release   - Build with release optimizations"
	@echo "  install   - Install to system"
	@echo "  help      - Show this help message"
	@echo ""
	@echo "Project: ${projectName}"
	@echo "Compiler: \$(CXX) \$(CXXFLAGS)"
`;
}

module.exports = {
    generateCppMakefile
};