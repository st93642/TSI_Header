/**
 * Python Build System Generator
 * Generates Makefiles for Python projects
 */

let vscode;
try {
    vscode = require('vscode');
} catch (e) {
    // vscode not available (running outside VS Code)
    vscode = null;
}

/**
 * Generate Python Makefile content with TSI header
 */
async function generatePythonMakefileContent(projectName, tsiHeader) {
    return `${tsiHeader}
# Makefile for ${projectName} Python project

# Python settings
PYTHON := python3
PIP := pip3
VENV := venv
VENV_ACTIVATE := \$(VENV)/bin/activate

# Project settings
PACKAGE_NAME := ${projectName}
MAIN_FILE := src/main.py

.PHONY: help install run test clean venv format lint docs build

# Default target
help: ## Show this help message
	@echo "${projectName} - Python Project Makefile"
	@echo ""
	@echo "Available targets:"
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' \$(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "  %-15s %s\\n", \$\$1, \$\$2}'

venv: ## Create virtual environment
	@echo "Creating virtual environment..."
	@\$(PYTHON) -m venv \$(VENV)
	@echo "Virtual environment created. Run 'source \$(VENV_ACTIVATE)' to activate."

install: venv ## Install dependencies
	@echo "Installing dependencies..."
	@source \$(VENV_ACTIVATE) && \$(PIP) install -r requirements.txt
	@echo "Dependencies installed."

install-dev: install ## Install development dependencies
	@echo "Installing development dependencies..."
	@source \$(VENV_ACTIVATE) && \$(PIP) install -e .[dev]
	@echo "Development dependencies installed."

run: ## Run the main application
	@echo "Running ${projectName}..."
	@source \$(VENV_ACTIVATE) && \$(PYTHON) \$(MAIN_FILE)

test: ## Run tests
	@echo "Running tests..."
	@source \$(VENV_ACTIVATE) && python -m pytest tests/ -v

test-cov: ## Run tests with coverage
	@echo "Running tests with coverage..."
	@source \$(VENV_ACTIVATE) && python -m pytest tests/ --cov=src --cov-report=html

format: ## Format code with black
	@echo "Formatting code..."
	@source \$(VENV_ACTIVATE) && black src/ tests/

lint: ## Lint code with flake8
	@echo "Linting code..."
	@source \$(VENV_ACTIVATE) && flake8 src/ tests/

type-check: ## Type check with mypy
	@echo "Type checking..."
	@source \$(VENV_ACTIVATE) && mypy src/

docs: ## Generate documentation
	@echo "Generating documentation..."
	@source \$(VENV_ACTIVATE) && sphinx-build -b html docs/ docs/_build/html

build: ## Build the package
	@echo "Building package..."
	@source \$(VENV_ACTIVATE) && python setup.py sdist bdist_wheel

clean: ## Clean build artifacts
	@echo "Cleaning..."
	@rm -rf build/
	@rm -rf dist/
	@rm -rf *.egg-info/
	@rm -rf .coverage
	@rm -rf htmlcov/
	@find . -type d -name __pycache__ -exec rm -rf {} +
	@find . -type f -name "*.pyc" -delete
	@find . -type f -name "*.pyo" -delete

clean-venv: ## Remove virtual environment
	@echo "Removing virtual environment..."
	@rm -rf \$(VENV)

# Development workflow
dev: install-dev format lint test ## Run full development workflow

# CI/CD targets
ci: format lint test-cov ## Run CI checks

# Utility targets
freeze: ## Freeze current dependencies to requirements.txt
	@echo "Freezing dependencies..."
	@source \$(VENV_ACTIVATE) && \$(PIP) freeze > requirements.txt

update-deps: ## Update all dependencies
	@echo "Updating dependencies..."
	@source \$(VENV_ACTIVATE) && \$(PIP) install --upgrade -r requirements.txt

# Project info
info: ## Show project information
	@echo "Project: ${projectName}"
	@echo "Python: \$(\$(PYTHON) --version)"
	@echo "Virtual Environment: \$(VENV)"
	@echo "Main File: \$(MAIN_FILE)"
`;
}

/**
 * Fallback Python Makefile generation
 */
function generateFallbackPythonMakefile(projectName) {
    const now = new Date();
    const dateStr = now.toLocaleDateString('en-US', {
        month: 'short',
        day: '2-digit',
        year: 'numeric'
    }).replace(',', '');

    const timeStr = now.toLocaleTimeString('en-US', {
        hour: '2-digit',
        minute: '2-digit',
        hour12: false
    });

    // Get user info from settings
    const config = vscode.workspace.getConfiguration('tsiheader');
    const username = config.get('username') || 'TSI Student';
    const email = config.get('email') || 'student@tsi.lv';

    const dateTime = `${dateStr} ${timeStr}`;

    return `/*****************************************************************************/
/*                                                                           */
/*  Makefile                                              TTTTTTTT SSSSSSS II */
/*                                                          TT    SS      II */
/*  By: ${email.padEnd(50, ' ')} TT    SSSSSSS II */
/*                                                          TT         SS II */
/*  Created: ${dateTime} ${username.padEnd(30, ' ')} TT    SSSSSSS II */
/*  Updated: ${dateTime} ${username.padEnd(30, ' ')}                          */
/*                                                                           */
/*   Transport and Telecommunication Institute - Riga, Latvia                */
/*                       https://tsi.lv                                      */
/*****************************************************************************/

# Makefile for ${projectName} Python project

# Python settings
PYTHON := python3
PIP := pip3
VENV := venv
VENV_ACTIVATE := \$(VENV)/bin/activate

# Project settings
PACKAGE_NAME := ${projectName}
MAIN_FILE := src/main.py

.PHONY: help install run test clean venv format lint docs build

# Default target
help: ## Show this help message
	@echo "${projectName} - Python Project Makefile"
	@echo ""
	@echo "Available targets:"
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' \$(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "  %-15s %s\\n", \$\$1, \$\$2}'

venv: ## Create virtual environment
	@echo "Creating virtual environment..."
	@\$(PYTHON) -m venv \$(VENV)
	@echo "Virtual environment created. Run 'source \$(VENV_ACTIVATE)' to activate."

install: venv ## Install dependencies
	@echo "Installing dependencies..."
	@source \$(VENV_ACTIVATE) && \$(PIP) install -r requirements.txt
	@echo "Dependencies installed."

run: ## Run the main application
	@echo "Running ${projectName}..."
	@source \$(VENV_ACTIVATE) && \$(PYTHON) \$(MAIN_FILE)

test: ## Run tests
	@echo "Running tests..."
	@source \$(VENV_ACTIVATE) && python -m pytest tests/ -v

clean: ## Clean build artifacts
	@echo "Cleaning..."
	@rm -rf build/
	@rm -rf dist/
	@rm -rf *.egg-info/
	@find . -type d -name __pycache__ -exec rm -rf {} +
	@find . -type f -name "*.pyc" -delete

# Project info
info: ## Show project information
	@echo "Project: ${projectName}"
	@echo "Python: \$(\$(PYTHON) --version)"
	@echo "Virtual Environment: \$(VENV)"
	@echo "Main File: \$(MAIN_FILE)"
`;
}

module.exports = {
    generatePythonMakefileContent,
    generateFallbackPythonMakefile
};