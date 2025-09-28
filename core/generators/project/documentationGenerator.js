/**
 * Documentation Generator
 * Creates README.md and project documentation
 */

let vscode;
try {
    vscode = require('vscode');
} catch (e) {
    // vscode not available (running outside VS Code)
    vscode = null;
}

/**
 * Create documentation files for the project
 */
async function createDocumentationFiles(language, projectName, projectUri, vscodeInstance) {
    // Use provided vscode instance or global vscode
    const vscode = vscodeInstance || (typeof vscode !== 'undefined' ? vscode : null);
    if (!vscode) {
        throw new Error('VS Code API not available');
    }

    await createReadmeFile(language, projectName, projectUri, vscode);
    // Future: Create additional docs like CONTRIBUTING.md, API docs, etc.
}

/**
 * Create README.md file with TSI branding and project information
 */
async function createReadmeFile(language, projectName, projectUri, vscode) {
    const readmeContent = generateReadmeContent(language, projectName, vscode);
    const readmeUri = vscode.Uri.joinPath(projectUri, 'README.md');
    
    const encoder = new TextEncoder();
    await vscode.workspace.fs.writeFile(readmeUri, encoder.encode(readmeContent));
}

/**
 * Generate README.md content
 */
function generateReadmeContent(language, projectName, vscode) {
    const now = new Date();
    const dateStr = now.toLocaleDateString('en-US', { 
        month: 'short', 
        day: '2-digit', 
        year: 'numeric' 
    });
    
    // Get user info from settings
    const config = vscode.workspace.getConfiguration('tsiheader');
    const username = config.get('username') || 'TSI Student';
    const email = config.get('email') || 'student@tsi.lv';
    
    const languageDisplayName = getLanguageDisplayName(language);
    const buildCommand = getBuildCommand(language);
    const runCommand = getRunCommand(language, projectName);
    const projectStructure = getProjectStructure(language, projectName);
    const languageResources = getLanguageResources(language);
    
    return `# ${projectName}

**Transport and Telecommunication Institute - ${languageDisplayName} Programming Project**

![TSI Logo](https://tsi.lv/themes/custom/tsi/logo.svg)

## 📋 Project Information

- **Language**: ${languageDisplayName}
- **Author**: ${username}
- **Email**: ${email}
- **Created**: ${dateStr}
- **Institution**: Transport and Telecommunication Institute (TSI)
- **Website**: [https://tsi.lv](https://tsi.lv)

## 🚀 Quick Start

### Prerequisites

Make sure you have the following installed:
${getPrerequisites(language)}

### Build the Project

\`\`\`bash
${buildCommand}
\`\`\`

### Run the Project

\`\`\`bash
${runCommand}
\`\`\`

### Clean Build Artifacts

\`\`\`bash
make clean
\`\`\`

## 📁 Project Structure

\`\`\`
${projectStructure}
\`\`\`

## 🛠️ Development

### Build Options

- **Debug Build**: \`make debug\` - Includes debugging symbols and debug output
- **Release Build**: \`make release\` - Optimized for production
- **Clean**: \`make clean\` - Remove all build artifacts
- **Install**: \`make install\` - Install to system (requires sudo)

### Code Style

This project follows TSI programming standards:

- ✅ Professional TSI headers in all source files
- ✅ Proper indentation and formatting
- ✅ Meaningful variable and function names
- ✅ Comprehensive documentation
- ✅ Error handling and input validation

## 🎓 TSI Academic Requirements

This project includes all required elements for TSI programming courses:

- ✅ **Professional Headers**: All source files include TSI institutional headers
- ✅ **Project Structure**: Industry-standard directory organization  
- ✅ **Build System**: Complete Makefile with multiple targets
- ✅ **Documentation**: Comprehensive README and code comments
- ✅ **Version Control**: Git-ready with appropriate .gitignore

## 📚 Learning Resources

### ${languageDisplayName} Programming
${languageResources}

### TSI Resources
- [TSI Official Website](https://tsi.lv)
- [Study Programs](https://tsi.lv/study)
- [Programming Courses](https://tsi.lv/study/undergraduate)
- [Library Resources](https://tsi.lv/library)

### Development Tools
- [Visual Studio Code](https://code.visualstudio.com/)
- [TSI Header Extension](https://marketplace.visualstudio.com/items?itemName=st93642.tsi-header)
- [Git Documentation](https://git-scm.com/doc)
- [GNU Make Manual](https://www.gnu.org/software/make/manual/)

## 🤝 Contributing

If this is a collaborative project, please:

1. Fork the repository
2. Create a feature branch (\`git checkout -b feature/amazing-feature\`)
3. Commit your changes with TSI headers (\`git commit -m 'Add amazing feature'\`)
4. Push to the branch (\`git push origin feature/amazing-feature\`)
5. Open a Pull Request

## 📄 License

This project is created for educational purposes at Transport and Telecommunication Institute (TSI). Please respect TSI's academic integrity policies.

## 📞 Contact

- **Author**: ${username}
- **Email**: ${email}
- **Institution**: Transport and Telecommunication Institute
- **Address**: Lomonosova 1, Riga, LV-1019, Latvia
- **Website**: [https://tsi.lv](https://tsi.lv)

---

*Generated by [TSI Header Extension](https://marketplace.visualstudio.com/items?itemName=st93642.tsi-header) for Visual Studio Code*

**🎓 Excellence in Technical Education - Transport and Telecommunication Institute**`;
}

/**
 * Get language display name
 */
function getLanguageDisplayName(language) {
    const displayNames = {
        'c': 'C',
        'cpp': 'C++',
        'rust': 'Rust',
        'ruby': 'Ruby',
        'php': 'PHP'
    };
    return displayNames[language] || language.toUpperCase();
}

/**
 * Get build command for language
 */
function getBuildCommand(language) {
    if (language === 'c' || language === 'cpp') {
        return 'make';
    } else if (language === 'rust') {
        return 'cargo build';
    } else if (language === 'ruby') {
        return 'bundle install';
    } else if (language === 'php') {
        return 'composer install';
    }
    return 'make';
}

/**
 * Get run command for language and project
 */
function getRunCommand(language, projectName) {
    if (language === 'c' || language === 'cpp') {
        return 'make run';
    } else if (language === 'rust') {
        return 'cargo run';
    } else if (language === 'ruby') {
        return 'ruby bin/main.rb';
    } else if (language === 'php') {
        return 'php -S localhost:8000 -t public/';
    }
    return `./build/${projectName}`;
}

/**
 * Get project structure for language
 */
function getProjectStructure(language, projectName) {
    if (language === 'c' || language === 'cpp') {
        const extension = language === 'c' ? 'c' : 'cpp';
        const headerExt = language === 'c' ? 'h' : 'hpp';
        
        return `${projectName}/
├── src/
│   └── main.${extension}              # Main source file with TSI header
├── include/
│   └── ${projectName}.${headerExt}     # Project header file
├── build/                    # Build artifacts (generated)
├── docs/                     # Documentation
├── Makefile                  # Build configuration
├── README.md                 # This file
└── .gitignore               # Git ignore patterns`;
    } else if (language === 'rust') {
        return `${projectName}/
├── src/
│   ├── main.rs               # Main source file with TSI header
│   ├── lib.rs                # Library interface
│   └── base_struct.rs        # Base struct implementation
├── tests/                    # Integration tests
├── examples/                 # Example programs
├── benches/                  # Benchmark tests
├── Cargo.toml                # Project configuration
├── Cargo.lock                # Dependency lock file (generated)
├── docs/                     # Documentation
├── README.md                 # This file
└── .gitignore               # Git ignore patterns`;
    } else if (language === 'ruby') {
        return `${projectName}/
├── bin/
│   └── main.rb               # Executable script with TSI header
├── lib/
│   └── base_class.rb         # Base class implementation
├── spec/                     # RSpec tests
├── config/                   # Configuration files
├── Gemfile                   # Ruby dependencies
├── Rakefile                  # Build automation
├── ${projectName}.gemspec    # Gem specification
├── docs/                     # Documentation
├── README.md                 # This file
└── .gitignore               # Git ignore patterns`;
    } else if (language === 'php') {
        return `${projectName}/
├── public/
│   └── index.php             # Main entry point with TSI header
├── src/
│   └── BaseClass.php         # Base class implementation
├── tests/                    # PHPUnit tests
├── composer.json             # PHP dependencies and configuration
├── composer.lock             # Dependency lock file (generated)
├── docs/                     # Documentation
├── README.md                 # This file
└── .gitignore               # Git ignore patterns`;
    }
    
    return `${projectName}/
├── src/                      # Source code files
├── docs/                     # Documentation
├── build/                    # Build artifacts (generated)
├── Makefile                  # Build configuration
├── README.md                 # This file
└── .gitignore               # Git ignore patterns`;
}

/**
 * Get prerequisites for language
 */
function getPrerequisites(language) {
    if (language === 'c') {
        return `- **GCC Compiler**: \`gcc --version\` (recommended: GCC 9.0+)
- **Make**: \`make --version\`
- **Git**: \`git --version\` (for version control)`;
    } else if (language === 'cpp') {
        return `- **G++ Compiler**: \`g++ --version\` (recommended: GCC 9.0+)
- **Make**: \`make --version\`
- **Git**: \`git --version\` (for version control)`;
    } else if (language === 'rust') {
        return `- **Rust Toolchain**: \`rustc --version\` and \`cargo --version\` (install via [rustup.rs](https://rustup.rs/))
- **Git**: \`git --version\` (for version control)
- **Recommended**: Install Rust via rustup for easy toolchain management`;
    } else if (language === 'ruby') {
        return `- **Ruby**: \`ruby --version\` (recommended: Ruby 3.0+)
- **RubyGems**: \`gem --version\` (usually included with Ruby)
- **Bundler**: \`bundle --version\` (install with \`gem install bundler\`)
- **Git**: \`git --version\` (for version control)
- **Recommended**: Use [RVM](https://rvm.io/) or [rbenv](https://github.com/rbenv/rbenv) for Ruby version management`;
    } else if (language === 'php') {
        return `- **PHP**: \`php --version\` (recommended: PHP 8.0+)
- **Composer**: \`composer --version\` (PHP dependency manager, install from [getcomposer.org](https://getcomposer.org/))
- **Git**: \`git --version\` (for version control)
- **Web Server**: Apache/Nginx or built-in PHP server for development
- **Recommended**: Install PHP with extensions like mbstring, intl, and pdo`;
    }
    
    return '- Basic development environment';
}

/**
 * Get language-specific learning resources
 */
function getLanguageResources(language) {
    if (language === 'c') {
        return `- [C Programming Reference](https://en.cppreference.com/w/c)
- [Learn C - Tutorial](https://www.learn-c.org/)
- [The C Programming Language (K&R Book)](https://www.amazon.com/Programming-Language-2nd-Brian-Kernighan/dp/0131103628)
- [C Programming Exercises](https://www.w3resource.com/c-programming-exercises/)`;
    } else if (language === 'cpp') {
        return `- [C++ Reference](https://en.cppreference.com/w/cpp)
- [Learn C++ Tutorial](https://www.learncpp.com/)
- [C++ Core Guidelines](https://isocpp.github.io/CppCoreGuidelines/CppCoreGuidelines)
- [Modern C++ Features](https://github.com/AnthonyCalandra/modern-cpp-features)`;
    } else if (language === 'rust') {
        return `- [The Rust Programming Language Book](https://doc.rust-lang.org/book/)
- [Rust by Example](https://doc.rust-lang.org/rust-by-example/)
- [Rust Standard Library Documentation](https://doc.rust-lang.org/std/)
- [Cargo Guide](https://doc.rust-lang.org/cargo/guide/)
- [Rustlings Exercises](https://github.com/rust-lang/rustlings)`;
    } else if (language === 'ruby') {
        return `- [The Ruby Programming Language](https://www.ruby-lang.org/en/documentation/)
- [Ruby Documentation](https://ruby-doc.org/)
- [Ruby on Rails Guides](https://guides.rubyonrails.org/) (if using Rails)
- [Ruby Style Guide](https://rubystyle.guide/)
- [Ruby Koans](http://rubykoans.com/) - Learning exercises
- [Try Ruby](https://try.ruby-lang.org/) - Interactive learning`;
    } else if (language === 'php') {
        return `- [PHP Manual](https://www.php.net/manual/en/)
- [PHP The Right Way](https://phptherightway.com/)
- [Composer Documentation](https://getcomposer.org/doc/)
- [Laravel Documentation](https://laravel.com/docs) (if using Laravel framework)
- [Symfony Documentation](https://symfony.com/doc/) (if using Symfony framework)
- [PHP Standards Recommendations (PSR)](https://www.php-fig.org/psr/)
- [PHP CodeSniffer](https://github.com/squizlabs/PHP_CodeSniffer) for code quality`;
    }
    
    return '- Language documentation and tutorials';
}

module.exports = {
    createDocumentationFiles
};