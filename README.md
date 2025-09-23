# TSI Header Extension for VS Code

![TSI Logo](tsi.jpg)

A VS Code extension that automatically generates professional headers for source code files with Transport and Telecommunication Institute (TSI) branding.

## 👨‍💻 Author

**st93642** - Former 42 Student, TSI Computer Science program

## ✨ Features

### ⚡ **Auto-Save Functionality** (New in v1.1.0!)

- **🔄 Automatic Updates**: Headers update automatically when you save files
- **🎯 Smart Detection**: Only processes files with existing TSI headers  
- **🔇 Silent Operation**: Works quietly in the background
- **⚙️ Configurable**: Enable/disable via `tsiheader.autoUpdate` setting

### 🎨 **Enhanced User Experience** (New in v1.1.0!)

- **🔧 Helpful Setup**: Friendly configuration guidance instead of error messages
- **🔍 Smart Credentials**: Checks VS Code settings, git config, and environment variables
- **📖 Direct Assistance**: One-click links to settings and documentation
- **🛡️ Proactive Validation**: Prevents errors with upfront credential checking

### 🏛️ **Institutional Branding**

- TSI ASCII logo
- University contact information and website
- Professional institutional formatting

### 🌍 **Extensive Language Support (84+ Languages)**

C, C++, Rust, Go, Assembly, JavaScript, TypeScript, HTML, CSS, Python, Ruby, Java, Swift, Kotlin, Haskell, OCaml, F#, Clojure, R, MATLAB, Julia, PHP, Perl, Shell, and many more...

### 🎨 **Smart Formatting**

**Standard Format (C/Java/JavaScript):**

```c
/*****************************************************************************/
/*                                                                           */
/*  filename.c                                           TTTTTTTT SSSSSSS II */
/*                                                          TT    SS      II */
/*  By: username@students.tsi.lv                            TT    SSSSSSS II */
/*                                                          TT         SS II */
/*  Created: Sep 23 2025 13:30 Full Name                    TT    SSSSSSS II */
/*  Updated: Sep 23 2025 13:30 Full Name                                     */
/*                                                                           */
/*   Transport and Telecommunication Institute - Riga, Latvia                */
/*                       https://tsi.lv                                      */
/*****************************************************************************/
```

**Python Frame Format:**

```python
#*****************************************************************************#
#                                                                             #
#  filename.py                                            TTTTTTTT SSSSSSS II #
#                                                            TT    SS      II #
#  By: username@students.tsi.lv                              TT    SSSSSSS II #
#                                                            TT         SS II #
#  Created: Sep 23 2025 13:30 Full Name                      TT    SSSSSSS II #
#  Updated: Sep 23 2025 13:30 Full Name                                       #
#                                                                             #
#   Transport and Telecommunication Institute - Riga, Latvia                  #
#                       https://tsi.lv                                        #
#*****************************************************************************#
```

## 🚀 Installation & Usage

### **Installation**

1. Install the extension in VS Code
2. Ensure Ruby 2.7+ is installed
3. Configure your information in VS Code settings or git config

### **Usage**

- **Right-click menu**: "Insert Header" / "Update Header"
- **Keyboard shortcuts**: `Ctrl+Alt+H` (Insert) / `Ctrl+Alt+U` (Update)
- **Command palette**: `Ctrl+Shift+P` → "TSI Header"

### **Configuration**

The extension automatically uses your git configuration. Alternatively, set in VS Code settings:

```json
{
  "tsiHeader.username": "your_username",
  "tsiHeader.email": "your_email@students.tsi.lv"
}
```

## 🛠️ Technical Implementation

- **Language**: Ruby with JavaScript VS Code wrapper
- **Architecture**: Modular design with separated concerns
- **Features**: 79-character alignment, multi-character delimiters, dynamic spacing

### **Core Components**

- `header_generator.rb` - Template rendering engine
- `delimiters.rb` - Language-specific comment syntax (84+ languages)  
- `tsi_header_cli.rb` - Command-line interface

## 🧪 Testing

Comprehensive test coverage with 84+ programming languages validated for:

- ✅ Perfect 79-character alignment
- ✅ Multi-character delimiter support
- ✅ All comment styles working correctly
- ✅ Dynamic alignment algorithms

## 🤝 Contributing

Pull requests and contributions are welcome! Whether you want to:

- Add support for new programming languages
- Improve existing functionality
- Fix bugs or enhance performance
- Update documentation

Feel free to open an issue or submit a pull request. All contributions help make this extension better.

## 📄 License

This project is licensed under the MIT License.

## 🙏 Acknowledgments

This extension is a complete rework of the original [42 header extension](https://github.com/kube/vscode-42header) created by 42 students, released under the MIT License. The original concept has been completely reimplemented with TSI branding, expanded language support (84+ languages), and enhanced functionality.

---

## Developed for the Transport and Telecommunication Institute Computer Science program
