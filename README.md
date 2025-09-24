# TSI Header Extension for VS Code

![TSI Logo](tsi.jpg)

## Code Generation & Professional Headers for TSI Students

Generate complete classes, boilerplate code, and professional TSI-branded headers for 85+ programming languages.

## 🏗️ **Primary Features**

### **Code Generation** (Main Feature)

- **Generate Classes**: Complete class templates with constructors, methods, and documentation
- **Generate Code Boilerplates**: Ready-to-run program templates for any language
- **89 Languages Supported** for code generation (see full list below)

### **Professional Headers**

- TSI-branded headers with institutional information
- Auto-update timestamps and author info
- Headers can be deleted if auto-update is disabled
- **89 Languages Supported** for headers

## 📋 **Supported Languages**

### **Class Generation** (13 Languages)

Java, C++, C#, Python, JavaScript, Kotlin, PHP, TypeScript, Ruby, Go, Swift, Dart, Scala

### **Code Boilerplate Generation** (89 Languages)

Ada, APL, Assembly, AWK, Basic, Batch, C, C++, CoffeeScript, ColdFusion, CSS, Clojure, COBOL, Dart, Dockerfile, Elixir, Erlang, F#, Factor, Forth, Fortran, Go, Groovy, Haskell, HTML, IDL, INI, Jade, Java, JavaScript, JavaScript React, JSON, Julia, Kotlin, LaTeX, Less, Lisp, Lua, Maple, Markdown, Mathematica, MATLAB, Mercury, Objective-C, Objective-C++, Objective-J, OCaml, Octave, Pascal, Perl, PHP, PostScript, PowerShell, Prolog, Python, R, Ruby, Rust, SAS, SCSS, Scheme, Sed, Shell Script, Smalltalk, Solidity, Swift, TCL, TypeScript, TypeScript React, VB, Verse, VHDL, Verilog, Vimscript, Vue, XML, XSL, YAML

### **Header Generation** (89 Languages)

Same languages as boilerplate generation - every language that can generate code can also have headers

## 🚀 **Quick Start**

### **Installation**

1. Install from VS Code Marketplace or download .vsix file
2. Ensure Ruby 2.7+ is installed
3. Configure your credentials (see Configuration below)

### **Usage**

#### **Code Generation**

- **Add Class**: Right-click → "TSI Header: Add class"
- **Add Code Base**: Right-click → "TSI Header: Add code base"

#### **Header Management**

- **Insert Header**: `Ctrl+Alt+H` or right-click → "TSI Header: Insert Header"
- **Update Header**: `Ctrl+Alt+U` or right-click → "TSI Header: Update Header"
- **Note**: Headers can be manually deleted if auto-update is disabled

### **Configuration**

```json
{
  "tsiheader.username": "your_username",
  "tsiheader.email": "your_email@students.tsi.lv",
  "tsiheader.autoUpdate": false
}
```

Or use Git config:

```bash
git config --global user.name "Your Full Name"
git config --global user.email "your_email@students.tsi.lv"
```

## 📄 **Example Header**

```c
/*****************************************************************************/
/*                                                                           */
/*  filename.c                                           TTTTTTTT SSSSSSS II */
/*                                                          TT    SS      II */
/*  By: username@students.tsi.lv                            TT    SSSSSSS II */
/*                                                          TT         SS II */
/*  Created: Sep 24 2025 11:47 Full Name                    TT    SSSSSSS II */
/*  Updated: Sep 24 2025 11:47 Full Name                                     */
/*                                                                           */
/*   Transport and Telecommunication Institute - Riga, Latvia                */
/*                       https://tsi.lv                                      */
/*****************************************************************************/
```

---

Developed for the Transport and Telecommunication Institute Computer Science Program
