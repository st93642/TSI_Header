# TSI Header v1.1.0 Release Notes

## 🚀 Auto-Save Functionality & Enhanced User Experience

**Release Date:** September 24, 2025  
**Version:** 1.1.0  
**Compatibility:** VS Code 1.74.0+, Ruby 2.7+

---

## 🎉 What's New

### ⚡ Auto-Save Functionality

The most requested feature is here! TSI Header now automatically updates your headers when you save files.

- **📁 Smart Detection**: Only processes files that already have TSI headers
- **🔇 Silent Operation**: Updates happen in the background without notifications
- **⚙️ Configurable**: Enable/disable via `tsiheader.autoUpdate` setting (disabled by default)
- **🛡️ Safe**: Skips auto-update if credentials are missing to prevent errors

**How to Use:**

1. Go to VS Code Settings → Search "tsiheader"
2. Enable "Auto Update" checkbox
3. Save any file with a TSI header - it updates automatically!

### 🎨 Enhanced User Experience  

Say goodbye to scary error messages! We've completely reimagined how the extension handles missing credentials.

**Before v1.1.0:**

```
❌ Error: Please configure your username in VS Code settings...
```

**Now in v1.1.0:**

```
🔧 TSI Header Setup: Please configure your username to get started!

📝 Choose one option:
• VS Code Settings: Search "tsiheader.username"  
• Git config: git config --global user.name "YourUsername"
• Environment: Set TSI_USERNAME variable

[Open Settings] [Git Config Help]
```

### 🔍 Smart Credential Detection

The extension now checks multiple sources automatically:

1. **VS Code Settings** (`tsiheader.username`, `tsiheader.email`)
2. **Git Configuration** (`git config user.name`, `git config user.email`)  
3. **Environment Variables** (`TSI_USERNAME`, `TSI_EMAIL`)

No more manual configuration required if you already have git set up!

---

## 🛠️ Technical Improvements

### Performance Optimizations

- **Lightning Fast Header Detection**: Quick pattern matching without full file parsing
- **Selective Processing**: Only processes relevant files during auto-save
- **Memory Efficient**: Minimal background resource usage

### Enhanced Error Handling  

- **Proactive Validation**: Checks credentials before calling CLI
- **Graceful Fallbacks**: Multiple credential sources with intelligent prioritization
- **User-Friendly Guidance**: Helpful instructions instead of technical errors
- **Direct Assistance**: One-click access to settings and documentation

### Developer Experience

- **Comprehensive Logging**: Detailed console output for debugging
- **Silent Failures**: Background operations don't interrupt workflow  
- **Robust Architecture**: Enhanced stability and error recovery

---

## 📋 Configuration Reference

### New Settings

| Setting | Type | Default | Description |
|---------|------|---------|-------------|
| `tsiheader.autoUpdate` | boolean | `false` | Automatically update headers when files are saved |

### Existing Settings  

| Setting | Type | Default | Description |
|---------|------|---------|-------------|
| `tsiheader.username` | string | `""` | Username for TSI headers |
| `tsiheader.email` | string | `""` | Email address for TSI headers |

---

## 🎯 Getting Started

### Installation

```bash
# Install from VS Code Marketplace
code --install-extension st93642.tsi-header

# Or install from local package
code --install-extension tsi-header-1.1.0.vsix
```

### Quick Setup

1. **Install the extension**
2. **Configure credentials** (choose one):
   - VS Code: Settings → Search "tsiheader" → Set username/email
   - Git: `git config --global user.name "Your Name"`
   - Git: `git config --global user.email "your.email@example.com"`
3. **Enable auto-save** (optional): Settings → Search "tsiheader" → Enable "Auto Update"
4. **Start coding!** Use `Ctrl+Alt+H` to insert headers

---

## 🔄 Upgrade Guide

### From v1.0.x to v1.1.0

- **✅ Fully Backward Compatible** - No breaking changes
- **🔧 New Settings Available** - `tsiheader.autoUpdate` setting added
- **🎨 Enhanced UX** - Better error messages (automatic improvement)
- **⚡ New Features** - Auto-save functionality available

### Migration Steps

1. **Update Extension**: Install v1.1.0 from VS Code Marketplace
2. **Review Settings**: Check new `autoUpdate` option in settings
3. **Test Functionality**: Try the improved credential setup experience
4. **Enable Auto-Save**: Optional - enable for enhanced workflow

---

## 🐛 Known Issues & Solutions

### Auto-Save Not Working?

- ✅ **Check Settings**: Ensure `tsiheader.autoUpdate` is enabled
- ✅ **Verify Header**: File must have existing TSI header for auto-updates
- ✅ **Check Credentials**: Username and email must be configured

### Credential Setup Issues?

- ✅ **Multiple Sources**: Try VS Code settings, git config, or environment variables
- ✅ **Use Helper**: Click "Open Settings" or "Git Config Help" in setup popup
- ✅ **Check Git**: Run `git config --global user.name` to verify git setup

### Performance Concerns?

- ✅ **Selective Processing**: Only files with headers are processed
- ✅ **Quick Detection**: Fast pattern matching, not full file parsing
- ✅ **Background Operation**: Auto-updates don't block UI

---

## 🙏 Credits & Acknowledgments

**Developed by:** st93642 (Igors Oleinikovs)  
**Institution:** Transport and Telecommunication Institute, Riga, Latvia  
**Email:** <st93642@students.tsi.lv>  
**Repository:** <https://github.com/st93642/TSI_Header>

**Special Thanks:**

- Original inspiration from 42 Header extension
- VS Code extension development community
- Ruby programming language contributors
- All beta testers and feedback providers

---

## 📊 Release Statistics

- **Extension Size:** 162.93 KB
- **Files Included:** 21
- **Supported Languages:** 84+
- **Test Coverage:** 100% production scenarios
- **Compatibility:** Windows, macOS, Linux

---

## 🔗 Useful Links

- **📖 Documentation:** [GitHub Repository](https://github.com/st93642/TSI_Header)
- **🐛 Report Issues:** [GitHub Issues](https://github.com/st93642/TSI_Header/issues)
- **💬 Support:** <st93642@students.tsi.lv>
- **🏫 Institution:** [TSI Official Website](https://tsi.lv)

---

**Happy Coding! 🎉**

*TSI Header v1.1.0 - Professional headers for professional developers*
