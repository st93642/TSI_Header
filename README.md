# Uni Header Extension

![OnMarketplace](image.png)
![Uni Header Overview](resources/main-picture.png)
![Calendar](resources/calendar.png)

The extension is now live and available for download from the [VS Code marketplace](https://marketplace.visualstudio.com/items?itemName=st93642.uni-header).

- **🆕 Custom Institution Headers**: Configure your own university/organization branding for all generated headers

## Features

- Insert university branded headers and boilerplates across **122+ languages**.
- Scaffold production-ready projects for **C / C++ / Java / Python / Ruby / Rust / PHP / HTML**.
- Teach and practice with in-editor **Learn Mode** lessons and exercises for **C, C++, Ruby, Rust, Git, and The Odin Project**.
- **🆕 Offline caching** for Odin Project lessons - access lessons without internet connection and manage cache from the sidebar.
- Stay focused via the **Study Mode** Pomodoro timer with persistent analytics.
- **🆕 Study Calendar** with deadline tracking, custom events, daily schedules, and **SMTP email notifications**.
- **🆕 Custom Institution Headers** - Configure your own institution name and URL for personalized headers.
- **🆕 Enhanced Header Detection** - Automatically detects and updates headers across all supported languages.
- **🆕 AI Chat Assistant** - Integrated chatbot using locally-hosted Ollama LLM for real-time coding and study assistance.

## Requirements

- **Visual Studio Code**: Version 1.74.0 or later.
- **Ruby**: Version 2.7 or later (required for header generation and CLI operations).
- **Git** (optional): For automatic user identity resolution in headers.
- **Compilers** (optional, for Learn Mode exercises):
  - **C/C++**: GCC, Clang, or MSVC compiler
  - **Ruby**: Ruby runtime for Ruby exercises
  - **Rust**: Rust compiler for Rust exercises
- **Email Server** (optional): SMTP server for calendar email notifications.
- **Ollama** (optional, for Chat Assistant): Download from [ollama.com](https://ollama.com/) and run locally for AI-powered chat assistance.

## Chat Assistant Setup

To use the integrated AI Chat Assistant:

1. **Install Ollama**: Download and install from [ollama.com](https://ollama.com/)
2. **Run Ollama locally**: Start the Ollama server (default: `http://localhost:11434`)
3. **Pull a model**: Run `ollama pull mistral` or your preferred model in terminal
4. **Configure in VS Code Settings**:
   - `tsiheader.chat.ollamaUrl`: Ollama server URL (default: `http://localhost:11434`)
   - `tsiheader.chat.defaultModel`: Model to use (e.g., `mistral`, `llama2`, `neural-chat`)
   - Optional: `tsiheader.chat.temperature`, `maxTokens`, `historyLimit`

5. **Access Chat**: Click the "Chat Assistant" icon in the Activity Bar or use commands `Uni Header: Open Chat`, `New Chat Conversation`, or `Clear Chat History`

## Configuration quick hints

- User identity resolves in this order: VS Code settings → Git config → `TSI_USERNAME` / `TSI_EMAIL` env vars.
- Calendar notifications: Configure SMTP settings for email reminders.
- **Custom Headers**: Enable `tsiheader.customHeader.enableCustomHeader` to use your institution's branding instead of default TSI headers.
- **Chat Assistant**: Configure Ollama URL and model selection in VS Code settings under `tsiheader.chat.*`

## Custom Header Configuration

Personalize headers with your institution's branding:

```json
{
  "tsiheader.customHeader.enableCustomHeader": true,
  "tsiheader.customHeader.institutionName": "Your University Name",
  "tsiheader.customHeader.institutionUrl": "https://your-university.edu"
}
```

When enabled, all generated headers (manual insertion, project creation, auto-updates) will use your custom institution name and URL instead of the default branding.

Use Activity Bar panels:

- **Uni Commands** for headers, classes, code bases.
- **Uni Projects** for language scaffolds.
- **📚 Learn** (Ruby, C, C++, Rust, Git, **Odin Project**) and **Study Mode** commands for curriculum + Pomodoro.
- **📅 Study Calendar** for scheduling deadlines, events, and automated email notifications.
- **💬 Chat Assistant** for AI-powered coding and study assistance using locally-hosted Ollama.

## Calendar Features

### Event Management

- **Deadlines**: Track assignment due dates with priority levels
- **Custom Events**: Schedule study sessions, meetings, or personal events
- **Daily Schedules**: Set recurring time blocks for consistent study routines
- **Import/Export**: Support for iCalendar (.ics) files and URL imports

### Email Notifications

Configure notifications in VS Code settings (`tsiheader.notifications.*`):

- **SMTP**: Direct email server support with STARTTLS encryption

Example SMTP configuration:

```json
{
  "tsiheader.notifications.enableEmail": true,
  "tsiheader.notifications.emailService": "smtp",
  "tsiheader.notifications.smtpHost": "smtp.gmail.com",
  "tsiheader.notifications.smtpPort": 587,
  "tsiheader.notifications.smtpUser": "your-email@gmail.com",
  "tsiheader.notifications.smtpPassword": "your-app-password",
  "tsiheader.notifications.emailAddress": "recipient@example.com",
  "tsiheader.notifications.advanceNotice": 24
}
```

## Troubleshooting

### Common Issues

- **Extension not activating**: Ensure VS Code is version 1.74.0+. Reload the window (Ctrl+Shift+P > "Developer: Reload Window").
- **Headers showing "unknown" username**: This is automatically fixed when updating existing headers. The extension now properly reads git configuration and VS Code settings.
- **Custom headers not working in projects**: Ensure `tsiheader.customHeader.enableCustomHeader` is enabled. Project creation now respects custom header settings.
- **Headers not inserting**: Check user settings (`tsiheader.username`, `tsiheader.email`) or Git config. Run `git config --global user.name` to verify.
- **SMTP connection errors**: Verify server settings and credentials. Use port 587 for STARTTLS or 465 for direct TLS. Check firewall settings.
- **Email notifications not working**: Test with "Uni Header: Test Email Notification" command. Check VS Code output panel for detailed SMTP logs.
- **C/C++ compilation errors on Windows**: Learn Mode automatically probes `g++`, `gcc`, and `clang++` in your PATH plus common MinGW/MSYS2/LLVM install folders. If you still see a "g++ not found" error, follow these steps:
  1. **Install MSYS2 (recommended)**
     - Download the installer from [msys2.org](https://www.msys2.org/) and complete the setup.
     - Open the *MSYS2 MinGW 64-bit* terminal and run:

       ```bash
       pacman -Syu
       pacman -S mingw-w64-x86_64-toolchain
       ```

     - Add `C:\msys64\mingw64\bin` to your Windows `Path` environment variable if it is not added automatically.
  2. **Alternative: Stand-alone MinGW-w64**
     - Download a build from [winlibs.com](https://winlibs.com/) or the official MinGW-w64 project.
     - Extract it to a folder such as `C:\MinGW` and add `C:\MinGW\bin` to `Path`.
  3. **Optional: LLVM/Clang**
     - Install LLVM from [llvm.org](https://releases.llvm.org/download.html) and add `C:\Program Files\LLVM\bin` to `Path` so the runner can find `clang++`.
  4. **Restart VS Code** to ensure the updated environment variables are picked up.
  5. **Verify the compiler** by running:

     ```bash
     g++ --version
     ```

     If you prefer Clang, run `clang++ --version` instead.
  6. **Re-run the Learn exercise**; the runner should automatically pick up the installed compiler. The error message lists every location it checks—use it to confirm your PATH settings if the issue persists.

- **Compilation errors**: For C/C++ projects, ensure a compatible compiler (GCC/Clang) is installed and in PATH. Test with `g++ --version`.

### Getting Help

- Check the [GitHub Issues](https://github.com/st93642/TSI_Header/issues) for known problems.

## License

MIT License - see [LICENSE](LICENSE) for details.
