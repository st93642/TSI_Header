# TSI Header Extension

![TSI Header Overview](resources/main-picture.png)

## Features

- Insert institution-branded headers and boilerplate across **147+ languages**.
- Scaffold production-ready projects for **C / C++ / Java / Python / Ruby / Rust / PHP / HTML**.
- Teach and practice with in-editor **Learn Mode** lessons and exercises.
- Stay focused via the **Study Mode** Pomodoro timer with persistent analytics.

## Requirements

- **Visual Studio Code**: Version 1.74.0 or later.
- **Node.js**: Version 16 or later (for development and testing).
- **Ruby**: Version 2.7 or later (for CLI operations and compilation).
- **Git**: For user identity resolution and project scaffolding.
- **Supported Languages**: C, C++, Java, Python, Ruby, Rust, PHP, HTML, and 140+ others for header insertion.

## Configuration quick hits

- User identity resolves in this order: VS Code settings → Git config → `TSI_USERNAME` / `TSI_EMAIL` env vars.

## Getting started in VS Code

1. Clone or open the repo, run `npm install` (installs only dev typings).
2. `npm run compile` to mirror the active `core/src/extension.js` into `out/`.
3. Launch the extension (F5) or package with `npx vsce package`.
4. Configure `tsiheader.username` / `tsiheader.email` or rely on Git settings.
5. Use Activity Bar panels:

- **TSI Commands** for headers, classes, code bases.
- **TSI Projects** for language scaffolds.
- **📚 Learn** (Ruby, C, C++) and **Study Mode** commands for curriculum + Pomodoro.

## Troubleshooting

### Common Issues

- **Extension not activating**: Ensure VS Code is version 1.74.0+. Reload the window (Ctrl+Shift+P > "Developer: Reload Window").
- **Headers not inserting**: Check user settings (`tsiheader.username`, `tsiheader.email`) or Git config. Run `git config --global user.name` to verify.
- **Compilation errors**: For C/C++ projects, ensure a compatible compiler (GCC/Clang) is installed and in PATH. Test with `g++ --version`.
- **Learn Mode exercises failing**: Verify Node.js and Ruby are installed. Run `ruby TEST_Suite/test_learn_module.rb` to check curriculum integrity.
- **Study Mode not persisting**: Data is stored in VS Code's global state; try resetting with "TSI Header: Reset Study Progress".

### Getting Help

- Check the [GitHub Issues](https://github.com/st93642/TSI_Header/issues) for known problems.
- Run `ruby TEST_Suite/full_test_suite.rb` for comprehensive diagnostics.
- For Learn Mode, ensure `learn_exercises/` is empty (files are generated dynamically).

## Contributing

Contributions welcome!

## License

MIT License - see [LICENSE](LICENSE) for details.
