# TSI Header Extension

![TSI Header Overview](resources/main-picture.png)

## TL;DR

- Insert institution-branded headers and boilerplate across **147+ languages**.
- Scaffold production-ready projects for **C / C++ / Java / Python / Ruby / Rust / PHP**.
- Teach and practice with in-editor **Learn Mode** lessons and exercises.
- Stay focused via the **Study Mode** Pomodoro timer with persistent analytics.

## What ships in the repo

| Area | Key files | Highlights |
| --- | --- | --- |
| VS Code entry | `core/src/extension.js` | Registers commands, shells into Ruby CLI, wires Study/Learn modes |
| Ruby engine | `core/lib/tsi_header/**` | Generates 79-char headers, parses metadata, maintains delimiters |
| Generators | `core/generators/**` | Code bases, classes, and project scaffolds per language |
| Learn Mode | `learn/**`, `learn_exercises/**` | Curriculum JSON + Markdown lessons + exercises with tests |
| Study Mode | `studyMode/timer.js`, `core/src/studyModeExtension.js` | Monotonic timer, modal confirmations, globalState persistence |

## Everyday workflows

- **Build the extension**: `npm run compile` (Ruby `scripts/compile.rb` copies JS into `out/`).
- **Package for VSIX**: `npx vsce package`.
- **Full regression**: `ruby TEST_Suite/full_test_suite.rb` (auto-loads `test_*.rb`).
- **Header sanity**: `ruby spec/unified_test.rb` (ensures delimiters + formatting).
- **Study Mode unit tests**: `cd studyMode && npm test` (Node built-in runner).
- **Learn module checks**: `ruby TEST_Suite/test_learn_module.rb`.

> ℹ️ `npm test` points to `node unified_test_suite.js`, which is currently absent. Lean on the Ruby/Node suites above until the JS harness returns.

## Configuration quick hits

- User identity resolves in this order: VS Code settings → Git config → `TSI_USERNAME` / `TSI_EMAIL` env vars.
- Use `TSICore.utils.detectLanguageFromExtension()` to cover `.erb`, `.vue`, `.h`, multi-part extensions, etc.
- Learn progress, study timer state, and hints are stored under `context.globalState` (`learn_progress_{lang}`, `studyMode.state`, `hint_{exercise}`).

## Working with features

- **Headers & code**: Always invoke the Ruby CLI (`core/lib/tsi_header_cli.rb`) instead of hand-writing headers; generators live in `core/generators/languages/` with `generate<Lang>CodeBase` exports.
- **Projects**: Project creator flows reuse helpers under `core/generators/project/`; extend those rather than crafting ad-hoc IO.
- **Learn Mode**: Exercise metadata couples curriculum JSON → exercises JSON → solutions JSON. Keep IDs in sync to preserve hints/tests/solutions.
- **Study Mode**: Timer logic trusts `elapsedTime` + `process.hrtime.bigint()` for accuracy. Any UX tweak must keep modal confirmations before state mutation.

## Getting started in VS Code

1. Clone or open the repo, run `npm install` (installs only dev typings).
2. `npm run compile` to mirror the active `core/src/extension.js` into `out/`.
3. Launch the extension (F5) or package with `npx vsce package`.
4. Configure `tsiheader.username` / `tsiheader.email` or rely on Git settings.
5. Use Activity Bar panels:

- **TSI Commands** for headers, classes, code bases.
- **TSI Projects** for language scaffolds.
- **📚 Learn** (Ruby, C++) and **Study Mode** commands for curriculum + Pomodoro.

## Troubleshooting snapshot

| Issue | Quick check |
| --- | --- |
| Ruby CLI errors | Ensure Ruby ≥ 2.7 is on PATH; run `ruby spec/unified_test.rb`. |
| Headers missing names | Set `tsiheader.username` / `tsiheader.email` or Git `user.name` / `user.email`. |
| Learn Mode exercise failures | Files live under `learn_exercises/{lang}`; run `ruby TEST_Suite/test_learn_module.rb` to validate metadata. |
| Study Mode not resuming | Timer state persists in `context.globalState`; confirm `tsiheader.studyMode.*` settings and retry `tsiheader.startStudySession`. |
| `npm test` fails | JS harness placeholder; rely on Ruby suites until reinstated. |

## Contributing

- Follow TDD: write or update failing tests first (Ruby suites or Study Mode Node tests) before implementation.
- Extend delimiter support in both Ruby (`core/lib/tsi_header/delimiters.rb`) and JS generators when adding languages.
- Keep `package.json` contributions aligned with `core/src/extension.js` registrations and tree providers.
- Document new commands or flows either here or in the Copilot guide (`.github/copilot-instructions.md`).

---
