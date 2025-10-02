# TSI Header – Copilot Guide
1. **Mission-critical rules**
	- ALWAYS apply TDD: add or update failing tests before implementation, then code until they pass.
	- Never create summary/status/change log files unless a user explicitly asks for them.
	- Preserve the 79-character TSI header format; rely on `core/lib/tsi_header_cli.rb` instead of hand-editing headers.
2. **Architecture snapshot**
	- VS Code entry `core/src/extension.js` orchestrates commands and shells to the Ruby CLI via `execSync`.
	- Shared interface `core/index.js` exposes `TSICore` (header manager, code generator, project scaffolding, utils); call through it rather than deep-linking modules.
	- Ruby engine lives under `core/lib/tsi_header/**` (parsing, delimiters, configuration precedence); extend `delimiters.rb` when adding languages.
	- Study Mode pairs `studyMode/timer.js` (monotonic `elapsedTime` using `process.hrtime.bigint`) with `core/src/studyModeExtension.js` (modal confirmations, state persisted in `context.globalState`).
	- Learn Mode uses `learn/index.js` to glue `learn_manager`, `progress_tracker`, and `exercise_runner` to curriculum JSON/Markdown/solution triples in `learn/curriculum/<lang>/`.
3. **Core workflows**
	- Comprehensive regression: `ruby TEST_Suite/full_test_suite.rb` (auto-loads `test_*.rb` modules) plus `ruby spec/unified_test.rb` for header delimiter sanity.
	- Learn module validation: `ruby TEST_Suite/test_learn_module.rb`; code generation: `ruby TEST_Suite/test_codebase_insertion.rb`; headers: `ruby TEST_Suite/test_header_insertion.rb`—run the suites you impact.
	- Study Mode unit tests: `cd studyMode && npm test` (Node built-in runner executing `timer.test.js`).
	- Build/preview the extension with `npm run compile` (Ruby `scripts/compile.rb` copies `core/src/extension.js` into `out/`); package via `npx vsce package`.
	- `npm test` is wired to `node unified_test_suite.js`, which is currently absent—lean on the Ruby/Node suites above until that harness returns.
4. **Patterns to follow**
	- Respect configuration precedence: VS Code settings → `git config` → `TSI_USERNAME/TSI_EMAIL`; use `TSICore.utils.getUserConfig` instead of custom lookups.
	- Use `core.utils.detectLanguageFromExtension` when handling nonstandard extensions (`.erb`, `.vue`, `.h`, etc.).
	- 147 language generators live in `core/generators/languages/`; each exports a `generate<Lang>CodeBase` consumed by `TSICore.codeGenerator`.
	- Project scaffolding pulls from `core/generators/project/**` (language creators, build files, docs, gitignore); reuse those helpers rather than duplicating logic.
	- Learn exercises require synchronized curriculum JSON → exercise JSON → solution JSON metadata consumed by `ExerciseRunner` and tracked via `learn_progress_{language}`.
5. **When extending features**
	- Add language header support by updating both the JS generator and `TSIHeader::Delimiters::LANGUAGE_DELIMITERS`, and cover it in `spec/unified_test.rb`.
	- New Study Mode behavior must keep `elapsedTime` as the persistence source of truth and continue modal confirmations before mutating state.
	- New Learn features should update progress tracking keys and add Ruby tests that catch missing lessons/exercises early.
	- Keep `package.json` contributions in sync with `core/src/extension.js` registrations and tree providers.
6. **Verification checklist**
	- After implementation, rerun every suite impacted by your change and ensure green before responding.
	- If touching activation/build flows, launch the extension in VS Code (F5) after `npm run compile` for a smoke test.
	- Surface any failing test output verbatim if you cannot resolve it; do not hand off with red builds.

