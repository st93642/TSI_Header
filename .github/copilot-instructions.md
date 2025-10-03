# TSI Header – Copilot Guide
1. **Mission-critical rules**
	- ALWAYS apply TDD: add or update failing tests before implementation, then code until they pass.
	- Never create summary/status/change log files unless a user explicitly asks for them.
	- Preserve the 79-character TSI header format; rely on `core/lib/tsi_header_cli.rb` instead of hand-editing headers.
	- Adding lessons to learn modules requires:
		1. A Markdown lesson in `learn/curriculum/<lang>/lessons/` with concept explanation, examples, and practice steps.
		2. An exercise JSON in `learn/curriculum/<lang>/exercises/` defining the task, input/output format, and tests.
		3. A starter code file in `learn_exercises/<lang>/` with scaffolding aligned to the exercise steps.
		4. A solution JSON in `learn/curriculum/<lang>/solutions/` containing the canonical answer.
		5. Updates to `learn/curriculum/<lang>/curriculum.json` to link the lesson and exercise.
		6. Lessons to be deep and broad, covering each topic thoroughly without external references, self-contained (Fetch online, can combine from different sources).
	- When a user requests "a lesson", fulfil the entire lesson bundle: curriculum entry, Markdown lesson, exercise JSON, starter code, and solution JSON.
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
	- **Escape-character regression (Oct 2025):** Unescaped control characters inside exercise JSON (`starterCode`, descriptions, hints) caused Learn Mode to crash at runtime and generated starter files with invalid C/C++ source. Fix by always emitting escaped newlines in JSON and letting `LearnManager.startExercise` materialize the starter on demand. Guard rails:
		- `node learn/tests/exercise_escape_integrity.test.js` scans every exercise JSON for raw control characters and verifies `JSON.parse` still succeeds.
		- `node learn/tests/learn_manager_starter_creation.test.js` spins up a temporary workspace and asserts the generated starter file exactly matches the escaped JSON payload.
		- Both scripts are executed automatically when you run `ruby TEST_Suite/test_learn_module.rb`; keep them green whenever you touch exercises, starters, or LearnManager write paths.
4. **Patterns to follow**
	- Respect configuration precedence: VS Code settings → `git config` → `TSI_USERNAME/TSI_EMAIL`; use `TSICore.utils.getUserConfig` instead of custom lookups.
	- Use `core.utils.detectLanguageFromExtension` when handling nonstandard extensions (`.erb`, `.vue`, `.h`, etc.).
	- 147 language generators live in `core/generators/languages/`; each exports a `generate<Lang>CodeBase` consumed by `TSICore.codeGenerator`.
	- Project scaffolding pulls from `core/generators/project/**` (language creators, build files, docs, gitignore); reuse those helpers rather than duplicating logic.
	- Learn exercises require synchronized curriculum JSON → exercise JSON → solution JSON metadata consumed by `ExerciseRunner` and tracked via `learn_progress_{language}`.
	- Every Learn lesson and exercise must spell out prescriptive "Steps" plus an "Expected output" (or equivalent explicit requirements); keep tests, descriptions, starter code, and solutions perfectly aligned so learners know exactly what to build.
	- The new C++ curriculum mirrors Ruby: every lesson in `learn/curriculum/cpp/lessons` needs matching exercise/solution JSON with output-based tests that satisfy `TEST_Suite/test_learn_cpp.rb`.
5. **When extending features**
	- Add language header support by updating both the JS generator and `TSIHeader::Delimiters::LANGUAGE_DELIMITERS`, and cover it in `spec/unified_test.rb`.
	- New Study Mode behavior must keep `elapsedTime` as the persistence source of truth and continue modal confirmations before mutating state.
	- New Learn features should update progress tracking keys and add Ruby tests that catch missing lessons/exercises early.
	- Keep `package.json` contributions in sync with `core/src/extension.js` registrations and tree providers.
6. **Verification checklist**
	- After implementation, rerun every suite impacted by your change and ensure green before responding.
	- If touching activation/build flows, launch the extension in VS Code (F5) after `npm run compile` for a smoke test.
	- Surface any failing test output verbatim if you cannot resolve it; do not hand off with red builds.
	
7. **Learn module authoring recipe**
	- **Lesson blueprint (deep & broad)**
		1. Map the learner journey: set learning goals → break them into 3‑6 checkpoints, each escalating in complexity.
		2. Draft narrative structure: hook (why it matters), concept explainer, guided example, and mini-project/recap.
		3. Embed interaction cues: call out terminal commands, expected compiler output, and self-check questions per checkpoint.
		4. Close the loop: summarize key takeaways, list next steps (exercise + tests to run), and reference required tooling.
		5. Run markdown lint and ensure code fences compile conceptually (syntax-highlighted language, real include paths, etc.).
	- **Interactive exercise pipeline**
		1. Mirror the lesson IDs: create/update `learn/curriculum/<lang>/exercises/<lesson_id>_exercise.json` plus a matching starter file under `learn_exercises/<lang>/` and a solution JSON under `learn/curriculum/<lang>/solutions/`.
		2. Author a concise brief with scenario, numbered steps, explicit input/output format, and an example run identical to tests.
		3. Provide runnable starter code: include placeholders and comments aligned with each numbered step; avoid hidden magic.
		4. Define tests in JSON (`type: "output"`, `"code"`, or custom harness) so `ExerciseRunner` can execute via the Ruby suite.
		5. Write the canonical solution, emphasizing edge cases introduced in the lesson; validate manually before committing.
	- **Checks & quality gates**
		- Run `ruby TEST_Suite/test_learn_module.rb` after curriculum/exercise changes; it validates ID wiring, JSON schema, and sample outputs.
		- When adding new languages or adjusting generators, extend `TEST_Suite/test_learn_<lang>.rb` and ensure `full_test_suite.rb` stays green.
		- For interactive code snippets, compile/run them locally (or via CI harness) to guarantee the Example Run matches test fixtures.
	- **Solution availability**
		- Store authoritative answers in `learn/curriculum/<lang>/solutions/<lesson_id>_exercise.json`; keep them in sync with starters.
		- Expose hints sparingly via the `"hints"` array—progressive disclosure from gentle nudge → specific tactic → near-spoiler.
		- Maintain parity between solution code and tests so learners passing the suite can compare their approach without surprises.
	- **How Learn module flows**
		- `learn/index.js` wires `LearnManager` (loads curriculum metadata), `ProgressTracker` (persists `learn_progress_{lang}`), and `ExerciseRunner` (executes tests via child processes/Ruby stubs).
		- Curriculum JSON defines modules → lessons; lessons point to Markdown (concept) and exercise IDs.
		- The VS Code tree view reflects this structure; completing an exercise updates progress and unlocks the next node.
		- Lesson content + exercises use shared IDs so telemetry, persistence, and hint throttling remain consistent.
		- Any new asset must respect this chain: curriculum entry → Markdown lesson → exercise JSON → starter file → solution JSON → tests.

