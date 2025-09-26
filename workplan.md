# TSI Header Extension - Language Support Expansion Workplan

## Overview

The TSI Header extension currently supports **126 programming languages**. Analysis of the `extension_to_vscode.csv` file reveals **643 unsupported languages** with **1,103 file extensions** that could be added.

## Current Status

- ✅ **Supported**: 126 languages
- ❌ **Unsupported**: 643 languages
- 📊 **Coverage**: ~16.4% of identified languages

## Priority Classification

### 🔥 HIGH PRIORITY (Immediate - Next 2-4 weeks)

Languages with high developer usage, VS Code support, and straightforward comment syntax.

#### Web Development & Popular Frameworks

- **TSX** (.tsx) - React TypeScript (High usage, VS Code: `typescriptreact`)
- **Svelte** (.svelte) - Modern web framework (VS Code: `svelte`)
- **Vue.js** (.vue) - Already partially supported but needs verification
- **Astro** (.astro) - Modern static site generator
- **Edge** (.edge) - Laravel templating

#### Data & Configuration

- **TOML** (.toml) - Configuration format (Already supported! ✅)
- **YAML** (.yaml, .yml) - Configuration (Already supported! ✅)
- **JSON5** (.json5) - Enhanced JSON
- **JSON with Comments** (.jsonc) - VS Code format
- **CSV** (.csv) - Data format

#### Infrastructure & DevOps

- **Dockerfile** (.dockerfile, .containerfile) - Already supported! ✅
- **Terraform** (.tf, .tfvars) - Infrastructure as Code
- **HCL** (.hcl, .nomad) - HashiCorp Configuration Language
- **Nginx** (.nginx, .nginxconf) - Web server config

#### Modern Languages

- **Go** (.go) - Already supported! ✅
- **Rust** (.rs) - Already supported! ✅
- **Zig** (.zig) - Already supported! ✅
- **Odin** (.odin) - Systems programming
- **Carbon** (.carbon) - Experimental C++ successor

### 🟡 MEDIUM PRIORITY (Next 1-2 months)

Established languages with good tooling support.

#### Functional Programming

- **F#** (.fs, .fsi, .fsx) - .NET functional language (VS Code: `fsharp`)
- **OCaml** (.ml, .mli) - Already supported! ✅
- **Haskell** (.hs) - Already supported! ✅
- **Elixir** (.ex, .exs) - Already supported! ✅
- **Clojure** (.clj, .cljs) - Already supported! ✅
- **Racket** (.rkt) - Already supported! ✅

#### Systems Programming

- **D** (.d, .di) - Already supported! ✅
- **Nim** (.nim) - Already supported! ✅
- **Crystal** (.cr) - Already supported! ✅
- **V** (.v) - Already supported! ✅

#### Scientific Computing

- **Julia** (.jl) - Already supported! ✅
- **R** (.r, .R) - Already supported! ✅
- **MATLAB** (.m) - Already supported! ✅

### 🟢 LOW PRIORITY (Future phases)

Niche or specialized languages.

#### Legacy & Domain-Specific

- **COBOL** (.cob, .cbl) - Already supported! ✅
- **Fortran** (.f90, .f95) - Already supported! ✅
- **Ada** (.adb, .ada) - Already supported! ✅

#### Esoteric & Educational

- **Brainfuck** (.bf, .b)
- **Whitespace** (no extensions)
- **LOLCODE** (.lol)
- **Shakespeare** (.spl)

## Implementation Strategy

### Phase 1: High Priority Languages (2-4 weeks)

1. **TSX/TSX React** - Add to delimiters as `typescriptreact` with `//` comments
2. **Svelte** - Add to delimiters as `svelte` with `<!-- -->` HTML comments
3. **JSON5/JSONC** - Add to delimiters with `//` or `/* */` comments
4. **Terraform** - Add to delimiters as `terraform` with `#` comments
5. **HCL** - Add to delimiters as `hcl` with `#` or `//` comments

### Phase 2: Medium Priority Languages (1-2 months)

1. **F#** - Add to delimiters as `fsharp` with `//` or `(* *)` comments
2. **Odin** - Add to delimiters as `odin` with `//` comments
3. **Carbon** - Add to delimiters as `carbon` with `//` comments

### Phase 3: Low Priority & Niche Languages (Ongoing)

Focus on languages with active communities and practical use cases.

## Technical Implementation

### Adding Language Support

For each new language, add an entry to `LANGUAGE_DELIMITERS` in `lib/tsi_header/delimiters.rb`:

```ruby
'language_id' => COMMENT_STYLE_ARRAY
```

Where `COMMENT_STYLE_ARRAY` is one of:

- `SLASHES` - `/* */` and `//`
- `HASHES` - `#`
- `DASHES` - `--`
- `ANGLE_BRACKETS` - `<!-- -->`
- `SEMICOLONS` - `;;`
- `PERCENTS` - `%%`

### Testing Requirements

- Add language to test arrays in `unified_test_suite.js`
- Verify syntax correctness
- Test header insertion, update, and removal
- Ensure 100% test pass rate maintained

### Documentation Updates

- Update `PROJECT_MAP.md` with new language support
- Update README.md language count
- Update changelog with new features

## Success Metrics

### Quantitative Goals

- **Phase 1**: Add 10+ high-priority languages (Total: 136+ languages)
- **Phase 2**: Add 15+ medium-priority languages (Total: 151+ languages)
- **Phase 3**: Add 20+ low-priority languages (Total: 171+ languages)
- **Coverage Target**: 25%+ of identified languages

### Quality Standards

- ✅ All tests pass (277/277)
- ✅ No breaking changes
- ✅ Consistent comment formatting
- ✅ VS Code language ID compatibility

## Risk Assessment

### Low Risk

- Languages using existing comment styles (SLASHES, HASHES, etc.)
- Languages with VS Code support
- Well-established languages

### Medium Risk

- Languages with unique comment syntax requiring new delimiter arrays
- Languages without VS Code language server support
- Experimental or rapidly evolving languages

### High Risk

- Esoteric languages with non-standard syntax
- Languages requiring complex parsing logic
- Legacy languages with limited tooling

## Resource Requirements

### Time Estimates

- **High Priority**: 2-4 weeks (40-80 hours)
- **Medium Priority**: 4-8 weeks (80-160 hours)
- **Low Priority**: 8-16 weeks (160-320 hours)

### Skills Needed

- Ruby programming (delimiter configuration)
- JavaScript/Node.js (VS Code extension)
- Understanding of programming language syntax
- Test-driven development

## Next Steps

1. **Immediate Action**: Start with Phase 1 languages
2. **Community Input**: Survey users for most requested languages
3. **Tooling**: Create automated scripts for language addition
4. **Documentation**: Update contribution guidelines for new language support

---

*Generated from analysis of `extension_to_vscode.csv`*
*Total unsupported languages: 643*
*Total unsupported extensions: 1,103*
*Last updated: September 26, 2025*
