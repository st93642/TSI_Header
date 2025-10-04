# TSI Header Release Notes

## v6.0.8 (October 4, 2025)

### New Features

- **DSA C++ Curriculum**: Added comprehensive Data Structures and Algorithms lessons for C++, including:
  - 20+ new lessons covering arrays, trees, graphs, algorithms, and more
  - Interactive quizzes with 10 questions each
  - Detailed solutions and key points
  - Rich SVG diagrams and visualizations
- **Curriculum Integrity Testing**: New Ruby test suite (`TEST_Suite/test_learn_cpp_dsa_integrity.rb`) to validate lesson assets, quiz schemas, and solution alignment
- **Enhanced Learn Mode**: Improved image loading and resource resolution for lessons
- **Packaging Optimizations**: Updated `.vscodeignore` to exclude test suites from extension bundle

### Improvements

- Normalized quiz schemas across all DSA exercises (added `passScore`, `totalQuestions`, consistent answer formats)
- Updated solution files with `exerciseId` fields and standardized answer keys
- Refactored shortest path quiz to align with shared schema
- Synced `package-lock.json` with version bump

### Technical Changes

- Version bump to 6.0.8
- Added 170+ new files including lessons, exercises, solutions, and resources
- Deleted obsolete `RELEASE_NOTES.md` (now maintained via GitHub releases)

### Quality Assurance

- All DSA curriculum validated via new integrity tests
- Extension packaged and tested locally
- Ruby test suites pass for curriculum structure

### Known Issues

- Activation event warning: Extension uses '*' activation for broad compatibility (consider optimizing for performance in future releases)

For full changelog, see commit history from v6.0.0 to v6.0.8.
