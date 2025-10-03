# Release Notes - TSI Header Extension v6.0.0

## Major Release Highlights

This major release introduces the **Data Structures and Algorithms Module (Module 11)** for C++, expanding the Learn Mode with advanced topics. Significant improvements to rendering, navigation, and testing infrastructure ensure a robust learning experience.

### New Features

- **Module 11: Data Structures and Algorithms**
  - **Lesson 11.0: Launching Advanced Data Structures** - Comprehensive introduction to heaps, priority queues, and their invariants with diagrams and practice exercises.
  - **Lesson 11.1: Heaps and Priority Queues** - Deep dive into heap operations, std::priority_queue usage, algorithmic complexity, and real-world applications.
  - **Lesson 11.2: Union-Find and Disjoint Set Optimisation** - Exploration of disjoint-set forests, path compression, union-by-rank, and applications in graph algorithms.

- **Enhanced Learn Mode**
  - Added quiz navigation allowing progression without full completion for flexible learning.
  - Improved rendering: Replaced tables with bullet lists in lessons for better VS Code webview display.
  - Expanded quizzes to 10 questions each for thorough assessment.
  - New SVG diagrams for visual explanations (heap structures, adapter flows).

- **Testing and Quality Assurance**
  - Dedicated Module 11 test suite (`TEST_Suite/test_learn_cpp_module11.rb`) for regression validation.
  - Enhanced escape character integrity checks to prevent runtime crashes.
  - Updated solution navigation tests for seamless lesson progression.

- **Documentation and Usability**
  - Added Requirements and Troubleshooting sections to README.md for marketplace users.
  - Updated Copilot instructions with Module 11 authoring playbook for future development.

### Technical Improvements

- **Rendering Fixes**: Addressed bad rendering of includes and tables in lesson webviews.
- **Navigation Logic**: Enhanced quiz flow and lesson unlocking mechanisms.
- **Code Quality**: Improved starter code generation and JSON escaping for cross-platform compatibility.

### Breaking Changes

- None. All changes are additive and backward-compatible.

### Known Issues

- '*' activation event may impact performance in large workspaces (as noted in packaging warnings).

### Acknowledgments

Thanks to contributors for expanding the curriculum and improving user experience. This release advances the extension toward comprehensive algorithmic education.

---

For previous releases, see [CHANGELOG.md](CHANGELOG.md) or Git history.