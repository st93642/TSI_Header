# TSI Header - Learn Mode

Interactive learning platform for programming languages, integrated directly into VS Code.

## Overview

Learn Mode provides a comprehensive, structured approach to learning programming languages with:

- **Progressive Curriculum**: Structured lessons from beginner to advanced
- **Interactive Exercises**: Hands-on coding challenges with instant feedback
- **Progress Tracking**: Monitor your learning journey with streaks and achievements
- **Gamification**: Earn badges and achievements as you progress
- **In-IDE Learning**: Learn without leaving your development environment
- **Quality Content**: Lessons adapted from FreeCodeCamp and Ruby community best practices

## Features

### 📚 Structured Curriculum

Each language has a carefully designed curriculum with:

- Multiple modules covering different topics
- Lessons with clear learning objectives
- Estimated duration for each lesson
- Difficulty levels (beginner, intermediate, advanced)

### 💪 Practice Exercises

Every lesson includes:

- Starter code to get you going
- Automated test suites
- Multiple hints when you're stuck
- Complete solutions with explanations
- Real-world applications

### 📊 Progress Tracking

- Track completed lessons and exercises
- Maintain learning streaks (consecutive study days)
- View total study time
- Unlock achievements as you progress

### 🏆 Achievements System

Earn achievements for:

- Completing your first exercise
- Reaching milestones (5, 10, 25+ exercises)
- Maintaining study streaks (3, 7, 30+ days)
- Finishing entire modules

## Architecture

```text
learn/
├── index.js                 # Main Learn module
├── lib/
│   ├── learn_manager.js     # Curriculum and lesson management
│   ├── progress_tracker.js  # Progress tracking and achievements
│   └── exercise_runner.js   # Exercise execution and testing
└── curriculum/
    └── ruby/                # Ruby curriculum
        ├── curriculum.json  # Course structure
        ├── lessons/         # Markdown lesson files
        ├── exercises/       # Exercise definitions (JSON)
        └── solutions/       # Solution files with explanations
```

## Curriculum Structure

### curriculum.json

Defines the complete course structure:

```json
{
  "language": "Ruby",
  "version": "1.0.0",
  "description": "Interactive Ruby programming curriculum",
  "estimatedHours": 40,
  "modules": [
    {
      "id": "basics",
      "title": "Ruby Basics",
      "description": "Learn the fundamentals",
      "lessons": [
        {
          "id": "hello_world",
          "title": "Hello World and Output",
          "duration": 30,
          "difficulty": "beginner"
        }
      ]
    }
  ]
}
```

### Lesson Files (Markdown)

Located in `lessons/`, written in Markdown with:

- Concept explanations
- Code examples
- Key takeaways
- Practice guidelines

### Exercise Files (JSON)

Located in `exercises/`, define:

```json
{
  "id": "hello_world_exercise",
  "title": "Hello World Exercise",
  "description": "Practice using puts, print, and comments",
  "starterCode": "# Your code here\n",
  "tests": [
    {
      "name": "test_name",
      "call": "function_call()",
      "expected": "expected_result"
    }
  ],
  "hints": [
    "First hint",
    "Second hint"
  ]
}
```

### Solution Files (JSON)

Located in `solutions/`, provide:

```json
{
  "exerciseId": "hello_world_exercise",
  "languageId": "ruby",
  "code": "# Complete solution code",
  "explanation": "Detailed explanation of the solution",
  "keyPoints": [
    "Important concept 1",
    "Important concept 2"
  ]
}
```

## Usage

### Starting a Learning Session

```javascript
const Learn = require('./learn');
const learn = new Learn(context, vscode);

// Start learning a language
await learn.startLearning('ruby');
```

### Running an Exercise

```javascript
// Run the current exercise
const result = await learn.runExercise('ruby', exercise);

if (result.passed) {
  console.log(`✅ All tests passed! Score: ${result.score}/${result.total}`);
} else {
  console.log(`❌ ${result.failedTests} tests failed`);
}
```

### Tracking Progress

```javascript
// Get current progress
const progress = await learn.progressTracker.getProgress('ruby');

console.log(`Completed: ${progress.completed.length} lessons`);
console.log(`Streak: ${progress.streakDays} days`);
console.log(`Achievements: ${progress.achievements.length}`);

// Get statistics
const stats = await learn.getStats('ruby');
```

## Testing

The Learn module is thoroughly tested with Ruby tests:

```bash
ruby TEST_Suite/test_learn_module.rb
```

Tests verify:

- ✅ Module structure (27 tests)
- ✅ Curriculum JSON validity
- ✅ Lesson file existence and content
- ✅ Exercise structure and tests
- ✅ Solution validity
- ✅ Progression logic
- ✅ Achievement system

## Supported Languages

### Currently Implemented

- **Ruby** (Complete curriculum with 6 modules, 18 lessons)

### Planned

- Python
- JavaScript
- Java
- C++
- Go
- Rust

## Adding a New Language

1. **Create curriculum structure:**

   ```bash
   mkdir -p learn/curriculum/{language}/lessons
   mkdir -p learn/curriculum/{language}/exercises
   mkdir -p learn/curriculum/{language}/solutions
   ```

2. **Create curriculum.json:**
   - Define modules and lessons
   - Set difficulty levels and durations
   - Specify learning outcomes

3. **Write lesson files:**
   - Create Markdown files for each lesson
   - Include examples and explanations
   - Add practice exercises

4. **Define exercises:**
   - Create JSON files with test cases
   - Provide starter code
   - Add multiple hints

5. **Write solutions:**
   - Complete solution code
   - Detailed explanations
   - Key learning points

6. **Add tests:**
   - Create test file in TEST_Suite/
   - Verify curriculum structure
   - Test exercise execution

## Best Practices

### Curriculum Design

- **Progressive Difficulty**: Start simple, build complexity gradually
- **Hands-on Learning**: Every lesson should have practical exercises
- **Real-world Examples**: Use realistic scenarios, not just toy problems
- **Spaced Repetition**: Revisit concepts in later lessons
- **Clear Outcomes**: Define what learners will achieve

### Lesson Writing

- Keep lessons focused on one concept
- Use code examples extensively
- Provide clear explanations
- Include common pitfalls and tips
- End with key takeaways

### Exercise Design

- Start with simple tests, increase complexity
- Provide multiple hints (3-5 per exercise)
- Include edge cases in tests
- Give immediate, actionable feedback
- Offer detailed solution explanations

### Testing Strategy

- Test all JSON files are valid
- Verify lesson files exist
- Check solution code actually works
- Ensure progression is logical
- Validate achievement triggers

## Integration with TSI Header

The Learn mode integrates with TSI Header extension:

1. **Tree View**: Accessible from Activity Bar
2. **Commands**: Registered in extension.js
3. **State Persistence**: Uses VS Code's globalState
4. **UI Components**: Webview panels for lessons
5. **File Generation**: Creates exercise files in workspace

## Performance Considerations

- **Curriculum Caching**: Loaded once, cached in memory
- **Lazy Loading**: Lessons loaded only when opened
- **Async Operations**: All I/O operations are async
- **Resource Cleanup**: Proper disposal of webview panels
- **Test Isolation**: Temporary files for exercise testing

## Future Enhancements

- [ ] Video lessons integration
- [ ] Peer code review system
- [ ] Live coding challenges
- [ ] Multiplayer competitions
- [ ] AI-powered personalized hints
- [ ] Integration with GitHub Copilot
- [ ] Mobile app companion
- [ ] Community-contributed content
- [ ] Certificate generation
- [ ] LMS (Learning Management System) integration

## Contributing

To contribute to Learn Mode:

1. Fork the repository
2. Create a feature branch
3. Add your language curriculum
4. Write comprehensive tests
5. Update documentation
6. Submit a pull request

See `.github/copilot-instructions.md` for detailed development guidelines.

## License

Part of TSI Header extension - Licensed under the same terms.

## Credits

Learn Mode designed and implemented for Transport and Telecommunication Institute (TSI), Riga, Latvia.

---

**Remember**: The best way to learn programming is by writing code. Start your learning journey today! 🚀
