const path = require('path');
const fs = require('fs').promises;
const { execSync } = require('child_process');

async function runSmoke() {
    const vscodeMock = {
        window: {
            activeTextEditor: null,
            showInformationMessage: () => {}
        }
    };
    const ExerciseRunner = require(path.join(__dirname, '..', 'learn', 'lib', 'exercise_runner.js'));
    const runner = new ExerciseRunner(vscodeMock);

    // Load sample exercise
    const exercisePath = path.join(__dirname, '..', 'learn', 'curriculum', 'rust', 'exercises', 'hello_world_exercise.json');
    const exercise = JSON.parse(await fs.readFile(exercisePath, 'utf8'));

    // Create a temporary editor file for the runner to pick up
    const tempCode = 'fn main() { println!("Hello, World!"); }\n';
    const tempFile = path.join(__dirname, '..', '.temp_tests', 'solution.rs');
    await fs.mkdir(path.dirname(tempFile), { recursive: true });
    await fs.writeFile(tempFile, tempCode);

    // Create a fake activeTextEditor with document getting text returning tempCode
    vscodeMock.window.activeTextEditor = {
        document: {
            getText: () => tempCode
        }
    };

    try {
        const result = await runner.run('rust', exercise);
        console.log('Smoke test result:', result);
    } catch (e) {
        console.error('Smoke test error:', e);
        process.exit(1);
    }
}

runSmoke();
