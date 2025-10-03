const fs = require('fs');
const path = require('path');

function loadExercises(language) {
    const exercisesDir = path.join(__dirname, '..', 'curriculum', language, 'exercises');
    return fs.readdirSync(exercisesDir)
        .filter(file => file.endsWith('.json'))
        .map(file => ({
            file,
            fullPath: path.join(exercisesDir, file),
            data: JSON.parse(fs.readFileSync(path.join(exercisesDir, file), 'utf8'))
        }));
}

function validateDescription(exercise) {
    const description = (exercise.data.description || '').trim();
    const issues = [];

    if (exercise.data.mode === 'quiz') {
        return issues;
    }

    if (!description.includes('Steps:\n')) {
        issues.push('missing "Steps:" section');
    }

    if (!/expected (output|results)/i.test(description)) {
        issues.push('missing explicit "Expected output/results" reference');
    }

    return issues;
}

(function main() {
    const languages = ['cpp', 'ruby', 'c'];
    const failures = [];

    for (const language of languages) {
        const exercises = loadExercises(language);
        for (const exercise of exercises) {
            const issues = validateDescription(exercise);
            if (issues.length > 0) {
                failures.push(`${language}/${exercise.file}: ${issues.join(', ')}`);
            }
        }
    }

    if (failures.length > 0) {
        console.error('Exercise description audit failed:\n' + failures.join('\n'));
        process.exit(1);
    }

    console.log('All exercise descriptions contain required guidance.');
})();
