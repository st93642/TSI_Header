const fs = require('fs');
const path = require('path');

function listLessons() {
    const lessonsDir = path.join(__dirname, '..', 'curriculum', 'git', 'lessons');
    return fs.readdirSync(lessonsDir)
        .filter(file => file.endsWith('.md'))
        .map(file => ({
            name: file,
            fullPath: path.join(lessonsDir, file)
        }));
}

function measureLines(filePath) {
    const content = fs.readFileSync(filePath, 'utf8');
    return content.trimEnd().split(/\r?\n/).length;
}

(function main() {
    const requiredLines = 500;
    const failures = [];

    for (const lesson of listLessons()) {
        const lineCount = measureLines(lesson.fullPath);
        if (lineCount < requiredLines) {
            failures.push(`${lesson.name} is ${lineCount} lines; requires at least ${requiredLines}.`);
        }
    }

    if (failures.length > 0) {
        console.error(`Git lesson length audit failed:\n${failures.join('\n')}`);
        process.exit(1);
    }

    console.log('All Git lessons meet the minimum length requirement.');
})();
