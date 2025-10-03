const fs = require('fs');
const path = require('path');

function filesForLanguage(language) {
  const exercisesDir = path.join(__dirname, '..', 'curriculum', language, 'exercises');
  return fs.readdirSync(exercisesDir)
    .filter((file) => file.endsWith('.json'))
    .map((file) => ({
      language,
      file,
      fullPath: path.join(exercisesDir, file),
      raw: fs.readFileSync(path.join(exercisesDir, file), 'utf8'),
    }));
}

function lineAndColumn(rawText, index) {
  let line = 1;
  let column = 1;

  for (let i = 0; i < rawText.length && i < index; i += 1) {
    if (rawText[i] === '\n') {
      line += 1;
      column = 1;
    } else {
      column += 1;
    }
  }

  return { line, column };
}

function scanForUnescapedControlCharacters(rawText) {
  const issues = [];
  let inString = false;
  let escapeNext = false;

  for (let i = 0; i < rawText.length; i += 1) {
    const ch = rawText[i];

    if (escapeNext) {
      escapeNext = false;
      continue;
    }

    if (ch === '"') {
      inString = !inString;
      continue;
    }

    if (!inString) {
      if (ch === '\\') {
        escapeNext = true; // escape sequences outside strings are safe but skip next char
      }
      continue;
    }

    if (ch === '\\') {
      escapeNext = true;
      continue;
    }

    if (ch === '\n' || ch === '\r') {
      issues.push({ index: i, codePoint: ch === '\n' ? 'LF' : 'CR' });
    }
  }

  return issues;
}

(function main() {
  const languages = ['c', 'cpp', 'ruby'];
  const failures = [];

  languages
    .flatMap(filesForLanguage)
    .forEach(({ language, file, fullPath, raw }) => {
      const issues = scanForUnescapedControlCharacters(raw);
      if (issues.length > 0) {
        const messages = issues.map(({ index, codePoint }) => {
          const { line, column } = lineAndColumn(raw, index);
          return `${codePoint} at line ${line}, column ${column}`;
        });
        failures.push(`${language}/${file}: ${messages.join('; ')}`);
      }

      try {
        JSON.parse(raw);
      } catch (error) {
        failures.push(`${language}/${file}: JSON.parse failed -> ${error.message}`);
      }
    });

  if (failures.length > 0) {
    console.error('Exercise escape integrity test failed:\n' + failures.join('\n'));
    process.exit(1);
  }

  console.log('All exercise JSON files use escaped control characters.');
})();
