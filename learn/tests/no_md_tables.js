const fs = require('fs');
const path = require('path');

function walk(dir) {
  let results = [];
  const list = fs.readdirSync(dir);
  list.forEach(function(file) {
    file = path.resolve(dir, file);
    const stat = fs.statSync(file);
    if (stat && stat.isDirectory()) {
      results = results.concat(walk(file));
    } else {
      results.push(file);
    }
  });
  return results;
}

const root = path.resolve(__dirname, '..', 'curriculum');
const files = walk(root).filter(f => /\/lessons\/.*\.md$/.test(f));
const tableRegex = /\|\s*-{3,}\s*\|/; // matches | --- |
let found = [];

files.forEach(f => {
  const content = fs.readFileSync(f, 'utf8');
  const lines = content.split(/\r?\n/);
  lines.forEach((ln, idx) => {
    if (tableRegex.test(ln)) found.push({file: f, line: idx+1, text: ln.trim()});
  });
});

if (found.length === 0) {
  console.log('✅ No Markdown pipe-style table separators found in lesson files.');
  process.exit(0);
} else {
  console.error('❌ Found Markdown table separators in the following lesson files:');
  found.forEach(f => console.error(`${f.file}:${f.line}: ${f.text}`));
  process.exit(1);
}
