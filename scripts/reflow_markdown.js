/*
Simple Markdown reflow script.
- Reads all files under learn/curriculum/ruby/lessons
- Wraps prose paragraphs to 80 chars
- Skips code fences (```), HTML blocks (<table>...</table>), and regions
  wrapped by <!-- markdownlint-disable ... --> ... <!-- markdownlint-enable ... -->

Note: conservative approach to avoid changing code blocks or HTML.
*/

const fs = require('fs');
const path = require('path');

const lessonsDir = path.join(__dirname, '..', 'learn', 'curriculum', 'ruby', 'lessons');
const wrapCol = 80;

function wrapText(text, width) {
  const words = text.split(/\s+/);
  let line = '';
  const out = [];
  for (const w of words) {
    if (!w) continue;
    if ((line + ' ' + w).trim().length <= width) {
      line = (line + ' ' + w).trim();
    } else {
      if (line) out.push(line);
      line = w;
    }
  }
  if (line) out.push(line);
  return out.join('\n');
}

function processFile(filePath) {
  const raw = fs.readFileSync(filePath, 'utf8');
  const lines = raw.split(/\r?\n/);
  let inCode = false;
  let inRawHtml = false; // for <table> blocks
  let inDisableRegion = false;
  // appendix handling: when we see a heading containing "appendix" we will
  // wrap the whole appendix section with a scoped markdownlint disable for
  // MD013 so long appendix rows or reference dumps aren't auto-wrapped.
  let inAppendix = false;
  let appendixLevel = 0;
  const out = [];
  let para = [];

  function flushPara() {
    if (para.length === 0) return;
    // join with spaces and wrap
    const joined = para.join(' ').replace(/\s+/g, ' ').trim();
    if (joined.length === 0) {
      out.push('');
      para = [];
      return;
    }
    // Heuristics: if the paragraph contains inline code, URLs, HTML tags,
    // or an unusually long token, avoid wrapping it to keep semantics intact.
    const shouldSkipWrap = /`|https?:\/\/|www\.|<[^>]+>|\b[A-Za-z0-9_\/.-]{45,}\b/.test(joined);
    if (shouldSkipWrap) {
      // Push as a single paragraph line (preserve content) to avoid breaking
      // code spans, long table-like rows, or long URLs.
      out.push(joined);
      para = [];
      return;
    }

    const wrapped = wrapText(joined, wrapCol);
    out.push(...wrapped.split('\n'));
    para = [];
  }

  for (let i = 0; i < lines.length; i++) {
    const line = lines[i];
    // detect code fence
    if (/^```/.test(line)) {
      // flush paragraph before code fence
      flushPara();
      inCode = !inCode;
      out.push(line);
      continue;
    }
    // detect markdownlint-disable/enable regions
    if (/<!--\s*markdownlint-disable/.test(line)) {
      flushPara();
      inDisableRegion = true;
      out.push(line);
      continue;
    }
    if (/<!--\s*markdownlint-enable/.test(line)) {
      inDisableRegion = false;
      out.push(line);
      continue;
    }
    // detect start/end of table HTML
    if (/^\s*<table\b/.test(line)) {
      flushPara();
      inRawHtml = true;
      out.push(line);
      continue;
    }
    if (inRawHtml) {
      out.push(line);
      if (/^\s*<\/table>/.test(line)) {
        inRawHtml = false;
      }
      continue;
    }
    if (inCode || inDisableRegion) {
      out.push(line);
      continue;
    }
    // blank lines separate paragraphs
    if (/^\s*$/.test(line)) {
      flushPara();
      out.push('');
      continue;
    }
    // headings, blockquotes, hr lines should flush paragraph
    // We handle appendix sections specially: a heading that contains the
    // word "appendix" (case-insensitive) will start an appendix region that
    // we protect from wrapping by inserting a scoped markdownlint-disable for
    // MD013. The appendix ends when a subsequent heading at the same or
    // higher level appears.
    const headingMatch = line.match(/^\s{0,3}(#{1,6})\s+(.*)$/);
    if (headingMatch) {
      const level = headingMatch[1].length;
      const title = headingMatch[2] || '';
      const isAppendixStart = /appendix/i.test(title);
      // If we're entering an appendix section, insert a disable marker
      if (isAppendixStart && !inDisableRegion && !inAppendix) {
        flushPara();
        out.push('<!-- markdownlint-disable MD013 -->');
        inAppendix = true;
        appendixLevel = level;
        out.push(line);
        continue;
      }
      // If we're currently in an appendix and encounter a heading at the
      // same-or-higher level, close the appendix region first.
      if (inAppendix && !inDisableRegion && level <= appendixLevel) {
        flushPara();
        out.push('<!-- markdownlint-enable MD013 -->');
        inAppendix = false;
      }
      flushPara();
      out.push(line);
      continue;
    }

    // List items: wrap the text after the marker, collect continuation
    // lines that are indented and part of the same list item.
    const listMatch = line.match(/^(\s{0,3})([-*+]|\d+\.)\s+(.*)$/);
    if (listMatch) {
      flushPara();
      const indent = listMatch[1] || '';
      const marker = listMatch[2];
      let rest = listMatch[3] || '';
      const contLines = [];
      // gather continuation lines
      let j = i + 1;
      for (; j < lines.length; j++) {
        const next = lines[j];
        if (/^\s*$/.test(next)) break;
        // stop if next is a new list item at same or lesser indent
        if (/^\s{0,3}([-*+]|\d+\.)\s+/.test(next)) break;
        // stop on headings or code fences
        if (/^\s{0,3}([#>`]|```)/.test(next)) break;
        // if it's indented (continuation), include its trimmed content
        const trimmed = next.trim();
        contLines.push(trimmed);
      }
      if (contLines.length) {
        rest = [rest].concat(contLines).join(' ');
      }
      // wrap the rest to the width minus marker area
      const markerArea = indent + marker + ' ';
      const wrapWidth = wrapCol - markerArea.length;
      const wrapped = wrapText(rest, wrapWidth).split('\n');
      // first line with marker
      out.push(markerArea + (wrapped[0] || ''));
      // continuation lines aligned under text
      const contPrefix = ' '.repeat(markerArea.length);
      for (let k = 1; k < wrapped.length; k++) {
        out.push(contPrefix + wrapped[k]);
      }
      // advance i to j-1
      i = j - 1;
      continue;
    }
    // inline HTML start with < and not part of paragraph - flush and copy until blank or tag close
    if (/^\s*</.test(line)) {
      flushPara();
      out.push(line);
      continue;
    }

    // otherwise part of paragraph
    para.push(line.trim());
  }
  flushPara();

  const outText = out.join('\n');
  if (outText !== raw) {
    fs.writeFileSync(filePath, outText, 'utf8');
    return true;
  }
  return false;
}

function walk(dir) {
  const files = fs.readdirSync(dir);
  const mdFiles = [];
  for (const f of files) {
    const p = path.join(dir, f);
    const st = fs.statSync(p);
    if (st.isDirectory()) {
      mdFiles.push(...walk(p));
    } else if (p.endsWith('.md')) {
      mdFiles.push(p);
    }
  }
  return mdFiles;
}

const files = walk(lessonsDir);
let changed = 0;
for (const f of files) {
  try {
    const did = processFile(f);
    if (did) changed++;
  } catch (err) {
    console.error('Error processing', f, err.message);
  }
}
console.log('Processed', files.length, 'files, changed', changed);
process.exit(0);
