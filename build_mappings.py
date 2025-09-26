#!/usr/bin/env python3
"""Build mappings between file extensions and languages.

Creates:
 - extension_to_linguist.csv  (extension, linguist_language)
 - extension_to_vscode.csv    (extension, linguist_language, vscode_language_id)

Downloads:
 - GitHub Linguist languages.yml
 - VS Code "Known language identifiers" docs page

Requires: PyYAML
"""
import re
import sys
import csv
from urllib.request import urlopen

LINGUIST_URL = 'https://raw.githubusercontent.com/github/linguist/master/lib/linguist/languages.yml'
VSCODE_URL = 'https://code.visualstudio.com/docs/languages/identifiers'
LOCAL_VSCODE_FILE = '/home/altin/CLionProjects/tests/generated/vscode_recognized_languages.txt'


def norm_key(s: str) -> str:
    if not s:
        return ''
    s = s.lower()
    # common rewrites
    s = s.replace('c++', 'cpp')
    s = s.replace('c#', 'csharp')
    s = s.replace('f#', 'fsharp')
    s = s.replace('objective-c', 'objectivec')
    s = s.replace('objective cpp', 'objectivecpp')
    s = s.replace(' ', '')
    # remove non-alphanum
    s = re.sub(r'[^0-9a-z]', '', s)
    return s


def fetch_text(url: str) -> str:
    with urlopen(url) as r:
        data = r.read()
        try:
            return data.decode('utf-8')
        except Exception:
            return data.decode('latin1')


def load_linguist(yml_text: str):
    try:
        import yaml
    except Exception as e:
        print('PyYAML not installed. Please install via `pip install pyyaml`.', file=sys.stderr)
        raise
    data = yaml.safe_load(yml_text)
    return data


def parse_vscode_ids(page_text: str):
    # find markdown-like table rows: | Display Name | id |
    vscode_map = {}
    for line in page_text.splitlines():
        m = re.match(r"^\|\s*([^|]+?)\s*\|\s*([^|]+?)\s*\|", line)
        if m:
            display = m.group(1).strip()
            id_ = m.group(2).strip()
            # some rows may be headers or empty
            if display and id_ and display.lower() not in ('', '---'):
                key = norm_key(display)
                vscode_map[key] = id_
                # also map the id itself
                vscode_map[norm_key(id_)] = id_
    return vscode_map


def load_local_vscode_list(path: str):
    """Load the simple list of vscode ids from the provided local file.

    The file `vscode_recognized_languages.txt` contains a list of ids (one per line)
    after a short header. We'll read lines and accept simple tokens that look like ids.
    """
    m = {}
    try:
        with open(path, 'r', encoding='utf-8') as f:
            started = False
            for line in f:
                line = line.strip()
                if not line:
                    continue
                # detect the start of the representative list section
                if line.lower().startswith('representative list'):
                    started = True
                    continue
                if not started:
                    continue
                # stop if we hit another header
                if line.endswith(':'):
                    break
                # some lines may contain parentheses or extra text; take the token before any space
                token = line.split()[0]
                # only consider tokens that start with a dot-free name or common id pattern
                if token.startswith('.'):
                    continue
                key = norm_key(token)
                m[key] = token
    except FileNotFoundError:
        return {}
    return m


def main():
    print('Fetching Linguist YAML...')
    yml_text = fetch_text(LINGUIST_URL)
    print('Loading YAML (PyYAML)...')
    linguist = load_linguist(yml_text)

    print('Fetching VS Code identifiers page...')
    page_text = fetch_text(VSCODE_URL)
    vscode_map = parse_vscode_ids(page_text)
    print(f'Parsed {len(vscode_map)} vscode identifier keys (including id aliases)')
    if not vscode_map:
        print('No vscode ids parsed from remote page — falling back to local list if available')
        local_map = load_local_vscode_list(LOCAL_VSCODE_FILE)
        if local_map:
            vscode_map = local_map
            print(f'Loaded {len(vscode_map)} vscode ids from local file {LOCAL_VSCODE_FILE}')
        else:
            print('No local vscode id file found or parsed; continuing with empty map')

    rows = []
    for lang_name, meta in linguist.items():
        exts = meta.get('extensions') or []
        aliases = meta.get('aliases') or []
        for ext in exts:
            ext = ext.strip()
            if not ext:
                continue
            rows.append((ext, lang_name, aliases))

    print(f'Found {len(rows)} extension entries in Linguist')

    out1 = '/home/altin/CLionProjects/tests/generated/extension_to_linguist.csv'
    out2 = '/home/altin/CLionProjects/tests/generated/extension_to_vscode.csv'

    with open(out1, 'w', newline='', encoding='utf-8') as f1:
        w = csv.writer(f1)
        w.writerow(['extension', 'linguist_language'])
        for ext, lang, _aliases in rows:
            w.writerow([ext, lang])

    with open(out2, 'w', newline='', encoding='utf-8') as f2:
        w = csv.writer(f2)
        w.writerow(['extension', 'linguist_language', 'vscode_language_id'])
        matched = 0
        for ext, lang, aliases in rows:
            # try to find a vscode id by matching lang name or aliases
            candidates = [lang] + list(aliases)
            found = ''
            for c in candidates:
                key = norm_key(c)
                if key in vscode_map:
                    found = vscode_map[key]
                    break
            # also try normalized lang with some heuristics
            if not found:
                key2 = norm_key(lang)
                if key2 in vscode_map:
                    found = vscode_map[key2]
            if found:
                matched += 1
            w.writerow([ext, lang, found])

    print(f'Wrote {out1} and {out2}. Matched {matched} extensions to VS Code ids.')


if __name__ == '__main__':
    main()
