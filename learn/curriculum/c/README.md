# C Curriculum Skeleton

This directory hosts the upcoming Learn Mode curriculum for the C programming language. The structure mirrors the existing Ruby and C++ tracks so new lessons, exercises, and solutions can be added incrementally.

## Layout

- `curriculum.json` – master manifest that lists modules, lessons, and metadata.
- `lessons/` – Markdown lesson content (`<lesson_id>.md`).
- `exercises/` – Exercise definitions in JSON (`<lesson_id>_exercise.json`).
- `solutions/` – Authoritative solutions for each exercise, also JSON (`<lesson_id>_exercise.json`).

Populate each folder as lessons are authored. Remember to keep lesson IDs, exercise IDs, and solution files in sync so the Learn manager can resolve them automatically.
