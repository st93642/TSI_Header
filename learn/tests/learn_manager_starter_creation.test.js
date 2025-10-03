const fs = require('fs');
const fsp = fs.promises;
const os = require('os');
const path = require('path');
const LearnManager = require('../lib/learn_manager');

function createContext() {
  const store = new Map();
  return {
    subscriptions: [],
    workspaceState: {
      get(key, defaultValue = null) {
        return store.has(key) ? store.get(key) : defaultValue;
      },
      async update(key, value) {
        store.set(key, value);
      }
    }
  };
}

function createVscodeStub(tempRoot) {
  return {
    workspace: {
      workspaceFolders: [{ uri: { fsPath: tempRoot } }],
      async openTextDocument(filePath) {
        return {
          uri: { fsPath: filePath },
          getText: () => fs.readFileSync(filePath, 'utf8')
        };
      }
    },
    window: {
      tabGroups: { all: [] },
      async showTextDocument(doc) {
        return {
          document: doc,
          edit: async (callback) => {
            const builder = {
              insert: () => {}
            };
            await callback(builder);
            return true;
          },
          revealRange: () => {},
          selection: null
        };
      },
      showInformationMessage: () => Promise.resolve(undefined),
      showErrorMessage: () => {}
    },
    commands: {
      executeCommand: () => {}
    },
    Position: class Position {
      constructor(line, character) {
        this.line = line;
        this.character = character;
      }
    },
    Selection: class Selection {
      constructor(start, end) {
        this.start = start;
        this.end = end;
      }
    },
    Range: class Range {
      constructor(start, end) {
        this.start = start;
        this.end = end;
      }
    }
  };
}

async function run() {
  const tempRoot = await fsp.mkdtemp(path.join(os.tmpdir(), 'tsi-learn-manager-'));
  const cleanup = async () => {
    await fsp.rm(tempRoot, { recursive: true, force: true });
  };

  try {
    const context = createContext();
    const vscodeStub = createVscodeStub(tempRoot);
    const manager = new LearnManager(context, vscodeStub);

    const lesson = {
      id: 'conditionals_c',
      sectionId: 'section',
      sectionTitle: 'Section',
      moduleId: 'module',
      moduleTitle: 'Module',
      difficulty: 'beginner'
    };

    const exerciseId = 'conditionals_c_exercise';
    const language = 'c';
    const exerciseJsonPath = path.join(__dirname, '..', 'curriculum', language, 'exercises', `${exerciseId}.json`);
    const exerciseData = JSON.parse(await fsp.readFile(exerciseJsonPath, 'utf8'));
    const expectedStarter = exerciseData.starterCode;

    await manager.startExercise(language, lesson, exerciseId);

    const writtenPath = path.join(tempRoot, 'learn_exercises', language, `${exerciseId}.c`);
    const exists = fs.existsSync(writtenPath);
    if (!exists) {
      throw new Error(`Starter file was not created at ${writtenPath}`);
    }

    const actualStarter = await fsp.readFile(writtenPath, 'utf8');
    if (actualStarter !== expectedStarter) {
      throw new Error('Starter file contents do not match exercise starter code.');
    }

    await cleanup();
    console.log('LearnManager starter creation test passed.');
  } catch (error) {
    try {
      await cleanup();
    } catch (_) {
      // ignore cleanup errors
    }
    console.error('LearnManager starter creation test failed:', error.message || error);
    process.exit(1);
  }
}

run();
