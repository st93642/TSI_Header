const assert = require('assert');
const fs = require('fs');
const LearnManager = require('../lib/learn_manager');

async function run() {
  const contextStub = {
    subscriptions: [],
    workspaceState: {
      async update() {},
      get() { return null; }
    },
    globalState: {
      get: () => ({}),
      async update() {}
    }
  };

  const infoMessages = [];
  const warningMessages = [];

  const panelStub = {
    webview: {
      html: '',
      asWebviewUri: (uri) => ({ toString: () => `vscode-resource://${uri.fsPath}` }),
      onDidReceiveMessage(handler) {
        panelStub._messageHandler = handler;
        return { dispose() {} };
      },
      postMessage() {}
    },
    onDidDispose(handler) {
      panelStub._disposeHandler = handler;
      return { dispose() {} };
    },
    dispose() {
      panelStub._disposed = true;
      if (panelStub._disposeHandler) {
        panelStub._disposeHandler();
      }
    }
  };

  const resetPanelStub = () => {
    panelStub._disposed = false;
    panelStub.webview.html = '';
    panelStub._messageHandler = null;
    panelStub._disposeHandler = null;
  };
  resetPanelStub();

  const progressRecorder = [];
  const progressTrackerMock = {
    async recordCompletion(language, exerciseId, metadata) {
      progressRecorder.push({ language, exerciseId, metadata });
      return { completed: [exerciseId] };
    },
    async getProgress() {
      return { completed: [] };
    }
  };

  const vscodeStub = {
    Uri: {
      file: (fsPath) => ({ fsPath, toString() { return fsPath; } })
    },
    ViewColumn: { One: 1 },
    workspace: {
      workspaceFolders: [{ uri: { fsPath: '/tmp/workspace' } }]
    },
    window: {
      tabGroups: { all: [] },
      createWebviewPanel: () => {
        resetPanelStub();
        return panelStub;
      },
      showInformationMessage: (message) => {
        infoMessages.push(message);
        return {
          then: (callback) => {
            if (typeof callback === 'function') {
              callback(undefined);
            }
            return { catch: () => {} };
          }
        };
      },
      showErrorMessage: () => ({ then: () => {} }),
      showWarningMessage: (message) => {
        warningMessages.push(message);
        return {
          then: (callback) => {
            if (typeof callback === 'function') {
              callback(undefined);
            }
            return { catch: () => {} };
          }
        };
      }
    },
    commands: {
      executeCommand: () => {}
    }
  };

  const learnManager = new LearnManager(contextStub, vscodeStub, progressTrackerMock);
  const openLessonCalls = [];
  learnManager.openLesson = async (language, lesson) => {
    openLessonCalls.push({ language, lessonId: lesson.id });
  };

  const exerciseId = 'quiz_sample_exercise';
  const lessonMeta = {
    id: 'hello_world_cpp',
    title: 'Lesson 1.1: Building Hello World',
    sectionId: 'getting_started',
    sectionTitle: 'Module 1: Getting Started',
    difficulty: 'beginner'
  };

  const originalReadFile = fs.promises.readFile;
  const originalWriteFile = fs.promises.writeFile;
  const originalMkdir = fs.promises.mkdir;

  fs.promises.readFile = async (filePath, encoding) => {
    if (filePath.endsWith(`${exerciseId}.json`)) {
      return JSON.stringify({
        id: exerciseId,
        mode: 'quiz',
        title: 'Sample Quiz',
        description: 'Check your understanding of graph terminology.',
        questions: [
          {
            id: 'q1',
            prompt: 'Which structure ensures O(log n) search on average?',
            type: 'single',
            options: [
              { id: 'a', text: 'Linked List' },
              { id: 'b', text: 'Binary Search Tree', correct: true },
              { id: 'c', text: 'Hash Map' }
            ],
            explanation: 'Balanced BSTs maintain logarithmic height.'
          }
        ]
      });
    }
    return originalReadFile.call(fs.promises, filePath, encoding);
  };

  fs.promises.writeFile = async () => {
    throw new Error('Quiz exercises should not write starter files');
  };

  fs.promises.mkdir = async () => {
    throw new Error('Quiz exercises should not create starter directories');
  };

  try {
    await learnManager.startExercise('cpp', lessonMeta, exerciseId);

  assert.ok(/<h1>Sample Quiz<\/h1>/.test(panelStub.webview.html), 'Quiz webview should render quiz title heading');
    assert.strictEqual(typeof panelStub._messageHandler, 'function', 'Quiz webview should register message handler');

    await panelStub._messageHandler({ command: 'openNextLesson' });
    assert.strictEqual(openLessonCalls.length, 1, 'Next Lesson command should invoke openLesson');
    assert.strictEqual(openLessonCalls[0].lessonId, 'iostream_basics', 'Next Lesson should advance to the following curriculum entry');
    assert.ok(panelStub._disposed, 'Panel should dispose when navigating to the next lesson');
  assert.strictEqual(progressRecorder.length, 0, 'Skipping ahead should not record quiz completion');

    await learnManager.startExercise('cpp', lessonMeta, exerciseId);

    assert.ok(/<h1>Sample Quiz<\/h1>/.test(panelStub.webview.html), 'Quiz webview should render quiz title heading after reopening');
    assert.strictEqual(typeof panelStub._messageHandler, 'function', 'Quiz webview should register message handler after reopening');

    await panelStub._messageHandler({ command: 'submitQuiz', answers: { q1: 'b' } });

    assert.strictEqual(progressRecorder.length, 1, 'Quiz completion should be recorded');
    assert.ok(infoMessages.some((msg) => msg.includes('Sample Quiz')), 'Success message should reference quiz title');
  } finally {
    fs.promises.readFile = originalReadFile;
    fs.promises.writeFile = originalWriteFile;
    fs.promises.mkdir = originalMkdir;
  }

  console.log('Quiz exercise flow test passed.');
}

run().catch((error) => {
  console.error(error);
  process.exit(1);
});
