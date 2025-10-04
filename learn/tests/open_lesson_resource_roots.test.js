const assert = require('assert');
const path = require('path');
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

  const panelStub = {
    webview: {
      html: '',
      asWebviewUri: (uri) => ({ toString: () => `vscode-webview://mock${uri.fsPath}` }),
      onDidReceiveMessage(handler) {
        panelStub._messageHandler = handler;
        return { dispose() {} };
      }
    },
    onDidDispose(handler) {
      panelStub._disposeHandler = handler;
      return { dispose() {} };
    },
    dispose() {
      panelStub._disposed = true;
      if (typeof panelStub._disposeHandler === 'function') {
        panelStub._disposeHandler();
      }
    }
  };

  const errorMessages = [];
  const createPanelInvocations = [];

  const vscodeStub = {
    Uri: {
      file: (fsPath) => ({ fsPath, toString() { return fsPath; } })
    },
    ViewColumn: { One: 1 },
    window: {
      tabGroups: {
        all: [],
        async close() {}
      },
      createWebviewPanel: (viewType, title, column, options) => {
        createPanelInvocations.push({ viewType, title, column, options });
        return panelStub;
      },
      showErrorMessage: (message) => {
        errorMessages.push(message);
        return { then: () => ({ catch: () => {} }) };
      }
    }
  };

  const learnManager = new LearnManager(contextStub, vscodeStub, {});

  await learnManager.openLesson('dsa_cpp', {
    id: 'dsa_trees_cpp',
    title: 'Lesson 11: Trees'
  });

  assert.strictEqual(errorMessages.length, 0, 'Lesson open should not surface an error modal');
  assert.strictEqual(createPanelInvocations.length, 1, 'Lesson open should create a webview panel');

  const panelOptions = createPanelInvocations[0].options;
  assert.ok(panelOptions.localResourceRoots, 'Lesson webview should declare local resource roots');

  const rootPaths = panelOptions.localResourceRoots.map((uri) => uri.fsPath);
  const expectedResourceRoot = path.join(__dirname, '..', '..', 'resources');
  const expectedLessonDir = path.join(__dirname, '..', 'curriculum', 'dsa_cpp', 'lessons');

  assert.ok(rootPaths.includes(expectedResourceRoot), 'Resource roots should include the shared resources directory');
  assert.ok(rootPaths.includes(expectedLessonDir), 'Resource roots should include the lesson directory for relative assets');

  assert.ok(/<!DOCTYPE html>/i.test(panelStub.webview.html), 'Lesson webview HTML should be populated');

  console.log('Open lesson resource roots test passed.');
}

run().catch((error) => {
  console.error(error);
  process.exit(1);
});
