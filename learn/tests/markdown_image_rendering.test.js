const assert = require('assert');
const LearnManager = require('../lib/learn_manager');

function createManager() {
  const contextStub = {
    subscriptions: [],
    workspaceState: {
      update: async () => {}
    },
    globalState: {
      get: () => ({}),
      update: async () => {}
    }
  };

  const vscodeStub = {
    window: {
      tabGroups: { all: [] }
    }
  };

  return new LearnManager(contextStub, vscodeStub, {});
}

(function run() {
  const manager = createManager();
  const markdown = '# Visualising Structures\n\n![Binary Tree](../assets/binary-tree.png)\n\n![Remote Chart](https://example.com/chart.png)';

  const html = manager.getLessonHtml(markdown, { id: 'data_structures_cpp' }, {
    resolveResourceUri: (source) => {
      if (!source || source.startsWith('http')) {
        return source;
      }
      return `webview://${source}`;
    }
  });

  assert.ok(html.includes('<img src="webview://../assets/binary-tree.png" alt="Binary Tree"'), 'Relative image path should be rewritten via resolver');
  assert.ok(html.includes('<img src="https://example.com/chart.png" alt="Remote Chart"'), 'Remote image URL should be preserved');

  console.log('Markdown image rendering test passed.');
})();
