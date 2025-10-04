const assert = require('assert');
const LearnManager = require('../lib/learn_manager');

function createManager() {
  const contextStub = {
    subscriptions: [],
    workspaceState: {
      update: async () => {}
    },
    globalState: {
      get: () => ({})
    }
  };

  const vscodeStub = {
    window: {
      tabGroups: { all: [] }
    },
    Uri: {
      file: (filePath) => ({
        fsPath: filePath,
        toString: () => `file://${filePath}`
      })
    }
  };

  return new LearnManager(contextStub, vscodeStub, {});
}

function createWebviewStub() {
  return {
    asWebviewUri: (uri) => ({
      toString: () => `vscode-webview://mock-extension${uri.fsPath}`
    })
  };
}

(function run() {
  const manager = createManager();
  const webview = createWebviewStub();

  // Test the resource resolver
  const baseDirectory = '/home/altin/Desktop/TSI_Header/learn/curriculum/dsa_cpp/lessons';
  const resolver = manager.createResourceResolver(webview, baseDirectory);

  // Test resolving the tree_structure.svg path
  const resolvedPath = resolver('../../../../resources/dsa_cpp/tree_structure.svg');
  console.log('Resolved path:', resolvedPath);

  // Should resolve to the correct webview URI containing the file path
  const expectedPath = '/home/altin/Desktop/TSI_Header/resources/dsa_cpp/tree_structure.svg';
  assert.ok(
    resolvedPath.startsWith('vscode-webview://mock-extension'),
    'Resolved path should use a webview URI scheme'
  );
  assert.ok(
    resolvedPath.includes(expectedPath),
    `Expected path to contain ${expectedPath}, got ${resolvedPath}`
  );

  // Test with bst_operations.svg
  const bstResolved = resolver('../../../../resources/dsa_cpp/bst_operations.svg');
  console.log('BST resolved path:', bstResolved);
  assert.ok(bstResolved.startsWith('vscode-webview://mock-extension'), 'BST path should use webview URI scheme');
  assert.ok(bstResolved.includes('/home/altin/Desktop/TSI_Header/resources/dsa_cpp/bst_operations.svg'), 'BST path should resolve correctly');

  // Test with tree_applications.svg
  const appResolved = resolver('../../../../resources/dsa_cpp/tree_applications.svg');
  console.log('Applications resolved path:', appResolved);
  assert.ok(appResolved.startsWith('vscode-webview://mock-extension'), 'Applications path should use webview URI scheme');
  assert.ok(appResolved.includes('/home/altin/Desktop/TSI_Header/resources/dsa_cpp/tree_applications.svg'), 'Applications path should resolve correctly');

  console.log('✅ Resource resolver test passed.');
})();