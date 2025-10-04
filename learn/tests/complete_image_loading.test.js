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

  // Mock vscode objects
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
    asWebviewUri: (uri) => {
      // Proper webview URI conversion
      const filePath = uri.fsPath;
      return `vscode-webview://mock-extension${filePath}`;
    }
  };
}

(function run() {
  const manager = createManager();
  const webview = createWebviewStub();

  // Test the complete image processing pipeline with proper webview URIs
  const baseDirectory = '/home/altin/Desktop/TSI_Header/learn/curriculum/dsa_cpp/lessons';
  const resolver = manager.createResourceResolver(webview, baseDirectory);

  // Test resolving the image paths
  const treeResolved = resolver('../../../../resources/dsa_cpp/tree_structure.svg');
  const bstResolved = resolver('../../../../resources/dsa_cpp/bst_operations.svg');
  const appResolved = resolver('../../../../resources/dsa_cpp/tree_applications.svg');

  console.log('Tree resolved:', treeResolved);
  console.log('BST resolved:', bstResolved);
  console.log('App resolved:', appResolved);

  // Should be webview URIs
  assert.ok(treeResolved.startsWith('vscode-webview://'), 'Tree image should be converted to webview URI');
  assert.ok(bstResolved.startsWith('vscode-webview://'), 'BST image should be converted to webview URI');
  assert.ok(appResolved.startsWith('vscode-webview://'), 'App image should be converted to webview URI');

  // Should contain the correct file paths
  assert.ok(treeResolved.includes('/home/altin/Desktop/TSI_Header/resources/dsa_cpp/tree_structure.svg'), 'Tree URI should contain correct path');
  assert.ok(bstResolved.includes('/home/altin/Desktop/TSI_Header/resources/dsa_cpp/bst_operations.svg'), 'BST URI should contain correct path');
  assert.ok(appResolved.includes('/home/altin/Desktop/TSI_Header/resources/dsa_cpp/tree_applications.svg'), 'App URI should contain correct path');

  // Test markdown with image references
  const markdown = `# Trees Lesson

![Tree Structure](../../../../resources/dsa_cpp/tree_structure.svg)

## BST Operations

![BST Operations](../../../../resources/dsa_cpp/bst_operations.svg)

## Tree Applications

![Tree Applications](../../../../resources/dsa_cpp/tree_applications.svg)
`;

  // Convert markdown to HTML
  let html = manager.markdownToHtml(markdown);

  // Apply image source rewriting
  html = manager.rewriteImageSources(html, resolver);

  // Check that all images are properly rewritten to webview URIs
  assert.ok(html.includes('vscode-webview://mock-extension/home/altin/Desktop/TSI_Header/resources/dsa_cpp/tree_structure.svg'), 'Tree structure image should be rewritten to webview URI');
  assert.ok(html.includes('vscode-webview://mock-extension/home/altin/Desktop/TSI_Header/resources/dsa_cpp/bst_operations.svg'), 'BST operations image should be rewritten to webview URI');
  assert.ok(html.includes('vscode-webview://mock-extension/home/altin/Desktop/TSI_Header/resources/dsa_cpp/tree_applications.svg'), 'Tree applications image should be rewritten to webview URI');

  console.log('✅ Complete image loading test passed.');
})();