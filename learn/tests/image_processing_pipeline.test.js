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

  // Test the complete image processing pipeline
  const baseDirectory = '/home/altin/Desktop/TSI_Header/learn/curriculum/dsa_cpp/lessons';
  const resolver = manager.createResourceResolver(webview, baseDirectory);

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
  console.log('HTML before rewrite:', html);

  // Apply image source rewriting
  html = manager.rewriteImageSources(html, resolver);
  console.log('HTML after rewrite:', html);

  // Check that all images are properly rewritten
  assert.ok(html.includes('vscode-webview://mock-extension/home/altin/Desktop/TSI_Header/resources/dsa_cpp/tree_structure.svg'), 'Tree structure image should be rewritten');
  assert.ok(html.includes('vscode-webview://mock-extension/home/altin/Desktop/TSI_Header/resources/dsa_cpp/bst_operations.svg'), 'BST operations image should be rewritten');
  assert.ok(html.includes('vscode-webview://mock-extension/home/altin/Desktop/TSI_Header/resources/dsa_cpp/tree_applications.svg'), 'Tree applications image should be rewritten');

  // Check that the HTML contains img tags
  const imgTagCount = (html.match(/<img/g) || []).length;
  assert.equal(imgTagCount, 3, 'Should have exactly 3 img tags');

  console.log('✅ Image processing pipeline test passed.');
})();