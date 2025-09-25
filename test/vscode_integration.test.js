const assert = require('assert');
const vscode = require('vscode');
const path = require('path');
const { TSITreeDataProvider, TSIProjectDataProvider } = require('../src/tsiViewProvider');

describe('TSI Header VS Code Extension Integration', function() {
  it('should activate extension and register commands', async function() {
    // Simulate VS Code extension activation
    const context = { subscriptions: [] };
    const extension = require('../src/extension');
    assert.ok(extension.activate);
    extension.activate(context);
    assert.ok(context.subscriptions.length > 0);
  });

  it('should provide TSI Tree View items', async function() {
    const provider = new TSITreeDataProvider();
    const items = await provider.getChildren();
    assert.ok(Array.isArray(items));
    assert.ok(items.some(item => item.label === 'Insert Header'));
  });

  it('should provide TSI Project View items', async function() {
    const provider = new TSIProjectDataProvider();
    const items = await provider.getChildren();
    assert.ok(Array.isArray(items));
    assert.ok(items.some(item => item.label.includes('Create TSI Project')));
  });
});
