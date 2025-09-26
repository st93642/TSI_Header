const vscode = require('vscode');

class TSITreeDataProvider {
    constructor() {
        this._onDidChangeTreeData = new vscode.EventEmitter();
        this.onDidChangeTreeData = this._onDidChangeTreeData.event;
    }

    refresh() {
        this._onDidChangeTreeData.fire();
    }

    getTreeItem(element) {
        return element;
    }

    getChildren(element) {
        if (!element) {
            // Root level - return main categories
            return Promise.resolve([
                new TSITreeItem('Insert Header', vscode.TreeItemCollapsibleState.None, {
                    command: 'tsiheader.insertHeader',
                    title: 'Insert Header',
                    tooltip: 'Insert TSI header to current file'
                }),
                new TSITreeItem('Update Header', vscode.TreeItemCollapsibleState.None, {
                    command: 'tsiheader.updateHeader',
                    title: 'Update Header',
                    tooltip: 'Update existing TSI header'
                }),
                new TSITreeItem('Remove Header', vscode.TreeItemCollapsibleState.None, {
                    command: 'tsiheader.removeHeader',
                    title: 'Remove Header',
                    tooltip: 'Remove TSI header from current file'
                }),
                new TSITreeItem('Add Class', vscode.TreeItemCollapsibleState.None, {
                    command: 'tsiheader.addClass',
                    title: 'Add Class',
                    tooltip: 'Add class with TSI header'
                }),
                new TSITreeItem('Add Code Base', vscode.TreeItemCollapsibleState.None, {
                    command: 'tsiheader.addCodeBase',
                    title: 'Add Code Base',
                    tooltip: 'Add code base with TSI header'
                }),
                new TSITreeItem('Create TSI Project', vscode.TreeItemCollapsibleState.None, {
                    command: 'tsiheader.createTSIProject',
                    title: 'Create TSI Project',
                    tooltip: 'Create new TSI project with professional structure'
                })
            ]);
        }
        return Promise.resolve([]);
    }
}

class TSIProjectDataProvider {
    constructor() {
        this._onDidChangeTreeData = new vscode.EventEmitter();
        this.onDidChangeTreeData = this._onDidChangeTreeData.event;
    }

    refresh() {
        this._onDidChangeTreeData.fire();
    }

    getTreeItem(element) {
        return element;
    }

    getChildren(element) {
        if (!element) {
            // Root level
            return Promise.resolve([
                new TSITreeItem('🚀 Create New Project', vscode.TreeItemCollapsibleState.None, {
                    command: 'tsiheader.createTSIProject',
                    title: 'Create TSI Project',
                    tooltip: 'Create new TSI project with professional structure'
                }),
                new TSITreeItem('📋 Recent Projects', vscode.TreeItemCollapsibleState.Collapsed),
                new TSITreeItem('⚙️ Project Templates', vscode.TreeItemCollapsibleState.Collapsed)
            ]);
        } else if (element.label === '📋 Recent Projects') {
            // Could add logic to show recent TSI projects
            return Promise.resolve([
                new TSITreeItem('No recent projects', vscode.TreeItemCollapsibleState.None)
            ]);
        } else if (element.label === '⚙️ Project Templates') {
            return Promise.resolve([
                new TSITreeItem('C/C++ Project', vscode.TreeItemCollapsibleState.None, {
                    command: 'tsiheader.createTSIProject',
                    title: 'Create C++ TSI Project',
                    tooltip: 'Create C++ project with TSI headers and BaseClass'
                }),
                new TSITreeItem('C Project', vscode.TreeItemCollapsibleState.None, {
                    command: 'tsiheader.createTSIProject',
                    title: 'Create C TSI Project',
                    tooltip: 'Create C project with TSI headers'
                })
            ]);
        }
        return Promise.resolve([]);
    }
}

class TSITreeItem extends vscode.TreeItem {
    constructor(label, collapsibleState, command = null) {
        super(label, collapsibleState);
        this.tooltip = command?.tooltip || label;
        this.description = '';
        
        if (command) {
            this.command = command;
        }

        // Add icons based on the command
        if (command?.command === 'tsiheader.insertHeader') {
            this.iconPath = new vscode.ThemeIcon('add');
        } else if (command?.command === 'tsiheader.updateHeader') {
            this.iconPath = new vscode.ThemeIcon('sync');
        } else if (command?.command === 'tsiheader.removeHeader') {
            this.iconPath = new vscode.ThemeIcon('trash');
        } else if (command?.command === 'tsiheader.addClass') {
            this.iconPath = new vscode.ThemeIcon('symbol-class');
        } else if (command?.command === 'tsiheader.addCodeBase') {
            this.iconPath = new vscode.ThemeIcon('code');
        } else if (command?.command === 'tsiheader.createTSIProject') {
            this.iconPath = new vscode.ThemeIcon('folder-opened');
        } else if (label.includes('🚀')) {
            this.iconPath = new vscode.ThemeIcon('rocket');
        } else if (label.includes('📋')) {
            this.iconPath = new vscode.ThemeIcon('history');
        } else if (label.includes('⚙️')) {
            this.iconPath = new vscode.ThemeIcon('gear');
        } else if (label.includes('C++') || label.includes('C/C++')) {
            this.iconPath = new vscode.ThemeIcon('file-code');
        }
    }
}

module.exports = {
    TSITreeDataProvider,
    TSIProjectDataProvider
};