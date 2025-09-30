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
                }),
                new TSITreeItem('🍅 Study Mode', vscode.TreeItemCollapsibleState.Collapsed, null, 'study-mode')
            ]);
        } else if (element.id === 'study-mode') {
            // Study Mode commands
            return Promise.resolve([
                new TSITreeItem('Start Study Session', vscode.TreeItemCollapsibleState.None, {
                    command: 'tsiheader.startStudySession',
                    title: 'Start Study Session',
                    tooltip: 'Begin a 25-minute focused coding session'
                }),
                new TSITreeItem('Pause/Resume Timer', vscode.TreeItemCollapsibleState.None, {
                    command: 'tsiheader.pauseStudyTimer',
                    title: 'Pause/Resume Study Timer',
                    tooltip: 'Pause or resume the current study session'
                }),
                new TSITreeItem('Stop Study Session', vscode.TreeItemCollapsibleState.None, {
                    command: 'tsiheader.stopStudySession',
                    title: 'Stop Study Session',
                    tooltip: 'End the current study session'
                }),
                new TSITreeItem('View Statistics', vscode.TreeItemCollapsibleState.None, {
                    command: 'tsiheader.viewStudyStats',
                    title: 'View Study Statistics',
                    tooltip: 'View your study session statistics and progress'
                }),
                new TSITreeItem('Configure Study Mode', vscode.TreeItemCollapsibleState.None, {
                    command: 'tsiheader.configureStudyMode',
                    title: 'Configure Study Mode',
                    tooltip: 'Configure timer durations and preferences'
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
                    command: 'tsiheader.createCppProject',
                    title: 'Create C++ TSI Project',
                    tooltip: 'Create C++ project with TSI headers and BaseClass'
                }),
                new TSITreeItem('C Project', vscode.TreeItemCollapsibleState.None, {
                    command: 'tsiheader.createCProject',
                    title: 'Create C TSI Project',
                    tooltip: 'Create C project with TSI headers'
                }),
                new TSITreeItem('Python Project', vscode.TreeItemCollapsibleState.None, {
                    command: 'tsiheader.createPythonProject',
                    title: 'Create Python TSI Project',
                    tooltip: 'Create Python project with TSI headers and BaseClass'
                }),
                new TSITreeItem('Java Project', vscode.TreeItemCollapsibleState.None, {
                    command: 'tsiheader.createJavaProject',
                    title: 'Create Java TSI Project',
                    tooltip: 'Create Java project with TSI headers and BaseClass'
                }),
                new TSITreeItem('Rust Project', vscode.TreeItemCollapsibleState.None, {
                    command: 'tsiheader.createRustProject',
                    title: 'Create Rust TSI Project',
                    tooltip: 'Create Rust project with TSI headers and BaseClass'
                }),
                new TSITreeItem('Ruby Project', vscode.TreeItemCollapsibleState.None, {
                    command: 'tsiheader.createRubyProject',
                    title: 'Create Ruby TSI Project',
                    tooltip: 'Create Ruby project with TSI headers and BaseClass'
                }),
                new TSITreeItem('PHP Project', vscode.TreeItemCollapsibleState.None, {
                    command: 'tsiheader.createPhpProject',
                    title: 'Create PHP TSI Project',
                    tooltip: 'Create PHP project with TSI headers and BaseClass'
                }),
                new TSITreeItem('HTML Project', vscode.TreeItemCollapsibleState.None, {
                    command: 'tsiheader.createHtmlProject',
                    title: 'Create HTML TSI Project',
                    tooltip: 'Create HTML project with TSI headers and modern web tools'
                })
            ]);
        }
        return Promise.resolve([]);
    }
}

class TSITreeItem extends vscode.TreeItem {
    constructor(label, collapsibleState, command = null, id = null) {
        super(label, collapsibleState);
        this.tooltip = command?.tooltip || label;
        this.description = '';
        this.id = id;
        
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
        } else if (label.includes('C Project')) {
            this.iconPath = new vscode.ThemeIcon('file-code');
        } else if (label.includes('Python')) {
            this.iconPath = new vscode.ThemeIcon('snake');
        } else if (label.includes('Java')) {
            this.iconPath = new vscode.ThemeIcon('coffee');
        } else if (label.includes('Rust')) {
            this.iconPath = new vscode.ThemeIcon('tools');
        } else if (label.includes('Ruby')) {
            this.iconPath = new vscode.ThemeIcon('ruby');
        } else if (label.includes('PHP')) {
            this.iconPath = new vscode.ThemeIcon('globe');
        } else if (label.includes('HTML')) {
            this.iconPath = new vscode.ThemeIcon('globe');
        } else if (command?.command === 'tsiheader.startStudySession') {
            this.iconPath = new vscode.ThemeIcon('play');
        } else if (command?.command === 'tsiheader.pauseStudyTimer') {
            this.iconPath = new vscode.ThemeIcon('debug-pause');
        } else if (command?.command === 'tsiheader.stopStudySession') {
            this.iconPath = new vscode.ThemeIcon('stop');
        } else if (command?.command === 'tsiheader.viewStudyStats') {
            this.iconPath = new vscode.ThemeIcon('graph');
        } else if (command?.command === 'tsiheader.configureStudyMode') {
            this.iconPath = new vscode.ThemeIcon('settings-gear');
        } else if (label.includes('🍅')) {
            this.iconPath = new vscode.ThemeIcon('clock');
        }
    }
}

module.exports = {
    TSITreeDataProvider,
    TSIProjectDataProvider
};