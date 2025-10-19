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
                new TSITreeItem('🍅 Study Mode', vscode.TreeItemCollapsibleState.Collapsed, null, 'study-mode'),
                new TSITreeItem('📚 Learn', vscode.TreeItemCollapsibleState.Collapsed, null, 'learn')
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
        } else if (element.id === 'learn') {
            // Learn section - programming language tutorials/resources
                return Promise.resolve([
                new TSITreeItem('Ruby', vscode.TreeItemCollapsibleState.Collapsed, null, 'learn-ruby'),
                new TSITreeItem('Rust', vscode.TreeItemCollapsibleState.Collapsed, null, 'learn-rust'),
                new TSITreeItem('C', vscode.TreeItemCollapsibleState.Collapsed, null, 'learn-c'),
                new TSITreeItem('C++', vscode.TreeItemCollapsibleState.Collapsed, null, 'learn-cpp'),
                new TSITreeItem('DSA C++', vscode.TreeItemCollapsibleState.Collapsed, null, 'learn-dsa-cpp'),
                new TSITreeItem('Git', vscode.TreeItemCollapsibleState.Collapsed, null, 'learn-git'),
                new TSITreeItem('� Mathematics', vscode.TreeItemCollapsibleState.Collapsed, null, 'learn-mathematics'),
                new TSITreeItem('�🚀 Odin Project', vscode.TreeItemCollapsibleState.Collapsed, null, 'learn-odin')
            ]);
        } else if (element.id === 'learn-ruby') {
            // Ruby learning section
            return Promise.resolve([
                new TSITreeItem('Start Learning', vscode.TreeItemCollapsibleState.None, {
                    command: 'tsiheader.learnRuby',
                    title: 'Start Learning Ruby',
                    tooltip: 'Begin Ruby programming tutorials from the start'
                }),
                new TSITreeItem('Browse All Lessons', vscode.TreeItemCollapsibleState.None, {
                    command: 'tsiheader.browseLessons',
                    title: 'Browse All Lessons',
                    tooltip: 'Jump to any lesson in the Ruby curriculum'
                }),
                new TSITreeItem('Run Exercise Tests', vscode.TreeItemCollapsibleState.None, {
                    command: 'tsiheader.runExerciseTests',
                    title: 'Run Exercise Tests',
                    tooltip: 'Run tests on your current Ruby exercise'
                }),
                new TSITreeItem('View Progress', vscode.TreeItemCollapsibleState.None, {
                    command: 'tsiheader.viewLearnProgress',
                    title: 'View Progress',
                    tooltip: 'View your Ruby learning progress and achievements'
                })
            ]);
        } else if (element.id === 'learn-c') {
            // C learning section
            return Promise.resolve([
                new TSITreeItem('Start Learning', vscode.TreeItemCollapsibleState.None, {
                    command: 'tsiheader.learnC',
                    title: 'Start Learning C',
                    tooltip: 'Begin C programming tutorials from the start'
                }),
                new TSITreeItem('Browse All Lessons', vscode.TreeItemCollapsibleState.None, {
                    command: 'tsiheader.browseLessonsC',
                    title: 'Browse All Lessons',
                    tooltip: 'Jump to any lesson in the C curriculum'
                }),
                new TSITreeItem('Run Exercise Tests', vscode.TreeItemCollapsibleState.None, {
                    command: 'tsiheader.runExerciseTestsC',
                    title: 'Run Exercise Tests',
                    tooltip: 'Run tests on your current C exercise'
                }),
                new TSITreeItem('View Progress', vscode.TreeItemCollapsibleState.None, {
                    command: 'tsiheader.viewLearnProgressC',
                    title: 'View Progress',
                    tooltip: 'View your C learning progress and achievements'
                })
            ]);
        } else if (element.id === 'learn-cpp') {
            // C++ learning section
            return Promise.resolve([
                new TSITreeItem('Start Learning', vscode.TreeItemCollapsibleState.None, {
                    command: 'tsiheader.learnCpp',
                    title: 'Start Learning C++',
                    tooltip: 'Begin C++ programming tutorials from the start'
                }),
                new TSITreeItem('Browse All Lessons', vscode.TreeItemCollapsibleState.None, {
                    command: 'tsiheader.browseLessonsCpp',
                    title: 'Browse All Lessons',
                    tooltip: 'Jump to any lesson in the C++ curriculum'
                }),
                new TSITreeItem('Run Exercise Tests', vscode.TreeItemCollapsibleState.None, {
                    command: 'tsiheader.runExerciseTestsCpp',
                    title: 'Run Exercise Tests',
                    tooltip: 'Run tests on your current C++ exercise'
                }),
                new TSITreeItem('View Progress', vscode.TreeItemCollapsibleState.None, {
                    command: 'tsiheader.viewLearnProgressCpp',
                    title: 'View Progress',
                    tooltip: 'View your C++ learning progress and achievements'
                })
            ]);
        } else if (element.id === 'learn-dsa-cpp') {
            return Promise.resolve([
                new TSITreeItem('Start Learning', vscode.TreeItemCollapsibleState.None, {
                    command: 'tsiheader.learnCppDsa',
                    title: 'Start Learning DSA in C++',
                    tooltip: 'Jump straight into the C++ Data Structures & Algorithms module'
                }),
                new TSITreeItem('Browse All Lessons', vscode.TreeItemCollapsibleState.None, {
                    command: 'tsiheader.browseLessonsCppDsa',
                    title: 'Browse DSA Lessons',
                    tooltip: 'Review every lesson in the C++ DSA module'
                }),
                new TSITreeItem('View Progress', vscode.TreeItemCollapsibleState.None, {
                    command: 'tsiheader.viewLearnProgressCppDsa',
                    title: 'View DSA Progress',
                    tooltip: 'Check your progress across the C++ DSA module'
                })
            ]);
        } else if (element.id === 'learn-git') {
            return Promise.resolve([
                new TSITreeItem('Start Learning', vscode.TreeItemCollapsibleState.None, {
                    command: 'tsiheader.learnGit',
                    title: 'Start Learning Git',
                    tooltip: 'Begin the Git mastery roadmap from fundamentals to advanced collaboration'
                }),
                new TSITreeItem('Browse All Lessons', vscode.TreeItemCollapsibleState.None, {
                    command: 'tsiheader.browseLessonsGit',
                    title: 'Browse Git Lessons',
                    tooltip: 'Explore all Git lessons in the roadmap'
                }),
                new TSITreeItem('View Progress', vscode.TreeItemCollapsibleState.None, {
                    command: 'tsiheader.viewLearnProgressGit',
                    title: 'View Git Progress',
                    tooltip: 'Review your Git learning progress and achievements'
                })
            ]);
        }
        else if (element.id === 'learn-rust') {
            return Promise.resolve([
                new TSITreeItem('Start Learning', vscode.TreeItemCollapsibleState.None, {
                    command: 'tsiheader.learnRust',
                    title: 'Start Learning Rust',
                    tooltip: 'Begin Rust tutorials from the start'
                }),
                new TSITreeItem('Browse All Lessons', vscode.TreeItemCollapsibleState.None, {
                    command: 'tsiheader.browseLessonsRust',
                    title: 'Browse All Lessons',
                    tooltip: 'Jump to any lesson in the Rust curriculum'
                }),
                new TSITreeItem('Run Exercise Tests', vscode.TreeItemCollapsibleState.None, {
                    command: 'tsiheader.runExerciseTests',
                    title: 'Run Exercise Tests',
                    tooltip: 'Run tests on your current Rust exercise'
                }),
                new TSITreeItem('View Progress', vscode.TreeItemCollapsibleState.None, {
                    command: 'tsiheader.viewLearnProgressRust',
                    title: 'View Progress',
                    tooltip: 'View your Rust learning progress and achievements'
                })
            ]);
        } else if (element.id === 'learn-odin') {
            return Promise.resolve([
                new TSITreeItem('Start Foundations', vscode.TreeItemCollapsibleState.None, {
                    command: 'tsiheader.learnOdin',
                    title: 'Start The Odin Project',
                    tooltip: 'Begin your full-stack JavaScript journey'
                }),
                new TSITreeItem('Browse All Lessons', vscode.TreeItemCollapsibleState.None, {
                    command: 'tsiheader.browseLessonsOdin',
                    title: 'Browse All Lessons',
                    tooltip: 'Explore all lessons from The Odin Project curriculum'
                }),
                new TSITreeItem('View Progress', vscode.TreeItemCollapsibleState.None, {
                    command: 'tsiheader.viewLearnProgressOdin',
                    title: 'View Progress',
                    tooltip: 'Track your progress through The Odin Project'
                }),
                new TSITreeItem('Clear Cache', vscode.TreeItemCollapsibleState.None, {
                    command: 'tsiheader.clearOdinCache',
                    title: 'Clear Cache',
                    tooltip: 'Clear all cached Odin Project lessons'
                }),
                new TSITreeItem('View Cache Stats', vscode.TreeItemCollapsibleState.None, {
                    command: 'tsiheader.viewOdinCacheStats',
                    title: 'View Cache Stats',
                    tooltip: 'View statistics about cached lessons'
                })
            ]);
        } else if (element.id === 'learn-mathematics') {
            return Promise.resolve([
                new TSITreeItem('Start Learning', vscode.TreeItemCollapsibleState.None, {
                    command: 'tsiheader.learnMathematics',
                    title: 'Start Learning Mathematics',
                    tooltip: 'Begin higher mathematics learning with HELM workbooks'
                }),
                new TSITreeItem('Browse Workbooks', vscode.TreeItemCollapsibleState.None, {
                    command: 'tsiheader.browseMathematicsWorkbooks',
                    title: 'Browse Mathematics Workbooks',
                    tooltip: 'View available mathematics workbooks and PDFs'
                }),
                new TSITreeItem('View Exercises', vscode.TreeItemCollapsibleState.None, {
                    command: 'tsiheader.viewMathematicsExercises',
                    title: 'View Mathematics Exercises',
                    tooltip: 'Browse and complete mathematics exercises'
                }),
                new TSITreeItem('Take Quiz', vscode.TreeItemCollapsibleState.None, {
                    command: 'tsiheader.takeMathematicsQuiz',
                    title: 'Take Mathematics Quiz',
                    tooltip: 'Test your knowledge with interactive mathematics quizzes'
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
                new TSITreeItem('📋 Cached Lessons', vscode.TreeItemCollapsibleState.Collapsed),
                new TSITreeItem('⚙️ Project Templates', vscode.TreeItemCollapsibleState.Collapsed)
            ]);
        } else if (element.label === '📋 Cached Lessons') {
            // Cached lessons section - show cache management commands
            return Promise.resolve([
                new TSITreeItem('Clear Cache', vscode.TreeItemCollapsibleState.None, {
                    command: 'tsiheader.clearOdinCache',
                    title: 'Clear Odin Project Cache',
                    tooltip: 'Clear all cached Odin Project lessons'
                }),
                new TSITreeItem('View Cache Stats', vscode.TreeItemCollapsibleState.None, {
                    command: 'tsiheader.viewOdinCacheStats',
                    title: 'View Odin Project Cache Stats',
                    tooltip: 'View statistics about cached lessons'
                })
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
        } else if (label.includes('📚')) {
            this.iconPath = new vscode.ThemeIcon('book');
        } else if (id === 'learn-ruby') {
            this.iconPath = new vscode.ThemeIcon('ruby');
        } else if (id === 'learn-c') {
            this.iconPath = new vscode.ThemeIcon('file-code');
        } else if (id === 'learn-cpp') {
            this.iconPath = new vscode.ThemeIcon('file-code');
        } else if (id === 'learn-dsa-cpp') {
            this.iconPath = new vscode.ThemeIcon('graph');
        } else if (id === 'learn-git') {
            this.iconPath = new vscode.ThemeIcon('git-branch');
        } else if (id === 'learn-mathematics') {
            this.iconPath = new vscode.ThemeIcon('symbol-operator');
        } else if (id === 'learn-odin') {
            this.iconPath = new vscode.ThemeIcon('rocket');
        } else if (command?.command === 'tsiheader.learnRuby') {
            this.iconPath = new vscode.ThemeIcon('play-circle');
        } else if (command?.command === 'tsiheader.learnC') {
            this.iconPath = new vscode.ThemeIcon('play-circle');
        } else if (command?.command === 'tsiheader.learnCpp') {
            this.iconPath = new vscode.ThemeIcon('play-circle');
        } else if (command?.command === 'tsiheader.learnCppDsa') {
            this.iconPath = new vscode.ThemeIcon('play-circle');
        } else if (command?.command === 'tsiheader.learnGit') {
            this.iconPath = new vscode.ThemeIcon('play-circle');
        } else if (command?.command === 'tsiheader.learnOdin') {
            this.iconPath = new vscode.ThemeIcon('play-circle');
        } else if (command?.command === 'tsiheader.browseLessons') {
            this.iconPath = new vscode.ThemeIcon('list-tree');
        } else if (command?.command === 'tsiheader.browseLessonsC') {
            this.iconPath = new vscode.ThemeIcon('list-tree');
        } else if (command?.command === 'tsiheader.browseLessonsCpp') {
            this.iconPath = new vscode.ThemeIcon('list-tree');
        } else if (command?.command === 'tsiheader.browseLessonsCppDsa') {
            this.iconPath = new vscode.ThemeIcon('list-tree');
        } else if (command?.command === 'tsiheader.browseLessonsGit') {
            this.iconPath = new vscode.ThemeIcon('list-tree');
        } else if (command?.command === 'tsiheader.browseLessonsOdin') {
            this.iconPath = new vscode.ThemeIcon('list-tree');
        } else if (command?.command === 'tsiheader.runExerciseTests') {
            this.iconPath = new vscode.ThemeIcon('beaker');
        } else if (command?.command === 'tsiheader.learnRust') {
            this.iconPath = new vscode.ThemeIcon('play-circle');
        } else if (command?.command === 'tsiheader.browseLessonsRust') {
            this.iconPath = new vscode.ThemeIcon('list-tree');
        } else if (command?.command === 'tsiheader.viewLearnProgressRust') {
            this.iconPath = new vscode.ThemeIcon('graph');
        } else if (command?.command === 'tsiheader.runExerciseTestsC') {
            this.iconPath = new vscode.ThemeIcon('beaker');
        } else if (command?.command === 'tsiheader.runExerciseTestsCpp') {
            this.iconPath = new vscode.ThemeIcon('beaker');
        } else if (command?.command === 'tsiheader.viewLearnProgress') {
            this.iconPath = new vscode.ThemeIcon('graph');
        } else if (command?.command === 'tsiheader.viewLearnProgressC') {
            this.iconPath = new vscode.ThemeIcon('graph');
        } else if (command?.command === 'tsiheader.viewLearnProgressCpp') {
            this.iconPath = new vscode.ThemeIcon('graph');
        } else if (command?.command === 'tsiheader.viewLearnProgressCppDsa') {
            this.iconPath = new vscode.ThemeIcon('graph');
        } else if (command?.command === 'tsiheader.viewLearnProgressGit') {
            this.iconPath = new vscode.ThemeIcon('graph');
        } else if (command?.command === 'tsiheader.viewLearnProgressOdin') {
            this.iconPath = new vscode.ThemeIcon('graph');
        } else if (command?.command === 'tsiheader.clearOdinCache') {
            this.iconPath = new vscode.ThemeIcon('clear-all');
        } else if (command?.command === 'tsiheader.viewOdinCacheStats') {
            this.iconPath = new vscode.ThemeIcon('database');
        }
    }
}

module.exports = {
    TSITreeDataProvider,
    TSIProjectDataProvider
};