/**
 * Learn Manager - Handles curriculum loading and lesson management
 */

const path = require('path');
const fs = require('fs').promises;

class LearnManager {
    constructor(context, vscode) {
        this.context = context;
        this.vscode = vscode;
        this.curriculumCache = new Map();
        this.currentLessonPanel = null; // Track current lesson panel
        this.currentExerciseEditor = null; // Track current exercise editor
    }

    /**
     * Close all learn-related tabs (exercise files and lesson panels)
     */
    async closeAllLearnTabs() {
        // Close current lesson panel if exists
        if (this.currentLessonPanel) {
            this.currentLessonPanel.dispose();
            this.currentLessonPanel = null;
        }
        
        // Close current exercise tab if exists
        if (this.currentExerciseEditor) {
            const tabGroups = this.vscode.window.tabGroups;
            for (const group of tabGroups.all) {
                for (const tab of group.tabs) {
                    if (tab.input && tab.input.uri && tab.input.uri.fsPath === this.currentExerciseEditor) {
                        await this.vscode.window.tabGroups.close(tab);
                        break;
                    }
                }
            }
            this.currentExerciseEditor = null;
        }
        
        // Close ALL tabs (including webviews and learn_exercises files)
        const tabGroups = this.vscode.window.tabGroups;
        const tabsToClose = [];
        for (const group of tabGroups.all) {
            for (const tab of group.tabs) {
                // Check if it's a webview tab (lesson panel)
                if (tab.input && tab.input.viewType === 'tsiLearnLesson') {
                    tabsToClose.push(tab);
                }
                // Check if it's a learn_exercises file
                else if (tab.input && tab.input.uri) {
                    const filePath = tab.input.uri.fsPath;
                    if (filePath.includes('/learn_exercises/') || filePath.includes('\\learn_exercises\\')) {
                        tabsToClose.push(tab);
                    }
                }
            }
        }
        
        // Close all collected tabs
        for (const tab of tabsToClose) {
            await this.vscode.window.tabGroups.close(tab);
        }
    }

    /**
     * Load curriculum for a language
     * @param {string} language - The programming language
     * @returns {Promise<Object>} Curriculum object
     */
    async loadCurriculum(language) {
        // Check cache first
        if (this.curriculumCache.has(language)) {
            return this.curriculumCache.get(language);
        }

        try {
            const curriculumPath = path.join(__dirname, '..', 'curriculum', language, 'curriculum.json');
            const content = await fs.readFile(curriculumPath, 'utf8');
            const curriculum = JSON.parse(content);
            
            // Cache the curriculum
            this.curriculumCache.set(language, curriculum);
            
            return curriculum;
        } catch (error) {
            throw new Error(`Failed to load curriculum for ${language}: ${error.message}`);
        }
    }

    /**
     * Get the next lesson based on progress
     * @param {Object} curriculum - The curriculum object
     * @param {Object} progress - Current progress
     * @returns {Object|null} Next lesson or null if complete
     */
    getNextLesson(curriculum, progress) {
        const completedLessons = new Set(progress.completed || []);
        
        // Find first incomplete lesson
        for (const module of curriculum.modules) {
            for (const lesson of module.lessons) {
                if (!completedLessons.has(lesson.id)) {
                    return {
                        ...lesson,
                        moduleTitle: module.title,
                        moduleId: module.id
                    };
                }
            }
        }
        
        return null; // All lessons completed
    }

    /**
     * Open a lesson in the editor
     * @param {string} language - The programming language
     * @param {Object} lesson - The lesson to open
     */
    async openLesson(language, lesson) {
        try {
            // Close ALL previous learn-related tabs (lesson panel + all exercise tabs)
            await this.closeAllLearnTabs();
            
            // Load lesson content
            const lessonPath = path.join(__dirname, '..', 'curriculum', language, 'lessons', `${lesson.id}.md`);
            const lessonContent = await fs.readFile(lessonPath, 'utf8');
            
            // Create webview panel for lesson
            const panel = this.vscode.window.createWebviewPanel(
                'tsiLearnLesson',
                `Learn ${language}: ${lesson.title || lesson.id}`,
                this.vscode.ViewColumn.One,
                {
                    enableScripts: true,
                    retainContextWhenHidden: true
                }
            );
            
            // Store reference to current panel
            this.currentLessonPanel = panel;
            
            // Clear reference when panel is disposed
            panel.onDidDispose(() => {
                if (this.currentLessonPanel === panel) {
                    this.currentLessonPanel = null;
                }
            }, null, this.context.subscriptions);
            
            // Set HTML content
            panel.webview.html = this.getLessonHtml(lessonContent, lesson);
            
            // Handle messages from webview
            panel.webview.onDidReceiveMessage(
                async message => {
                    switch (message.command) {
                        case 'startExercise':
                            await this.startExercise(language, lesson, message.exerciseId);
                            break;
                        case 'completeLesson':
                            await this.completeLesson(language, lesson.id);
                            panel.dispose();
                            break;
                    }
                },
                undefined,
                this.context.subscriptions
            );
            
        } catch (error) {
            this.vscode.window.showErrorMessage(
                `Failed to open lesson: ${error.message}`,
                { modal: true },
                'Got it!'
            );
        }
    }

    /**
     * Generate HTML for lesson display
     * @param {string} content - Markdown content
     * @param {Object} lesson - Lesson metadata
     * @returns {string} HTML string
     */
    getLessonHtml(content, lesson) {
        // Convert markdown to HTML (simplified)
        const htmlContent = this.markdownToHtml(content);
        
        return `<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>${lesson.title || 'Lesson'}</title>
    <style>
        * {
            margin: 0;
            padding: 0;
            box-sizing: border-box;
        }
        body {
            font-family: var(--vscode-font-family);
            color: var(--vscode-foreground);
            background-color: var(--vscode-editor-background);
            padding: 20px;
            line-height: 1.6;
            font-size: 14px;
            max-width: 900px;
            margin: 0 auto;
        }
        h1 {
            color: var(--vscode-textLink-foreground);
            border-bottom: 2px solid var(--vscode-textLink-foreground);
            padding-bottom: 10px;
            font-size: 24px;
            font-weight: 600;
            margin-top: 0;
            margin-bottom: 20px;
        }
        h2 {
            color: var(--vscode-textLink-activeForeground);
            margin-top: 30px;
            margin-bottom: 15px;
            font-size: 18px;
            font-weight: 600;
        }
        h3 {
            color: var(--vscode-textLink-activeForeground);
            margin-top: 20px;
            margin-bottom: 10px;
            font-size: 16px;
            font-weight: 600;
        }
        p {
            margin: 10px 0;
            font-size: 14px;
            line-height: 1.6;
        }
        ul, ol {
            margin: 10px 0;
            padding-left: 25px;
        }
        li {
            margin: 5px 0;
            font-size: 14px;
            line-height: 1.6;
        }
        code {
            background-color: var(--vscode-textCodeBlock-background);
            padding: 2px 6px;
            border-radius: 3px;
            font-family: var(--vscode-editor-font-family);
            font-size: 13px;
            color: var(--vscode-textPreformat-foreground);
        }
        pre {
            background-color: var(--vscode-textCodeBlock-background);
            padding: 15px;
            border-radius: 5px;
            overflow-x: auto;
            margin: 15px 0;
            line-height: 1.5;
        }
        pre code {
            background-color: transparent;
            padding: 0;
            font-size: 13px;
            line-height: 1.5;
            display: block;
            color: var(--vscode-textPreformat-foreground);
        }
        pre code * {
            font-size: 13px !important;
            line-height: 1.5 !important;
        }
        .code-comment {
            color: var(--vscode-editorLineNumber-foreground);
            font-style: italic;
            font-size: 13px;
        }
        strong {
            font-weight: 600;
            color: var(--vscode-textLink-activeForeground);
        }
        em {
            font-style: italic;
            color: var(--vscode-textPreformat-foreground);
        }
        .exercise-button {
            background-color: var(--vscode-button-background);
            color: var(--vscode-button-foreground);
            border: none;
            padding: 10px 20px;
            margin: 10px 5px;
            border-radius: 3px;
            cursor: pointer;
            font-size: 14px;
            font-weight: 500;
        }
        .exercise-button:hover {
            background-color: var(--vscode-button-hoverBackground);
        }
        .complete-button {
            background-color: var(--vscode-button-secondaryBackground);
            color: var(--vscode-button-secondaryForeground);
        }
        .button-container {
            margin-top: 30px;
            text-align: center;
        }
        .tip {
            background-color: var(--vscode-textBlockQuote-background);
            border-left: 4px solid var(--vscode-textLink-foreground);
            padding: 10px 15px;
            margin: 20px 0;
            font-size: 14px;
            line-height: 1.6;
        }
    </style>
</head>
<body>
    ${htmlContent}
    
    <div class="button-container">
        <button class="exercise-button" onclick="startExercise()">
            📝 Start Practice Exercise
        </button>
        <button class="exercise-button complete-button" onclick="completeLesson()">
            ✅ Mark as Complete
        </button>
    </div>
    
    <script>
        const vscode = acquireVsCodeApi();
        
        function startExercise() {
            vscode.postMessage({
                command: 'startExercise',
                exerciseId: '${lesson.id}_exercise'
            });
        }
        
        function completeLesson() {
            vscode.postMessage({
                command: 'completeLesson'
            });
        }
    </script>
</body>
</html>`;
    }

    /**
     * Simple markdown to HTML converter
     * @param {string} markdown - Markdown content
     * @returns {string} HTML content
     */
    markdownToHtml(markdown) {
        let html = markdown;
        
        // Convert code blocks first (before other replacements)
        html = html.replace(/```(\w+)?\n([\s\S]*?)```/g, (match, lang, code) => {
            // Highlight comments in code blocks
            const processedCode = code
                .split('\n')
                .map(line => {
                    // Check if line is a comment
                    if (line.trim().startsWith('#')) {
                        return `<span class="code-comment">${this.escapeHtml(line)}</span>`;
                    }
                    return this.escapeHtml(line);
                })
                .join('\n');
            return `<pre><code>${processedCode}</code></pre>`;
        });
        
        // Convert headers
        html = html.replace(/^### (.+)$/gm, '<h3>$1</h3>');
        html = html.replace(/^## (.+)$/gm, '<h2>$1</h2>');
        html = html.replace(/^# (.+)$/gm, '<h1>$1</h1>');
        
        // Convert lists
        html = html.replace(/^- (.+)$/gm, '<li>$1</li>');
        html = html.replace(/(<li>.*<\/li>\n?)+/g, '<ul>$&</ul>');
        
        // Convert bold and italic (before inline code to avoid conflicts)
        html = html.replace(/\*\*(.+?)\*\*/g, '<strong>$1</strong>');
        html = html.replace(/(?<!\*)\*(?!\*)(.+?)(?<!\*)\*(?!\*)/g, '<em>$1</em>');
        
        // Convert inline code
        html = html.replace(/`([^`]+)`/g, '<code>$1</code>');
        
        // Convert blockquotes
        html = html.replace(/^> (.+)$/gm, '<div class="tip">$1</div>');
        
        // Convert paragraphs (avoid converting already tagged content)
        html = html.replace(/\n\n+/g, '</p><p>');
        html = html.replace(/^(?!<[hupldi])(.+)$/gm, '<p>$1</p>');
        
        // Clean up extra paragraph tags around block elements
        html = html.replace(/<p>(<(?:h\d|pre|ul|div))/g, '$1');
        html = html.replace(/(<\/(?:h\d|pre|ul|div)>)<\/p>/g, '$1');
        
        // Remove empty paragraphs
        html = html.replace(/<p>\s*<\/p>/g, '');
        
        return html;
    }
    
    /**
     * Escape HTML special characters
     * @param {string} text - Text to escape
     * @returns {string} Escaped text
     */
    escapeHtml(text) {
        return text
            .replace(/&/g, '&amp;')
            .replace(/</g, '&lt;')
            .replace(/>/g, '&gt;')
            .replace(/"/g, '&quot;')
            .replace(/'/g, '&#039;');
    }

    /**
     * Start an exercise
     * @param {string} language - The programming language
     * @param {Object} lesson - The lesson
     * @param {string} exerciseId - Exercise ID
     */
    async startExercise(language, lesson, exerciseId) {
        try {
            const exercisePath = path.join(__dirname, '..', 'curriculum', language, 'exercises', `${exerciseId}.json`);
            const exerciseContent = await fs.readFile(exercisePath, 'utf8');
            const exercise = JSON.parse(exerciseContent);
            
            // Close the lesson panel when starting exercise
            if (this.currentLessonPanel) {
                this.currentLessonPanel.dispose();
                this.currentLessonPanel = null;
            }
            
            // Create exercise file in workspace
            const workspaceFolder = this.vscode.workspace.workspaceFolders?.[0];
            if (!workspaceFolder) {
                this.vscode.window.showErrorMessage(
                    'Please open a workspace folder first.',
                    { modal: true },
                    'Got it!'
                );
                return;
            }
            
            const exerciseFilePath = path.join(workspaceFolder.uri.fsPath, 'learn_exercises', language, `${exerciseId}.${this.getFileExtension(language)}`);
            
            // Close previous exercise editor if it exists
            if (this.currentExerciseEditor) {
                // Try to find and close the previous exercise file
                const tabGroups = this.vscode.window.tabGroups;
                for (const group of tabGroups.all) {
                    for (const tab of group.tabs) {
                        if (tab.input && tab.input.uri && tab.input.uri.fsPath === this.currentExerciseEditor) {
                            await this.vscode.window.tabGroups.close(tab);
                            break;
                        }
                    }
                }
                this.currentExerciseEditor = null;
            }
            
            // Create directory if it doesn't exist
            const exerciseDir = path.dirname(exerciseFilePath);
            await fs.mkdir(exerciseDir, { recursive: true });
            
            // Write starter code
            await fs.writeFile(exerciseFilePath, exercise.starterCode || '# Write your code here\n');
            
            // Store reference to current exercise file
            this.currentExerciseEditor = exerciseFilePath;
            
            // Open the file
            const doc = await this.vscode.workspace.openTextDocument(exerciseFilePath);
            const editor = await this.vscode.window.showTextDocument(doc);
            
            // Position cursor on an empty line after a comment marker
            // Look for patterns like "# Your code here", "# Use", etc.
            const text = doc.getText();
            const lines = text.split('\n');
            let targetLine = -1;
            
            for (let i = 0; i < lines.length; i++) {
                const line = lines[i].trim();
                // Look for comment lines that indicate where to write code
                if (line.startsWith('#') && 
                    (line.includes('Your code here') || 
                     line.includes('your code here') ||
                     line.includes('Use if') ||
                     line.includes('Use a') ||
                     line.includes('Use') ||
                     line.includes('Return') ||
                     line.includes('Calculate'))) {
                    // Check if next line is empty or not
                    const nextLineIndex = i + 1;
                    if (nextLineIndex < lines.length) {
                        const nextLine = lines[nextLineIndex].trim();
                        // If next line is empty, use it; otherwise insert a new line
                        if (nextLine === '') {
                            targetLine = nextLineIndex;
                        } else {
                            // Need to insert an empty line
                            targetLine = nextLineIndex;
                            // Use edit to insert a newline
                            await editor.edit(editBuilder => {
                                const position = new this.vscode.Position(nextLineIndex, 0);
                                editBuilder.insert(position, '\n');
                            });
                        }
                    }
                    break;
                }
            }
            
            // Set cursor position if found
            if (targetLine >= 0) {
                const position = new this.vscode.Position(targetLine, 0);
                editor.selection = new this.vscode.Selection(position, position);
                editor.revealRange(new this.vscode.Range(position, position));
            }
            
            this.vscode.window.showInformationMessage(
                `Exercise "${exercise.title}" opened. Complete the code and run tests!`,
                { modal: true },
                'Run Tests',
                'Got it!'
            ).then(selection => {
                if (selection === 'Run Tests') {
                    this.vscode.commands.executeCommand('tsiheader.runExerciseTests', language, exercise);
                }
            });
            
        } catch (error) {
            this.vscode.window.showErrorMessage(
                `Failed to start exercise: ${error.message}`,
                { modal: true },
                'Got it!'
            );
        }
    }

    /**
     * Get file extension for language
     * @param {string} language - Programming language
     * @returns {string} File extension
     */
    getFileExtension(language) {
        const extensions = {
            'ruby': 'rb',
            'python': 'py',
            'javascript': 'js',
            'java': 'java',
            'cpp': 'cpp',
            'c': 'c'
        };
        return extensions[language.toLowerCase()] || 'txt';
    }

    /**
     * Mark lesson as complete
     * @param {string} language - Programming language
     * @param {string} lessonId - Lesson ID
     */
    async completeLesson(language, lessonId) {
        const progressKey = `learn_progress_${language}`;
        const progress = this.context.globalState.get(progressKey, { completed: [] });
        
        if (!progress.completed.includes(lessonId)) {
            progress.completed.push(lessonId);
            await this.context.globalState.update(progressKey, progress);
            
            // Load curriculum to find next lesson
            const curriculum = await this.loadCurriculum(language);
            const nextLesson = this.getNextLesson(curriculum, progress);
            
            if (nextLesson) {
                // Has next lesson - offer to jump to it
                this.vscode.window.showInformationMessage(
                    `🎉 Lesson completed! Progress saved.`,
                    { modal: true },
                    'Next Lesson',
                    'Got it!'
                ).then(async selection => {
                    if (selection === 'Next Lesson') {
                        // Directly open the next lesson
                        await this.openLesson(language, nextLesson);
                    }
                });
            } else {
                // Curriculum complete
                this.vscode.window.showInformationMessage(
                    `🎉 Congratulations! You've completed all ${language} lessons!`,
                    { modal: true },
                    'Review Lessons',
                    'Got it!'
                ).then(selection => {
                    if (selection === 'Review Lessons') {
                        this.vscode.commands.executeCommand('tsiheader.browseLessons');
                    }
                });
            }
        }
    }

    /**
     * Load solution for an exercise
     * @param {string} language - Programming language
     * @param {string} exerciseId - Exercise ID
     * @returns {Promise<Object>} Solution object
     */
    async loadSolution(language, exerciseId) {
        try {
            const solutionPath = path.join(__dirname, '..', 'curriculum', language, 'solutions', `${exerciseId}.json`);
            const content = await fs.readFile(solutionPath, 'utf8');
            return JSON.parse(content);
        } catch (error) {
            throw new Error(`Failed to load solution: ${error.message}`);
        }
    }
}

module.exports = LearnManager;
