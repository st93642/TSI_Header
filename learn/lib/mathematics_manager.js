/**
 * TSI Header - Mathematics Learning Manager
 *
 * Handles mathematics workbooks, PDF viewing, and mathematics exercises
 *
 * @module mathematics_manager
 */

const path = require('path');
const fs = require('fs').promises;

class MathematicsManager {
    constructor(context, vscode) {
        this.context = context;
        this.vscode = vscode;
        this.curriculumCache = new Map();
    }

    /**
     * Load mathematics curriculum
     * @returns {Promise<Object>} Curriculum object
     */
    async loadCurriculum() {
        if (this.curriculumCache.has('mathematics')) {
            return this.curriculumCache.get('mathematics');
        }

        try {
            const curriculumPath = path.join(__dirname, 'curriculum', 'mathematics', 'curriculum.json');
            const curriculumContent = await fs.readFile(curriculumPath, 'utf8');
            const curriculum = JSON.parse(curriculumContent);
            this.curriculumCache.set('mathematics', curriculum);
            return curriculum;
        } catch (error) {
            throw new Error(`Failed to load mathematics curriculum: ${error.message}`);
        }
    }

    /**
     * Get list of available workbooks
     * @returns {Promise<Array>} Array of workbook objects
     */
    async getWorkbooks() {
        try {
            const workbooksPath = path.join(__dirname, 'curriculum', 'mathematics', 'workbooks');
            const files = await fs.readdir(workbooksPath);
            const pdfFiles = files.filter(file => file.endsWith('.pdf'));

            return pdfFiles.map(filename => ({
                id: filename.replace('.pdf', '').toLowerCase().replace(/\s+/g, '_'),
                title: filename.replace('.pdf', ''),
                filename: filename,
                path: path.join(workbooksPath, filename)
            }));
        } catch (error) {
            throw new Error(`Failed to load workbooks: ${error.message}`);
        }
    }

    /**
     * Open a workbook in PDF viewer
     * @param {Object} workbook - Workbook object
     * @returns {Promise<void>}
     */
    async openWorkbook(workbook) {
        try {
            const panel = this.vscode.window.createWebviewPanel(
                'tsiMathematicsWorkbook',
                `📚 ${workbook.title}`,
                this.vscode.ViewColumn.One,
                {
                    enableScripts: true,
                    retainContextWhenHidden: true,
                    localResourceRoots: [
                        this.vscode.Uri.file(path.join(__dirname, '..', '..', 'learn', 'curriculum', 'mathematics', 'workbooks'))
                    ]
                }
            );

            // Convert PDF path to webview URI
            const pdfUri = this.vscode.Uri.file(workbook.path);
            const webviewUri = panel.webview.asWebviewUri(pdfUri);

            panel.webview.html = this.getPdfViewerHtml(workbook, webviewUri);

        } catch (error) {
            this.vscode.window.showErrorMessage(
                `Failed to open workbook: ${error.message}`,
                { modal: true },
                'Got it!'
            );
        }
    }

    /**
     * Load a mathematics exercise
     * @param {string} exerciseId - Exercise ID
     * @returns {Promise<Object>} Exercise object
     */
    async loadExercise(exerciseId) {
        try {
            const exercisePath = path.join(__dirname, 'curriculum', 'mathematics', 'exercises', `${exerciseId}_exercise.json`);
            const exerciseContent = await fs.readFile(exercisePath, 'utf8');
            return JSON.parse(exerciseContent);
        } catch (error) {
            throw new Error(`Failed to load exercise ${exerciseId}: ${error.message}`);
        }
    }

    /**
     * Load a mathematics lesson
     * @param {string} lessonId - Lesson ID
     * @returns {Promise<Object>} Lesson object
     */
    async loadLesson(lessonId) {
        try {
            const lessonPath = path.join(__dirname, 'curriculum', 'mathematics', 'lessons', `${lessonId}.md`);
            const lessonContent = await fs.readFile(lessonPath, 'utf8');
            return {
                id: lessonId,
                content: lessonContent,
                title: this.extractTitleFromContent(lessonContent)
            };
        } catch (error) {
            throw new Error(`Failed to load lesson ${lessonId}: ${error.message}`);
        }
    }

    /**
     * Open a lesson in webview
     * @param {Object} lesson - Lesson object
     * @returns {Promise<void>}
     */
    async openLesson(lesson) {
        try {
            const panel = this.vscode.window.createWebviewPanel(
                'tsiMathematicsLesson',
                `📖 ${lesson.title}`,
                this.vscode.ViewColumn.One,
                {
                    enableScripts: true,
                    retainContextWhenHidden: true
                }
            );

            panel.webview.html = this.getLessonHtml(lesson);

        } catch (error) {
            this.vscode.window.showErrorMessage(
                `Failed to open lesson: ${error.message}`,
                { modal: true },
                'Got it!'
            );
        }
    }

    /**
     * Get curriculum sections
     * @param {Object} curriculum - Curriculum object
     * @returns {Array} Array of sections
     */
    getCurriculumSections(curriculum) {
        return curriculum.modules || [];
    }

    /**
     * Extract title from markdown content
     * @param {string} content - Markdown content
     * @returns {string} Title
     */
    extractTitleFromContent(content) {
        const lines = content.split('\n');
        for (const line of lines) {
            if (line.startsWith('# ')) {
                return line.substring(2).trim();
            }
        }
        return 'Mathematics Lesson';
    }

    /**
     * Generate HTML for PDF viewer
     * @param {Object} workbook - Workbook object
     * @param {string} pdfUri - PDF webview URI
     * @returns {string} HTML string
     */
    getPdfViewerHtml(workbook, pdfUri) {
        return `<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>${workbook.title}</title>
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
            height: 100vh;
            display: flex;
            flex-direction: column;
        }
        .header {
            padding: 10px 20px;
            background-color: var(--vscode-titleBar-activeBackground);
            border-bottom: 1px solid var(--vscode-titleBar-border);
            display: flex;
            justify-content: space-between;
            align-items: center;
        }
        .header h1 {
            font-size: 18px;
            font-weight: 600;
            color: var(--vscode-titleBar-activeForeground);
        }
        .pdf-container {
            flex: 1;
            position: relative;
        }
        iframe {
            width: 100%;
            height: 100%;
            border: none;
        }
        .loading {
            position: absolute;
            top: 50%;
            left: 50%;
            transform: translate(-50%, -50%);
            font-size: 16px;
            color: var(--vscode-descriptionForeground);
        }
        .error {
            position: absolute;
            top: 50%;
            left: 50%;
            transform: translate(-50%, -50%);
            text-align: center;
            color: var(--vscode-errorForeground);
        }
        .error h2 {
            margin-bottom: 10px;
        }
    </style>
</head>
<body>
    <div class="header">
        <h1>📚 ${workbook.title}</h1>
        <div>
            <button onclick="goToExercises()" style="
                background: var(--vscode-button-background);
                color: var(--vscode-button-foreground);
                border: none;
                padding: 6px 12px;
                border-radius: 3px;
                cursor: pointer;
                font-size: 12px;
            ">📝 Exercises</button>
        </div>
    </div>
    <div class="pdf-container">
        <div class="loading">Loading PDF...</div>
        <iframe
            src="${pdfUri}"
            onload="hideLoading()"
            onerror="showError()"
            style="display: none;">
        </iframe>
    </div>

    <script>
        const vscode = acquireVsCodeApi();

        function hideLoading() {
            document.querySelector('.loading').style.display = 'none';
            document.querySelector('iframe').style.display = 'block';
        }

        function showError() {
            document.querySelector('.loading').innerHTML = \`
                <div class="error">
                    <h2>❌ PDF Loading Error</h2>
                    <p>Unable to load the PDF. Please ensure you have a PDF viewer installed.</p>
                    <p>You can also try opening the PDF externally.</p>
                </div>
            \`;
        }

        function goToExercises() {
            vscode.postMessage({
                command: 'openExercises',
                workbookId: '${workbook.id}'
            });
        }
    </script>
</body>
</html>`;
    }

    /**
     * Generate HTML for lesson viewer
     * @param {Object} lesson - Lesson object
     * @returns {string} HTML string
     */
    getLessonHtml(lesson) {
        // Simple markdown-like rendering (could be enhanced with a proper markdown parser)
        const renderedContent = lesson.content
            .replace(/^# (.+)$/gm, '<h1>$1</h1>')
            .replace(/^## (.+)$/gm, '<h2>$1</h2>')
            .replace(/^### (.+)$/gm, '<h3>$1</h3>')
            .replace(/\*\*(.+?)\*\*/g, '<strong>$1</strong>')
            .replace(/\*(.+?)\*/g, '<em>$1</em>')
            .replace(/`(.+?)`/g, '<code>$1</code>')
            .replace(/\n\n/g, '</p><p>')
            .replace(/\n/g, '<br>');

        return `<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>${lesson.title}</title>
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
            max-width: 800px;
            margin: 0 auto;
        }
        h1 {
            color: var(--vscode-textLink-foreground);
            border-bottom: 2px solid var(--vscode-textLink-foreground);
            padding-bottom: 10px;
            font-size: 28px;
            font-weight: 600;
            margin-bottom: 20px;
        }
        h2 {
            color: var(--vscode-textLink-foreground);
            font-size: 24px;
            font-weight: 600;
            margin: 30px 0 15px 0;
        }
        h3 {
            color: var(--vscode-textLink-foreground);
            font-size: 20px;
            font-weight: 600;
            margin: 25px 0 10px 0;
        }
        p {
            margin-bottom: 15px;
            font-size: 16px;
        }
        code {
            background-color: var(--vscode-textCodeBlock-background);
            padding: 2px 4px;
            border-radius: 3px;
            font-family: var(--vscode-editor-font-family);
            font-size: 14px;
        }
        strong {
            font-weight: 600;
        }
        em {
            font-style: italic;
        }
        .math-expression {
            background-color: var(--vscode-textBlockQuote-background);
            border-left: 4px solid var(--vscode-textLink-foreground);
            padding: 15px 20px;
            margin: 20px 0;
            font-family: 'Times New Roman', serif;
        }
        .navigation {
            margin-top: 40px;
            text-align: center;
            padding-top: 20px;
            border-top: 1px solid var(--vscode-textBlockQuote-border);
        }
        .nav-button {
            background-color: var(--vscode-button-background);
            color: var(--vscode-button-foreground);
            border: none;
            padding: 10px 20px;
            margin: 0 5px;
            border-radius: 3px;
            cursor: pointer;
            font-size: 16px;
            font-weight: 500;
        }
        .nav-button:hover {
            background-color: var(--vscode-button-hoverBackground);
        }
    </style>
</head>
<body>
    <h1>📖 ${lesson.title}</h1>
    <div class="content">
        <p>${renderedContent}</p>
    </div>

    <div class="navigation">
        <button class="nav-button" onclick="openWorkbook()">📚 View Workbook</button>
        <button class="nav-button" onclick="startExercise()">📝 Practice Exercise</button>
        <button class="nav-button" onclick="nextLesson()">Next Lesson →</button>
    </div>

    <script>
        const vscode = acquireVsCodeApi();

        function openWorkbook() {
            vscode.postMessage({
                command: 'openWorkbook'
            });
        }

        function startExercise() {
            vscode.postMessage({
                command: 'startExercise'
            });
        }

        function nextLesson() {
            vscode.postMessage({
                command: 'nextLesson'
            });
        }
    </script>
</body>
</html>`;
    }

    /**
     * Clear curriculum cache
     */
    clearCurriculumCache() {
        this.curriculumCache.clear();
    }
}

module.exports = MathematicsManager;