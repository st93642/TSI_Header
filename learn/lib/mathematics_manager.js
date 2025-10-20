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
            const curriculumPath = path.join(__dirname, '..', 'curriculum', 'mathematics', 'curriculum.json');
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
            const workbooksPath = path.join(__dirname, '..', 'curriculum', 'mathematics', 'workbooks');
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
            // Open PDF file using VS Code's default file opener
            const uri = this.vscode.Uri.file(workbook.path);
            await this.vscode.commands.executeCommand('vscode.open', uri);
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
            const exercisePath = path.join(__dirname, '..', 'curriculum', 'mathematics', 'exercises', `${exerciseId}_exercise.json`);
            const exerciseContent = await fs.readFile(exercisePath, 'utf8');
            return JSON.parse(exerciseContent);
        } catch (error) {
            throw new Error(`Failed to load exercise ${exerciseId}: ${error.message}`);
        }
    }

    /**
     * Load a mathematics quiz
     * @param {string} quizId - Quiz ID
     * @returns {Promise<Object>} Quiz object
     */
    async loadQuiz(quizId) {
        try {
            const quizPath = path.join(__dirname, '..', 'curriculum', 'mathematics', 'quizzes', `${quizId}_quiz.json`);
            const quizContent = await fs.readFile(quizPath, 'utf8');
            return JSON.parse(quizContent);
        } catch (error) {
            throw new Error(`Failed to load quiz ${quizId}: ${error.message}`);
        }
    }

    /**
     * Get list of available quizzes
     * @returns {Promise<Array>} Array of quiz objects
     */
    async getQuizzes() {
        try {
            const quizzesPath = path.join(__dirname, '..', 'curriculum', 'mathematics', 'quizzes');
            const files = await fs.readdir(quizzesPath);
            const jsonFiles = files.filter(file => file.endsWith('_quiz.json'));

            return jsonFiles.map(filename => ({
                id: filename.replace('_quiz.json', ''),
                title: filename.replace('_quiz.json', '').replace(/_/g, ' ').replace(/\b\w/g, l => l.toUpperCase()),
                filename: filename,
                path: path.join(quizzesPath, filename)
            }));
        } catch (error) {
            throw new Error(`Failed to load quizzes: ${error.message}`);
        }
    }

    /**
     * Get list of available exercises
     * @returns {Promise<Array>} Array of exercise objects
     */
    async getExercises() {
        try {
            const exercisesPath = path.join(__dirname, '..', 'curriculum', 'mathematics', 'exercises');
            
            // Check if exercises directory exists
            try {
                await fs.access(exercisesPath);
            } catch (error) {
                // Directory doesn't exist, return empty array
                return [];
            }
            
            const files = await fs.readdir(exercisesPath);
            const jsonFiles = files.filter(file => file.endsWith('_exercise.json'));

            return jsonFiles.map(filename => ({
                id: filename.replace('_exercise.json', ''),
                title: filename.replace('_exercise.json', '').replace(/_/g, ' ').replace(/\b\w/g, l => l.toUpperCase()),
                filename: filename,
                path: path.join(exercisesPath, filename)
            }));
        } catch (error) {
            // Return empty array instead of throwing error
            console.warn('Exercises directory not available:', error.message);
            return [];
        }
    }

    /**
     * Open an exercise in webview
     * @param {Object} exercise - Exercise object
     * @returns {Promise<void>}
     */
    async openExercise(exercise) {
        try {
            // Configure webview options with local resource access
            const resourceRoots = [];
            if (this.vscode && this.vscode.Uri && typeof this.vscode.Uri.file === 'function') {
                const resourcesRoot = path.join(__dirname, '..', '..', 'resources');
                resourceRoots.push(this.vscode.Uri.file(resourcesRoot));
            }

            const webviewOptions = {
                enableScripts: true,
                retainContextWhenHidden: true
            };

            if (resourceRoots.length > 0) {
                webviewOptions.localResourceRoots = resourceRoots;
            }

            const panel = this.vscode.window.createWebviewPanel(
                'tsiMathematicsExercise',
                `� ${exercise.title}`,
                this.vscode.ViewColumn.One,
                webviewOptions
            );

            panel.webview.html = this.getExerciseHtml(exercise, panel.webview);

            // Handle messages from the webview
            panel.webview.onDidReceiveMessage(async (message) => {
                switch (message.command) {
                    case 'openWorkbook':
                        try {
                            // Find the workbook based on the workbookId or exercise workbook reference
                            const workbooks = await this.getWorkbooks();
                            let workbookToOpen;

                            if (message.workbookId) {
                                // Find workbook by ID
                                workbookToOpen = workbooks.find(wb =>
                                    wb.id.includes(message.workbookId) ||
                                    wb.title.toLowerCase().includes(message.workbookId.toLowerCase())
                                );
                            } else if (exercise.workbook) {
                                // Find workbook by exercise's workbook reference
                                workbookToOpen = workbooks.find(wb =>
                                    wb.title.includes(exercise.workbook) ||
                                    exercise.workbook.includes(wb.title)
                                );
                            }

                            if (workbookToOpen) {
                                await this.openWorkbook(workbookToOpen);
                            } else {
                                // Fallback: open first available workbook
                                if (workbooks.length > 0) {
                                    await this.openWorkbook(workbooks[0]);
                                } else {
                                    this.vscode.window.showErrorMessage('No workbooks available');
                                }
                            }
                        } catch (error) {
                            this.vscode.window.showErrorMessage(`Failed to open workbook: ${error.message}`);
                        }
                        break;

                    case 'takeQuiz':
                        try {
                            // Find appropriate quiz based on exercise
                            const quizzes = await this.getQuizzes();
                            let quizToOpen;

                            if (message.exerciseId) {
                                // Try to find quiz that matches the exercise
                                quizToOpen = quizzes.find(quiz =>
                                    quiz.id.includes(message.exerciseId.replace('_exercise', '')) ||
                                    message.exerciseId.includes(quiz.id)
                                );
                            }

                            if (quizToOpen) {
                                const quizData = await this.loadQuiz(quizToOpen.id);
                                await this.openQuiz(quizData);
                            } else if (quizzes.length > 0) {
                                // Fallback: open first available quiz
                                const quizData = await this.loadQuiz(quizzes[0].id);
                                await this.openQuiz(quizData);
                            } else {
                                this.vscode.window.showErrorMessage('No quizzes available');
                            }
                        } catch (error) {
                            this.vscode.window.showErrorMessage(`Failed to open quiz: ${error.message}`);
                        }
                        break;
                }
            });

        } catch (error) {
            this.vscode.window.showErrorMessage(
                `Failed to open exercise: ${error.message}`,
                { modal: true },
                'Got it!'
            );
        }
    }

    /**
     * Open a quiz in webview
     * @param {Object} quiz - Quiz object
     * @returns {Promise<void>}
     */
    async openQuiz(quiz) {
        try {
            // Configure webview options with local resource access
            const resourceRoots = [];
            if (this.vscode && this.vscode.Uri && typeof this.vscode.Uri.file === 'function') {
                const resourcesRoot = path.join(__dirname, '..', '..', 'resources');
                resourceRoots.push(this.vscode.Uri.file(resourcesRoot));
            }

            const webviewOptions = {
                enableScripts: true,
                retainContextWhenHidden: true
            };

            if (resourceRoots.length > 0) {
                webviewOptions.localResourceRoots = resourceRoots;
            }

            const panel = this.vscode.window.createWebviewPanel(
                'tsiMathematicsQuiz',
                `🧠 ${quiz.title}`,
                this.vscode.ViewColumn.One,
                webviewOptions
            );

            panel.webview.html = this.getQuizHtml(quiz, panel.webview);

            // Handle messages from the webview
            panel.webview.onDidReceiveMessage(async (message) => {
                switch (message.command) {
                    case 'openWorkbook':
                        try {
                            // Find the workbook based on the quiz's workbook reference
                            const workbooks = await this.getWorkbooks();
                            let workbookToOpen;

                            if (quiz.workbook) {
                                // Find workbook by quiz's workbook reference
                                workbookToOpen = workbooks.find(wb =>
                                    wb.title.includes(quiz.workbook) ||
                                    quiz.workbook.includes(wb.title)
                                );
                            }

                            if (workbookToOpen) {
                                await this.openWorkbook(workbookToOpen);
                            } else if (workbooks.length > 0) {
                                // Fallback: open first available workbook
                                await this.openWorkbook(workbooks[0]);
                            } else {
                                this.vscode.window.showErrorMessage('No workbooks available');
                            }
                        } catch (error) {
                            this.vscode.window.showErrorMessage(`Failed to open workbook: ${error.message}`);
                        }
                        break;
                }
            });

        } catch (error) {
            this.vscode.window.showErrorMessage(
                `Failed to open quiz: ${error.message}`,
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
     * Generate HTML for exercise viewer
     * @param {Object} exercise - Exercise object
     * @param {Object} webview - Webview instance for URI conversion
     * @returns {string} HTML string
     */
    getExerciseHtml(exercise, webview) {
        // Handle description as string or array
        const descriptionText = Array.isArray(exercise.description) 
            ? exercise.description.join('\n') 
            : exercise.description;
        const formattedDescription = this.processMathContent(this.markdownToHtml(descriptionText));

        // Handle solution as string or array
        const solutionText = Array.isArray(exercise.solution) 
            ? exercise.solution.join('\n') 
            : (exercise.solution || 'Solution not available yet.');
        const formattedSolution = this.processMathContent(this.markdownToHtml(solutionText));

        const hintsHtml = exercise.hints ? exercise.hints.map(hint =>
            `<li>${this.markdownToHtml(hint)}</li>`
        ).join('') : '';

        // Prepare URIs for KaTeX assets. If a webview is provided, use local extension resources
        let katexCss = 'https://cdn.jsdelivr.net/npm/katex@0.16.9/dist/katex.min.css';
        let katexJs = 'https://cdn.jsdelivr.net/npm/katex@0.16.9/dist/katex.min.js';
        let autoRenderJs = 'https://cdn.jsdelivr.net/npm/katex@0.16.9/dist/contrib/auto-render.min.js';
        if (webview && this.vscode && this.vscode.Uri) {
            try {
                const cssPath = path.join(__dirname, '..', '..', 'resources', 'katex', 'katex.min.css');
                const jsPath = path.join(__dirname, '..', '..', 'resources', 'katex', 'katex.min.js');
                const autoRenderPath = path.join(__dirname, '..', '..', 'resources', 'katex', 'auto-render.min.js');
                katexCss = webview.asWebviewUri(this.vscode.Uri.file(cssPath));
                katexJs = webview.asWebviewUri(this.vscode.Uri.file(jsPath));
                autoRenderJs = webview.asWebviewUri(this.vscode.Uri.file(autoRenderPath));
            } catch (e) {
                // fallback to CDN if anything goes wrong
            }
        }

        return `<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>${exercise.title}</title>
    <link rel="stylesheet" href="${katexCss}">
    <script src="${katexJs}"></script>
    <script src="${autoRenderJs}"></script>
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
            max-width: 900px;
            margin: 0 auto;
            font-size: 16px;
        }
        .header {
            background-color: var(--vscode-titleBar-activeBackground);
            color: var(--vscode-titleBar-activeForeground);
            padding: 20px;
            border-radius: 8px;
            margin-bottom: 20px;
            border: 1px solid var(--vscode-titleBar-border);
        }
        .header h1 {
            font-size: 24px;
            font-weight: 600;
            margin-bottom: 10px;
        }
        .difficulty {
            display: inline-block;
            padding: 4px 8px;
            border-radius: 4px;
            font-size: 12px;
            font-weight: 500;
            text-transform: uppercase;
        }
        .difficulty.intermediate {
            background-color: var(--vscode-charts-orange);
            color: white;
        }
        .difficulty.advanced {
            background-color: var(--vscode-charts-red);
            color: white;
        }
        .exercise-content {
            background-color: var(--vscode-editorWidget-background);
            border: 1px solid var(--vscode-editorWidget-border);
            border-radius: 8px;
            padding: 20px;
            margin-bottom: 20px;
        }
        .exercise-content h2 {
            color: var(--vscode-textLink-foreground);
            font-size: 20px;
            margin-bottom: 15px;
            border-bottom: 2px solid var(--vscode-textLink-foreground);
            padding-bottom: 5px;
        }
        .exercise-content p {
            margin-bottom: 15px;
            font-size: 16px;
            line-height: 1.6;
        }
        .math-expression {
            background-color: var(--vscode-textBlockQuote-background);
            border-left: 4px solid var(--vscode-textLink-foreground);
            padding: 15px 20px;
            margin: 20px 0;
            font-family: 'Times New Roman', serif;
            font-size: 16px;
            white-space: pre-wrap;
            line-height: 1.6;
        }
        .hints-section {
            background-color: var(--vscode-textBlockQuote-background);
            border: 1px solid var(--vscode-textBlockQuote-border);
            border-radius: 6px;
            padding: 15px;
            margin-bottom: 20px;
        }
        .hints-section h3 {
            color: var(--vscode-textLink-foreground);
            font-size: 18px;
            margin-bottom: 10px;
        }
        .hints-section ul {
            margin-left: 20px;
        }
        .hints-section li {
            margin-bottom: 8px;
            color: var(--vscode-descriptionForeground);
        }
        .actions {
            text-align: center;
            padding-top: 20px;
            border-top: 1px solid var(--vscode-textBlockQuote-border);
        }
        .btn {
            background-color: var(--vscode-button-background);
            color: var(--vscode-button-foreground);
            border: none;
            padding: 12px 24px;
            border-radius: 6px;
            cursor: pointer;
            font-size: 16px;
            font-weight: 500;
            margin: 0 10px;
            transition: background-color 0.2s;
        }
        .btn:hover {
            background-color: var(--vscode-button-hoverBackground);
        }
        .btn.secondary {
            background-color: var(--vscode-button-secondaryBackground);
            color: var(--vscode-button-secondaryForeground);
        }
        .btn.secondary:hover {
            background-color: var(--vscode-button-secondaryHoverBackground);
        }
        .solution-section {
            background-color: var(--vscode-textBlockQuote-background);
            border: 1px solid var(--vscode-textBlockQuote-border);
            border-radius: 6px;
            padding: 15px;
            margin-top: 20px;
            display: none;
        }
        .solution-section.show {
            display: block;
        }
        .solution-section h3 {
            color: var(--vscode-textLink-foreground);
            font-size: 18px;
            margin-bottom: 10px;
        }
        .solution-content {
            background-color: var(--vscode-editor-background);
            border: 1px solid var(--vscode-editorWidget-border);
            border-radius: 4px;
            padding: 15px;
            color: var(--vscode-foreground);
            font-size: 16px;
            line-height: 1.6;
        }
        .solution-content p {
            margin-bottom: 15px;
            font-size: 16px;
            line-height: 1.6;
        }
        .solution-content strong {
            font-weight: bold;
        }
        .solution-content code {
            background-color: var(--vscode-textCodeBlock-background);
            color: var(--vscode-textCodeBlock-foreground);
            padding: 2px 4px;
            border-radius: 3px;
            font-family: var(--vscode-editor-font-family);
        }
        .solution-content h1,
        .solution-content h2,
        .solution-content h3,
        .solution-content h4,
        .solution-content h5,
        .solution-content h6 {
            color: var(--vscode-textLink-foreground);
            margin-top: 20px;
            margin-bottom: 10px;
        }
        .solution-content h1 {
            font-size: 24px;
            border-bottom: 2px solid var(--vscode-textLink-foreground);
            padding-bottom: 5px;
        }
        .solution-content h2 {
            font-size: 20px;
            border-bottom: 2px solid var(--vscode-textLink-foreground);
            padding-bottom: 5px;
        }
        .solution-content h3 {
            font-size: 18px;
        }
        .solution-content ul,
        .solution-content ol {
            margin: 10px 0;
            padding-left: 25px;
        }
        .solution-content li {
            margin: 5px 0;
        }
    </style>
</head>
<body>
    <div class="header">
        <h1>📝 ${exercise.title}</h1>
        <span class="difficulty ${exercise.difficulty}">${exercise.difficulty}</span>
    </div>

    <div class="exercise-content">
        <h2>Problem Statement</h2>
        ${formattedDescription}
    </div>

    ${hintsHtml ? `
    <div class="hints-section">
        <h3>💡 Hints</h3>
        <ul>${hintsHtml}</ul>
    </div>
    ` : ''}

    <div class="actions">
        <button class="btn secondary" onclick="showSolution()">Show Solution</button>
        <button class="btn" onclick="openWorkbook()">📚 View Workbook</button>
        <button class="btn" onclick="takeQuiz()">🧠 Take Quiz</button>
    </div>

    <div class="solution-section" id="solution">
        <h3>✅ Solution</h3>
        <div class="solution-content">${formattedSolution}</div>
    </div>

    <script>
        const vscode = acquireVsCodeApi();

        function showSolution() {
            document.getElementById('solution').classList.add('show');
            // Re-render math expressions in the newly shown solution
            if (typeof window.renderMath === 'function') {
                window.renderMath();
            }
        }

        function openWorkbook() {
            vscode.postMessage({
                command: 'openWorkbook',
                workbookId: '${exercise.workbook ? exercise.workbook.replace(/HELM Workbook /, '').split(' ')[0] : ''}'
            });
        }

        function takeQuiz() {
            vscode.postMessage({
                command: 'takeQuiz',
                exerciseId: '${exercise.id}'
            });
        }

        // Render LaTeX math expressions
        document.addEventListener('DOMContentLoaded', function() {
            // Function to render math in the entire document
            function renderMath() {
                if (typeof renderMathInElement !== 'undefined') {
                    renderMathInElement(document.body, {
                        delimiters: [
                            {left: '$$', right: '$$', display: true},
                            {left: '$', right: '$', display: false},
                            {left: '\\\\(', right: '\\\\)', display: false},
                            {left: '\\\\[', right: '\\\\]', display: true}
                        ],
                        throwOnError: false
                    });
                }
            }
            
            // Initial render
            renderMath();
            
            // Make renderMath available globally for dynamic content
            window.renderMath = renderMath;
        });
    </script>
</body>
</html>`;
    }

    /**
     * Generate HTML for quiz viewer
     * @param {Object} quiz - Quiz object
     * @param {Object} webview - Webview instance for URI conversion
     * @returns {string} HTML string
     */
    getQuizHtml(quiz, webview) {
        // Process quiz questions for math rendering
        const processedQuestions = quiz.questions.map(question => ({
            ...question,
            question: this.processMathContent(this.markdownToHtml(question.question)),
            options: question.options.map(option => {
                // Only apply markdown processing if the option contains markdown syntax
                const processedOption = option.includes('*') || option.includes('`') || option.includes('\n') 
                    ? this.markdownToHtml(option) 
                    : option;
                return this.processMathContent(processedOption);
            }),
            explanation: question.explanation ? this.processMathContent(this.markdownToHtml(question.explanation)) : question.explanation
        }));

        const questionsHtml = processedQuestions.map((question, index) => {
            const optionsHtml = question.options.map((option, optIndex) =>
                `<label class="option" data-value="${optIndex}">
                    <input type="radio" name="q${index}" value="${optIndex}">
                    <span>${option}</span>
                </label>`
            ).join('');

            return `
                <div class="question" data-index="${index}">
                    <h3>Question ${index + 1}</h3>
                    <p>${question.question}</p>
                    <div class="options">
                        ${optionsHtml}
                    </div>
                    <div class="explanation" id="explanation-${index}" style="display: none;">
                        <div class="explanation-content">
                            <strong>Explanation:</strong> ${question.explanation || 'No explanation available.'}
                        </div>
                    </div>
                    <div class="question-actions">
                        <button class="btn check-btn" onclick="checkAnswer(${index})">Check Answer</button>
                        <div class="feedback" id="feedback-${index}"></div>
                    </div>
                </div>
            `;
        }).join('');

        // Prepare URIs for KaTeX assets. If a webview is provided, use local extension resources
        let katexCss = 'https://cdn.jsdelivr.net/npm/katex@0.16.9/dist/katex.min.css';
        let katexJs = 'https://cdn.jsdelivr.net/npm/katex@0.16.9/dist/katex.min.js';
        let autoRenderJs = 'https://cdn.jsdelivr.net/npm/katex@0.16.9/dist/contrib/auto-render.min.js';
        if (webview && this.vscode && this.vscode.Uri) {
            try {
                const cssPath = path.join(__dirname, '..', '..', 'resources', 'katex', 'katex.min.css');
                const jsPath = path.join(__dirname, '..', '..', 'resources', 'katex', 'katex.min.js');
                const autoRenderPath = path.join(__dirname, '..', '..', 'resources', 'katex', 'auto-render.min.js');
                katexCss = webview.asWebviewUri(this.vscode.Uri.file(cssPath));
                katexJs = webview.asWebviewUri(this.vscode.Uri.file(jsPath));
                autoRenderJs = webview.asWebviewUri(this.vscode.Uri.file(autoRenderPath));
            } catch (e) {
                // fallback to CDN if anything goes wrong
            }
        }

        return `<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>${quiz.title}</title>
    <link rel="stylesheet" href="${katexCss}">
    <script src="${katexJs}"></script>
    <script src="${autoRenderJs}"></script>
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
            max-width: 900px;
            margin: 0 auto;
        }
        .header {
            background-color: var(--vscode-titleBar-activeBackground);
            color: var(--vscode-titleBar-activeForeground);
            padding: 20px;
            border-radius: 8px;
            margin-bottom: 20px;
            border: 1px solid var(--vscode-titleBar-border);
            text-align: center;
        }
        .header h1 {
            font-size: 24px;
            font-weight: 600;
            margin-bottom: 10px;
        }
        .progress {
            background-color: var(--vscode-progressBar-background);
            height: 8px;
            border-radius: 4px;
            margin: 20px 0;
            overflow: hidden;
        }
        .progress-bar {
            height: 100%;
            background-color: var(--vscode-progressBar-foreground);
            width: 0%;
            transition: width 0.3s ease;
        }
        .question {
            background-color: var(--vscode-editorWidget-background);
            border: 1px solid var(--vscode-editorWidget-border);
            border-radius: 8px;
            padding: 20px;
            margin-bottom: 20px;
        }
        .question h3 {
            color: var(--vscode-textLink-foreground);
            font-size: 18px;
            margin-bottom: 15px;
        }
        .question p {
            font-size: 16px;
            margin-bottom: 15px;
        }
        .options {
            display: flex;
            flex-direction: column;
            gap: 10px;
        }
        .option {
            display: flex;
            align-items: center;
            padding: 12px;
            border: 1px solid var(--vscode-editorWidget-border);
            border-radius: 6px;
            cursor: pointer;
            transition: background-color 0.2s;
        }
        .option:hover {
            background-color: var(--vscode-list-hoverBackground);
        }
        .option input[type="radio"] {
            margin-right: 12px;
            accent-color: var(--vscode-textLink-foreground);
        }
        .option.correct {
            background-color: var(--vscode-charts-green);
            border-color: var(--vscode-charts-green);
            color: white;
        }
        .option.incorrect {
            background-color: var(--vscode-charts-red);
            border-color: var(--vscode-charts-red);
            color: white;
        }
        .option.correct.selected {
            background-color: var(--vscode-charts-green);
            border-color: var(--vscode-charts-green);
            color: white;
            font-weight: bold;
        }
        .option.incorrect.selected {
            background-color: var(--vscode-charts-red);
            border-color: var(--vscode-charts-red);
            color: white;
            font-weight: bold;
        }
        .explanation {
            margin-top: 15px;
            padding: 15px;
            background-color: var(--vscode-textBlockQuote-background);
            border: 1px solid var(--vscode-textBlockQuote-border);
            border-radius: 6px;
        }
        .explanation-content {
            color: var(--vscode-descriptionForeground);
            line-height: 1.5;
        }
        .question-actions {
            margin-top: 15px;
            text-align: center;
        }
        .check-btn {
            background-color: var(--vscode-button-secondaryBackground);
            color: var(--vscode-button-secondaryForeground);
            border: none;
            padding: 8px 16px;
            border-radius: 4px;
            cursor: pointer;
            font-size: 14px;
            margin-bottom: 10px;
        }
        .check-btn:hover {
            background-color: var(--vscode-button-secondaryHoverBackground);
        }
        .check-btn:disabled {
            opacity: 0.6;
            cursor: not-allowed;
        }
        .feedback {
            font-size: 14px;
            font-weight: 500;
            margin-top: 5px;
        }
        .feedback.correct {
            color: var(--vscode-charts-green);
        }
        .feedback.incorrect {
            color: var(--vscode-charts-red);
        }
        .actions {
            text-align: center;
            padding-top: 20px;
            border-top: 1px solid var(--vscode-textBlockQuote-border);
        }
        .btn {
            background-color: var(--vscode-button-background);
            color: var(--vscode-button-foreground);
            border: none;
            padding: 12px 24px;
            border-radius: 6px;
            cursor: pointer;
            font-size: 16px;
            font-weight: 500;
            margin: 0 10px;
            transition: background-color 0.2s;
        }
        .btn:hover {
            background-color: var(--vscode-button-hoverBackground);
        }
        .btn:disabled {
            opacity: 0.6;
            cursor: not-allowed;
        }
        .results {
            background-color: var(--vscode-textBlockQuote-background);
            border: 1px solid var(--vscode-textBlockQuote-border);
            border-radius: 8px;
            padding: 20px;
            margin-top: 20px;
            text-align: center;
            display: none;
        }
        .results.show {
            display: block;
        }
        .results h2 {
            color: var(--vscode-textLink-foreground);
            font-size: 24px;
            margin-bottom: 15px;
        }
        .score {
            font-size: 48px;
            font-weight: bold;
            margin: 20px 0;
        }
        .score.excellent { color: var(--vscode-charts-green); }
        .score.good { color: var(--vscode-charts-yellow); }
        .score.needs-work { color: var(--vscode-charts-red); }
    </style>
</head>
<body>
    <div class="header">
        <h1>🧠 ${quiz.title}</h1>
        <div class="progress">
            <div class="progress-bar" id="progressBar"></div>
        </div>
        <div id="questionCounter">Question 1 of ${quiz.questions.length}</div>
    </div>

    <div id="quizContent">
        ${questionsHtml}
    </div>

    <div class="actions">
        <button class="btn" id="prevBtn" onclick="previousQuestion()" disabled>Previous</button>
        <button class="btn" id="nextBtn" onclick="nextQuestion()">Next</button>
        <button class="btn" id="submitBtn" onclick="submitQuiz()" style="display: none;">Submit Quiz</button>
    </div>

    <div class="results" id="results">
        <h2>Quiz Complete!</h2>
        <div class="score" id="score"></div>
        <p id="feedback"></p>
        <button class="btn" onclick="restartQuiz()">Try Again</button>
        <button class="btn" onclick="openWorkbook()">📚 View Workbook</button>
    </div>

    <script>
        const vscode = acquireVsCodeApi();
        const quiz = ${JSON.stringify(quiz)};
        let currentQuestion = 0;
        let answers = new Array(quiz.questions.length).fill(null);
        let checkedAnswers = new Array(quiz.questions.length).fill(false);
        let showResults = false;

        // Render LaTeX math expressions
        document.addEventListener('DOMContentLoaded', function() {
            // Function to render math in the entire document
            function renderMath() {
                if (typeof renderMathInElement !== 'undefined') {
                    renderMathInElement(document.body, {
                        delimiters: [
                            {left: '$$', right: '$$', display: true},
                            {left: '$', right: '$', display: false},
                            {left: '\\\\(', right: '\\\\)', display: false},
                            {left: '\\\\[', right: '\\\\]', display: true}
                        ],
                        throwOnError: false
                    });
                }
            }
            
            // Initial render
            renderMath();
            
            // Make renderMath available globally for dynamic content
            window.renderMath = renderMath;
        });

        function updateUI() {
            const progress = ((currentQuestion + 1) / quiz.questions.length) * 100;
            document.getElementById('progressBar').style.width = progress + '%';
            document.getElementById('questionCounter').textContent = \`Question \${currentQuestion + 1} of \${quiz.questions.length}\`;

            // Show/hide questions
            document.querySelectorAll('.question').forEach((q, index) => {
                q.style.display = index === currentQuestion ? 'block' : 'none';
            });

            // Update navigation buttons
            document.getElementById('prevBtn').disabled = currentQuestion === 0;
            document.getElementById('nextBtn').style.display = currentQuestion === quiz.questions.length - 1 ? 'none' : 'inline-block';
            document.getElementById('submitBtn').style.display = currentQuestion === quiz.questions.length - 1 ? 'inline-block' : 'none';

            // Show results if quiz is complete
            document.getElementById('results').style.display = showResults ? 'block' : 'none';
            document.getElementById('quizContent').style.display = showResults ? 'none' : 'block';
            document.querySelector('.actions').style.display = showResults ? 'none' : 'block';
        }

        function checkAnswer(questionIndex) {
            const question = quiz.questions[questionIndex];
            const selectedOption = document.querySelector(\`input[name="q\${questionIndex}"]:checked\`);
            
            if (!selectedOption) {
                document.getElementById(\`feedback-\${questionIndex}\`).textContent = 'Please select an answer first.';
                document.getElementById(\`feedback-\${questionIndex}\`).className = 'feedback';
                return;
            }

            const selectedValue = parseInt(selectedOption.value);
            const isCorrect = selectedValue === question.correct;
            
            // Update answers array
            answers[questionIndex] = selectedValue;
            checkedAnswers[questionIndex] = true;
            
            // Highlight options
            const options = document.querySelectorAll(\`input[name="q\${questionIndex}"]\`);
            options.forEach((option, index) => {
                const label = option.parentElement;
                if (index === question.correct) {
                    label.classList.add('correct');
                    if (index === selectedValue) {
                        label.classList.add('selected');
                    }
                } else if (index === selectedValue && !isCorrect) {
                    label.classList.add('incorrect', 'selected');
                }
            });
            
            // Show explanation
            document.getElementById(\`explanation-\${questionIndex}\`).style.display = 'block';
            
            // Show feedback
            const feedback = document.getElementById(\`feedback-\${questionIndex}\`);
            if (isCorrect) {
                feedback.textContent = '✓ Correct!';
                feedback.className = 'feedback correct';
            } else {
                feedback.textContent = '✗ Incorrect. The correct answer is highlighted above.';
                feedback.className = 'feedback incorrect';
            }
            
            // Disable check button
            const checkBtn = document.querySelector(\`.question[data-index="\${questionIndex}"] .check-btn\`);
            checkBtn.disabled = true;
            checkBtn.textContent = 'Answer Checked';
        }

        function nextQuestion() {
            if (currentQuestion < quiz.questions.length - 1) {
                currentQuestion++;
                updateUI();
            }
        }

        function previousQuestion() {
            if (currentQuestion > 0) {
                currentQuestion--;
                updateUI();
            }
        }

        function submitQuiz() {
            let correct = 0;
            answers.forEach((answer, index) => {
                if (answer === quiz.questions[index].correct) {
                    correct++;
                }
            });

            const percentage = Math.round((correct / quiz.questions.length) * 100);
            const scoreElement = document.getElementById('score');
            const feedbackElement = document.getElementById('feedback');

            scoreElement.textContent = \`\${percentage}%\`;
            scoreElement.className = 'score';

            if (percentage >= 90) {
                scoreElement.classList.add('excellent');
                feedbackElement.textContent = 'Excellent work! You have a strong understanding of the material.';
            } else if (percentage >= 70) {
                scoreElement.classList.add('good');
                feedbackElement.textContent = 'Good job! Review the areas you missed and try again.';
            } else {
                scoreElement.classList.add('needs-work');
                feedbackElement.textContent = 'Keep practicing! Review the workbook and try the quiz again.';
            }

            showResults = true;
            updateUI();
        }

        function restartQuiz() {
            currentQuestion = 0;
            answers = new Array(quiz.questions.length).fill(null);
            checkedAnswers = new Array(quiz.questions.length).fill(false);
            showResults = false;
            
            // Reset all UI elements
            document.querySelectorAll('.option').forEach(option => {
                option.className = 'option';
            });
            document.querySelectorAll('.explanation').forEach(explanation => {
                explanation.style.display = 'none';
            });
            document.querySelectorAll('.feedback').forEach(feedback => {
                feedback.textContent = '';
                feedback.className = 'feedback';
            });
            document.querySelectorAll('.check-btn').forEach(btn => {
                btn.disabled = false;
                btn.textContent = 'Check Answer';
            });
            document.querySelectorAll('input[type="radio"]').forEach(radio => {
                radio.checked = false;
            });
            
            updateUI();
        }

        function openWorkbook() {
            vscode.postMessage({
                command: 'openWorkbook'
            });
        }

        // Handle radio button changes
        document.addEventListener('change', (e) => {
            if (e.target.type === 'radio') {
                const questionIndex = parseInt(e.target.name.replace('q', ''));
                answers[questionIndex] = parseInt(e.target.value);
            }
        });

        updateUI();
    </script>
</body>
</html>`;
    }

    /**
     * Process mathematical content to make it KaTeX-compatible
     * @param {string} content - HTML content with math expressions
     * @returns {string} Processed content with LaTeX delimiters
     */
    processMathContent(content) {
        // Use Ruby CLI for math preprocessing (better regex support)
        try {
            const { execSync } = require('child_process');
            const rubyScript = `ruby ${__dirname}/../../core/lib/tsi_header_cli.rb preprocess_math`;
            const result = execSync(rubyScript, {
                input: content,
                encoding: 'utf8',
                timeout: 5000
            });

            const parsed = JSON.parse(result);
            if (parsed.success) {
                return parsed.processed_content;
            } else {
                console.warn('Ruby math preprocessing failed:', parsed.message);
                return content; // Fallback to original content
            }
        } catch (error) {
            console.warn('Failed to use Ruby math preprocessor, falling back to JavaScript:', error.message);
            // Fallback to basic JavaScript processing
            return this.fallbackMathProcessing(content);
        }
    }

    /**
     * Fallback math processing in JavaScript (simplified version)
     * @param {string} content - Content to process
     * @returns {string} Processed content
     */
    fallbackMathProcessing(content) {
        let processed = content;

        // Basic Unicode replacements
        const unicodeReplacements = {
            '√': '\\sqrt',
            'π': '\\pi',
            '∞': '\\infty',
            '≤': '\\leq',
            '≥': '\\geq',
            '≠': '\\neq'
        };

        for (const [unicode, latex] of Object.entries(unicodeReplacements)) {
            processed = processed.replace(new RegExp(unicode, 'g'), latex);
        }

        // Basic superscript conversion
        processed = processed.replace(/([a-zA-Z0-9]+)\^([a-zA-Z0-9\-+]+)(?![}\]])/g, '$1^{$2}');

        // Basic math expression wrapping
        processed = processed.replace(/\b(dy\/dx|dx\/dt)\b/g, '$$$1$$');
        processed = processed.replace(/\b([xy]\s*=\s*[^,\n]+(?:,\s*[xy]\s*=\s*[^,\n]+)*)/g, '$$$1$$');

        return processed;
    }

    /**
     * Simple markdown to HTML converter
     * @param {string} markdown - Markdown content
     * @returns {string} HTML content
     */
    markdownToHtml(markdown) {
        let html = markdown;

        // Convert code blocks first (before other replacements)
        // Preserve language class for highlight.js and avoid wrapping individual lines so the highlighter can parse them
        html = html.replace(/```(\w+)?\n([\s\S]*?)```/g, (match, lang, code) => {
            const language = (lang || '').trim();
            const langClass = language ? `language-${language}` : 'language-plaintext';
            // Escape HTML inside code block but keep original newlines. Normalize excessive blank lines.
            const escaped = this.escapeHtml(code).replace(/\n{3,}/g, '\n\n');
            return `<pre><code class="${langClass}">${escaped}</code></pre>`;
        });

        // Convert images
        html = html.replace(/!\[(.*?)\]\((.*?)\)/g, (match, alt, src) => {
            const rawSource = (src || '').trim();
            const altText = (alt || '').trim();
            const safeAlt = this.escapeHtml(altText);
            const safeSource = this.escapeHtml(rawSource);
            return `<img src="${safeSource}" alt="${safeAlt}" data-tsi-src="${safeSource}">`;
        });

        // --- Protect fenced code blocks from downstream replacements ---
        // Extract any generated <pre><code>...</code></pre> blocks and replace
        // them with stable placeholders so subsequent regexes don't alter
        // their inner content (this prevents paragraph-wrapping from
        // collapsing or concatenating code lines).
        const codeBlockPlaceholders = [];
        html = html.replace(/<pre><code[\s\S]*?<\/code><\/pre>/g, (match) => {
            const idx = codeBlockPlaceholders.length;
            codeBlockPlaceholders.push(match);
            return `@@CODEBLOCK_${idx}@@`;
        });

        // Convert tables
        html = html.replace(/(\|.*\|\n\|[\s\-\|:]+\|\n(?:\|.*\|\n?)*)/g, (match) => {
            const lines = match.trim().split('\n');
            if (lines.length < 2) return match;

            // Parse header row
            const headerLine = lines[0];
            const separatorLine = lines[1];
            const dataLines = lines.slice(2);

            // Check if this is a valid table (separator line should contain | and - or : characters)
            if (!separatorLine.match(/\|[\s\-\|:]+\|/)) return match;

            // Parse header cells
            const headers = headerLine.split('|').slice(1, -1).map(cell => cell.trim());

            // Parse data rows
            const rows = dataLines.map(line => {
                return line.split('|').slice(1, -1).map(cell => cell.trim());
            });

            // Generate HTML table
            let tableHtml = '<table>\n';

            // Add header row
            tableHtml += '<thead>\n<tr>\n';
            headers.forEach(header => {
                tableHtml += `<th>${header}</th>\n`;
            });
            tableHtml += '</tr>\n</thead>\n';

            // Add data rows
            if (rows.length > 0) {
                tableHtml += '<tbody>\n';
                rows.forEach(row => {
                    tableHtml += '<tr>\n';
                    row.forEach(cell => {
                        tableHtml += `<td>${cell}</td>\n`;
                    });
                    tableHtml += '</tr>\n';
                });
                tableHtml += '</tbody>\n';
            }

            tableHtml += '</table>';
            return tableHtml;
        });

        // --- Protect table HTML from paragraph conversion ---
        const tablePlaceholders = [];
        html = html.replace(/<table[\s\S]*?<\/table>/g, (match) => {
            const idx = tablePlaceholders.length;
            tablePlaceholders.push(match);
            return `@@TABLE_${idx}@@`;
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

        // Restore protected code blocks
        html = html.replace(/@@CODEBLOCK_(\d+)@@/g, (m, id) => {
            const i = parseInt(id, 10);
            return codeBlockPlaceholders[i] || '';
        });

        // Restore protected tables
        html = html.replace(/@@TABLE_(\d+)@@/g, (m, id) => {
            const i = parseInt(id, 10);
            return tablePlaceholders[i] || '';
        });

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
}

module.exports = MathematicsManager;