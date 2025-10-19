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
     * Open an exercise in webview
     * @param {Object} exercise - Exercise object
     * @returns {Promise<void>}
     */
    async openExercise(exercise) {
        try {
            const panel = this.vscode.window.createWebviewPanel(
                'tsiMathematicsExercise',
                `� ${exercise.title}`,
                this.vscode.ViewColumn.One,
                {
                    enableScripts: true,
                    retainContextWhenHidden: true
                }
            );

            panel.webview.html = this.getExerciseHtml(exercise);

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
            const panel = this.vscode.window.createWebviewPanel(
                'tsiMathematicsQuiz',
                `🧠 ${quiz.title}`,
                this.vscode.ViewColumn.One,
                {
                    enableScripts: true,
                    retainContextWhenHidden: true
                }
            );

            panel.webview.html = this.getQuizHtml(quiz);

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
     * @returns {string} HTML string
     */
    getExerciseHtml(exercise) {
        const formattedDescription = exercise.description
            .replace(/\n\n/g, '</p><p>')
            .replace(/\n/g, '<br>')
            .replace(/\*\*(.+?)\*\*/g, '<strong>$1</strong>')
            .replace(/`(.+?)`/g, '<code>$1</code>');

        const hintsHtml = exercise.hints ? exercise.hints.map(hint =>
            `<li>${hint}</li>`
        ).join('') : '';

        return `<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>${exercise.title}</title>
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
        }
        .math-expression {
            background-color: var(--vscode-textBlockQuote-background);
            border-left: 4px solid var(--vscode-textLink-foreground);
            padding: 15px 20px;
            margin: 20px 0;
            font-family: 'Times New Roman', serif;
            font-size: 16px;
            white-space: pre-wrap;
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
            font-family: 'Courier New', monospace;
            white-space: pre-wrap;
            color: var(--vscode-editor-foreground);
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
        <p>${formattedDescription}</p>
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
        <div class="solution-content">${exercise.solution || 'Solution not available yet.'}</div>
    </div>

    <script>
        const vscode = acquireVsCodeApi();

        function showSolution() {
            document.getElementById('solution').classList.add('show');
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
    </script>
</body>
</html>`;
    }

    /**
     * Generate HTML for quiz viewer
     * @param {Object} quiz - Quiz object
     * @returns {string} HTML string
     */
    getQuizHtml(quiz) {
        const questionsHtml = quiz.questions.map((question, index) => {
            const optionsHtml = question.options.map((option, optIndex) =>
                `<label class="option">
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
                </div>
            `;
        }).join('');

        return `<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>${quiz.title}</title>
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
        }
        .option.incorrect {
            background-color: var(--vscode-charts-red);
            border-color: var(--vscode-charts-red);
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
        let showResults = false;

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
            showResults = false;
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
     * Clear curriculum cache
     */
    clearCurriculumCache() {
        this.curriculumCache.clear();
    }
}

module.exports = MathematicsManager;