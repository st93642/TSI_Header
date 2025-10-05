/**
 * TSI Header - Learn Mode
 * 
 * Interactive learning platform for programming languages within VS Code.
 * Provides structured lessons, exercises, and progress tracking.
 * 
 * @module learn
 */

const path = require('path');
const LearnManager = require('./lib/learn_manager');
const ProgressTracker = require('./lib/progress_tracker');
const ExerciseRunner = require('./lib/exercise_runner');

class Learn {
    constructor(context, vscode) {
        this.context = context;
        this.vscode = vscode;
        
    this.progressTracker = new ProgressTracker(context);
    this.learnManager = new LearnManager(context, vscode, this.progressTracker);
        this.exerciseRunner = new ExerciseRunner(vscode);
        
        // Track solution documents to avoid save prompts
        this.solutionDocuments = new Set();
        
        // Handle document save attempts for solution documents
        this.saveListener = this.vscode.workspace.onWillSaveTextDocument(e => {
            if (this.solutionDocuments.has(e.document.uri.toString())) {
                // For solution documents, don't save - just close
                e.waitUntil(Promise.resolve([]));
            }
        });
        
        // Clean up solution document tracking on close
        this.closeListener = this.vscode.workspace.onDidCloseTextDocument(doc => {
            this.solutionDocuments.delete(doc.uri.toString());
        });
        
        this.context.subscriptions.push(this.saveListener, this.closeListener);
    }

    /**
     * Start a learning session for a specific language
     * @param {string} language - The programming language to learn
     * @returns {Promise<void>}
     */
    async startLearning(language) {
        try {
            // Load language curriculum
            const curriculum = await this.learnManager.loadCurriculum(language);
            
            // Get current progress
            const progress = await this.progressTracker.getProgress(language);
            
            // Determine next lesson
            const nextLesson = this.learnManager.getNextLesson(curriculum, progress);
            
            if (!nextLesson) {
                this.vscode.window.showInformationMessage(
                    `🎉 Congratulations! You've completed the ${language} curriculum!`,
                    { modal: true },
                    'Review Lessons',
                    'Take Assessment'
                ).then(selection => {
                    if (selection === 'Review Lessons') {
                        this.reviewLessons(language);
                    } else if (selection === 'Take Assessment') {
                        this.startAssessment(language);
                    }
                });
                return;
            }
            
            // Open lesson view
            await this.learnManager.openLesson(language, nextLesson);
            
        } catch (error) {
            this.vscode.window.showErrorMessage(
                `Failed to start learning: ${error.message}`,
                { modal: true },
                'Got it!'
            );
        }
    }

    /**
     * Run an exercise
     * @param {string} language - The programming language
     * @param {Object} exercise - The exercise to run
     * @returns {Promise<Object>} Exercise results
     */
    async runExercise(language, exercise) {
        try {
            const runtimeLanguage = (exercise.language || language || '').toLowerCase();
            const result = await this.exerciseRunner.run(runtimeLanguage || language, exercise);
            
            // Handle manual exercises (no automated tests)
            if (result.isManual) {
                this.vscode.window.showInformationMessage(
                    `📝 Manual Exercise: "${exercise.title}"\n\n${result.message}`,
                    { modal: true },
                    'Mark Complete',
                    'Cancel'
                ).then(async selection => {
                    if (selection === 'Mark Complete') {
                        await this.progressTracker.recordCompletion(language, exercise.id, {
                            lessonId: exercise.lessonId,
                            baseExerciseId: exercise.baseExerciseId || exercise.baseExerciseFile
                        });
                        
                        // Get next lesson in curriculum
                        const curriculum = await this.learnManager.loadCurriculum(language);
                        const progress = await this.progressTracker.getProgress(language);
                        const nextLesson = this.learnManager.getNextLesson(curriculum, progress);
                        
                        this.vscode.window.showInformationMessage(
                            `✅ Exercise "${exercise.title}" marked as complete!`,
                            { modal: true },
                            nextLesson ? 'Next Lesson' : 'View Progress',
                            'Browse Lessons'
                        ).then(async selection => {
                            if (selection === 'Next Lesson') {
                                // Open next lesson
                                await this.learnManager.openLesson(language, nextLesson);
                            } else if (selection === 'View Progress') {
                                // Show completion stats
                                const stats = await this.getStats(language);
                                this.vscode.window.showInformationMessage(
                                    `🎉 Curriculum Complete!\n\n📚 Lessons: ${stats.lessonsCompleted}\n✅ Exercises: ${stats.exercisesCompleted}\n🔥 Streak: ${stats.currentStreak} days`,
                                    { modal: true },
                                    'Got it!'
                                );
                            } else if (selection === 'Browse Lessons') {
                                this.browseLessons(language);
                            }
                        });
                    }
                });
                return result;
            }
            
            // Update progress if passed
            if (result.passed) {
                const progressLanguage = language;
                const progressId = exercise.progressId || exercise.id;
                await this.progressTracker.recordCompletion(progressLanguage, progressId, {
                    lessonId: exercise.lessonId,
                    baseExerciseId: exercise.baseExerciseId || exercise.baseExerciseFile
                });
                
                // Get next lesson in curriculum
                const curriculumLanguage = exercise.curriculumLanguage || language;
                const curriculum = await this.learnManager.loadCurriculum(curriculumLanguage);
                const progress = await this.progressTracker.getProgress(language);
                const nextLesson = this.learnManager.getNextLesson(curriculum, progress);
                
                // Show success message
                this.vscode.window.showInformationMessage(
                    `✅ Exercise "${exercise.title}" completed! ${result.score}/${result.total} tests passed.`,
                    { modal: true },
                    nextLesson ? 'Next Lesson' : 'View Progress',
                    'Review Solution'
                ).then(async selection => {
                    if (selection === 'Next Lesson') {
                        // Open next lesson
                        await this.learnManager.openLesson(language, nextLesson);
                    } else if (selection === 'View Progress') {
                        // Show completion stats
                        const stats = await this.getStats(language);
                        this.vscode.window.showInformationMessage(
                            `🎉 Curriculum Complete!\n\n📚 Lessons: ${stats.lessonsCompleted}\n✅ Exercises: ${stats.exercisesCompleted}\n🔥 Streak: ${stats.currentStreak} days`,
                            { modal: true },
                            'Got it!'
                        );
                    } else if (selection === 'Review Solution') {
                        this.showSolution(curriculumLanguage, exercise);
                    }
                });
            } else {
                // Show failure message with detailed results
                const failedCount = result.failedTests || result.total - result.score;
                
                // Format failure messages
                let failureDetails = '';
                if (result.failures && result.failures.length > 0) {
                    failureDetails = '\n\nFailures:\n' + result.failures.map((f, i) => 
                        `${i + 1}. ${f.message || 'Test failed'}`
                    ).join('\n');
                }
                
                this.vscode.window.showWarningMessage(
                    `❌ ${failedCount} test(s) failed${failureDetails}\n\n${result.hint || 'Review the differences and try again!'}`,
                    { modal: true },
                    'Try Again',
                    'Show Hint',
                    'View Solution'
                ).then(selection => {
                    if (selection === 'Show Hint') {
                        this.showHint(exercise);
                    } else if (selection === 'View Solution') {
                        this.showSolution(exercise.curriculumLanguage || language, exercise);
                    }
                });
            }
            
            return result;
            
        } catch (error) {
            this.vscode.window.showErrorMessage(
                `Exercise failed: ${error.message}`,
                { modal: true },
                'Got it!'
            );
            throw error;
        }
    }

    /**
     * Show a hint for an exercise
     * @param {Object} exercise - The exercise
     */
    async showHint(exercise) {
        const hints = exercise.hints || [];
        const currentHintIndex = this.context.globalState.get(`hint_${exercise.id}`, 0);
        
        if (currentHintIndex < hints.length) {
            const hint = hints[currentHintIndex];
            await this.vscode.window.showInformationMessage(
                `💡 Hint ${currentHintIndex + 1}/${hints.length}: ${hint}`,
                { modal: true },
                'Got it!',
                'Next Hint'
            ).then(selection => {
                if (selection === 'Next Hint') {
                    this.context.globalState.update(`hint_${exercise.id}`, currentHintIndex + 1);
                    this.showHint(exercise);
                }
            });
        } else {
            this.vscode.window.showInformationMessage(
                'No more hints available. Try viewing the solution!',
                { modal: true },
                'Got it!'
            );
        }
    }

    getExerciseMetadata(filePath) {
        return this.learnManager.getExerciseMetadata(filePath);
    }

    /**
     * Show the solution for an exercise
     * @param {string} language - The programming language
     * @param {Object} exercise - The exercise
     */
    async showSolution(language, exercise) {
        try {
            const baseExerciseId = exercise.baseExerciseId || exercise.id;
            const solution = await this.learnManager.loadSolution(
                exercise.curriculumLanguage || language,
                baseExerciseId,
                exercise.variantId || exercise.id
            );
            
            // Create a webview panel to display the solution
            const resourceRoots = [];
            if (this.vscode && this.vscode.Uri && typeof this.vscode.Uri.file === 'function') {
                const resourcesRoot = path.join(__dirname, '..', 'resources');
                resourceRoots.push(this.vscode.Uri.file(resourcesRoot));
            }

            const solutionPanelOptions = {
                enableScripts: true,
                retainContextWhenHidden: true
            };

            if (resourceRoots.length > 0) {
                solutionPanelOptions.localResourceRoots = resourceRoots;
            }

            const panel = this.vscode.window.createWebviewPanel(
                'tsiSolution',
                `Solution: ${exercise.title}`,
                this.vscode.ViewColumn.One,
                solutionPanelOptions
            );
            
            // Set HTML content with syntax highlighting using local extension resources
            panel.webview.html = this.getSolutionHtml(solution, exercise, panel.webview);
            
            // Get next lesson in curriculum for the "Next Lesson" button
            const curriculumLanguage = exercise.curriculumLanguage || language;
            const curriculum = await this.learnManager.loadCurriculum(curriculumLanguage);
            const progress = await this.progressTracker.getProgress(curriculumLanguage);
            const currentLessonId = this.progressTracker.normalizeLessonId(
                exercise.lessonId || exercise.baseExerciseId || baseExerciseId
            );
            const nextLesson = this.learnManager.getNextLessonForSolution(
                curriculum,
                progress,
                currentLessonId
            );
            
            // Handle messages from webview
            panel.webview.onDidReceiveMessage(
                async message => {
                    switch (message.command) {
                        case 'nextLesson':
                            if (nextLesson) {
                                panel.dispose();
                                await this.learnManager.openLesson(curriculumLanguage, nextLesson);
                            } else {
                                this.vscode.window.showInformationMessage(
                                    'You have completed the current curriculum. Great job!',
                                    { modal: true },
                                    'Browse Lessons'
                                ).then(selection => {
                                    if (selection === 'Browse Lessons') {
                                        this.browseLessons(curriculumLanguage);
                                    }
                                });
                            }
                            break;
                        case 'browseLessons':
                            panel.dispose();
                            this.browseLessons(curriculumLanguage);
                            break;
                    }
                },
                undefined,
                this.context.subscriptions
            );
            
            // Show explanation dialog
            if (solution.explanation) {
                this.vscode.window.showInformationMessage(
                    `📖 Solution Explanation: ${solution.explanation}`,
                    { modal: true },
                    nextLesson ? 'Next Lesson' : 'Browse Lessons',
                    'Got it!'
                ).then(async selection => {
                    if (selection === 'Next Lesson') {
                        panel.dispose();
                        await this.learnManager.openLesson(curriculumLanguage, nextLesson);
                    } else if (selection === 'Browse Lessons') {
                        panel.dispose();
                        this.browseLessons(curriculumLanguage);
                    }
                });
            }
            
        } catch (error) {
            this.vscode.window.showErrorMessage(
                `Failed to load solution: ${error.message}`,
                { modal: true },
                'Got it!'
            );
        }
    }

    /**
     * Review completed lessons
     * @param {string} language - The programming language
     */
    async reviewLessons(language) {
        const progress = await this.progressTracker.getProgress(language);
        const completedLessons = progress.completed || [];
        
        if (completedLessons.length === 0) {
            this.vscode.window.showInformationMessage(
                'No completed lessons to review yet.',
                { modal: true },
                'Got it!'
            );
            return;
        }
        
        const curriculum = await this.learnManager.loadCurriculum(language);
        const sections = this.learnManager.getCurriculumSections(curriculum);

        const lessonLookup = new Map();
        for (const section of sections) {
            const lessons = Array.isArray(section.lessons) ? section.lessons : [];
            for (const lesson of lessons) {
                lessonLookup.set(lesson.id, {
                    lesson,
                    sectionTitle: section.title,
                    sectionId: section.id
                });
            }
        }

        // Show quick pick of completed lessons with friendly titles
        const items = completedLessons.map(lessonId => {
            const info = lessonLookup.get(lessonId);
            return {
                label: info ? `📘 ${info.lesson.title}` : `Lesson: ${lessonId}`,
                description: info ? info.sectionTitle : 'Click to review',
                lessonId,
                lessonInfo: info
            };
        });
        
        const selected = await this.vscode.window.showQuickPick(items, {
            placeHolder: 'Select a lesson to review'
        });
        
        if (selected) {
            const info = selected.lessonInfo;
            const lessonToOpen = info?.lesson || lessonLookup.get(selected.lessonId)?.lesson;
            const sectionTitle = info?.sectionTitle || lessonLookup.get(selected.lessonId)?.sectionTitle || '';
            const sectionId = info?.sectionId || lessonLookup.get(selected.lessonId)?.sectionId || '';

            if (lessonToOpen) {
                await this.learnManager.openLesson(language, {
                    ...lessonToOpen,
                    sectionTitle,
                    sectionId
                });
            } else {
                this.vscode.window.showErrorMessage(
                    `Lesson "${selected.lessonId}" could not be found in the curriculum.`,
                    { modal: true },
                    'Got it!'
                );
            }
        }
    }

    /**
     * Browse and jump to any lesson in the curriculum
     * @param {string} language - The programming language
     */
    async browseLessons(language) {
        try {
            // Clear curriculum cache to ensure fresh data
            this.learnManager.clearCurriculumCache();
            
            // Load curriculum
            const curriculum = await this.learnManager.loadCurriculum(language);
            const sections = this.learnManager.getCurriculumSections(curriculum);
            const progress = await this.progressTracker.getProgress(language);
            const completedLessons = new Set(progress.completed || []);
            
            // Build lesson list grouped by section (module or chapter)
            const items = [];
            
            for (const section of sections) {
                const lessons = Array.isArray(section.lessons) ? section.lessons : [];

                // Add section header as separator
                items.push({
                    label: `� ${section.title}`,
                    kind: this.vscode.QuickPickItemKind.Separator
                });
                
                for (const lesson of lessons) {
                    const isCompleted = completedLessons.has(lesson.id);
                    const icon = isCompleted ? '✓' : '○';
                    const status = isCompleted ? 'Completed' : 'Not started';
                    
                    items.push({
                        label: `  ${icon} ${lesson.title}`,
                        description: `${lesson.duration || '?'} min • ${lesson.difficulty || 'beginner'}`,
                        detail: status,
                        lessonId: lesson.id,
                        sectionTitle: section.title,
                        sectionId: section.id,
                        lesson: lesson
                    });
                }
            }
            
            // Show quick pick
            const selected = await this.vscode.window.showQuickPick(items, {
                placeHolder: 'Select a lesson to jump to',
                matchOnDescription: true,
                matchOnDetail: true
            });
            
            if (selected && selected.lessonId) {
                await this.learnManager.openLesson(language, {
                    ...selected.lesson,
                    sectionTitle: selected.sectionTitle,
                    sectionId: selected.sectionId
                });
            }
            
        } catch (error) {
            this.vscode.window.showErrorMessage(
                `Failed to browse lessons: ${error.message}`,
                { modal: true },
                'Got it!'
            );
        }
    }

    /**
     * Start a comprehensive assessment
     * @param {string} language - The programming language
     */
    async startAssessment(language) {
        this.vscode.window.showInformationMessage(
            '🎯 Assessment feature coming soon! This will test your knowledge across all topics.',
            { modal: true }
        );
    }

    /**
     * Get learning statistics
     * @param {string} language - The programming language
     * @returns {Promise<Object>} Statistics object
     */
    async getStats(language) {
        return await this.progressTracker.getStats(language);
    }

    /**
     * Generate HTML for solution display
     * @param {Object} solution - Solution object
     * @param {Object} exercise - Exercise object
     * @returns {string} HTML string
     */
    getSolutionHtml(solution, exercise, webview) {
        // Get next lesson in curriculum for the button
        const curriculum = this.learnManager.loadCurriculum('ruby').then(curriculum => {
            const progress = this.progressTracker.getProgress('ruby');
            return this.learnManager.getNextLesson(curriculum, progress);
        });
        
        // Prepare URIs for highlight assets. If a webview is provided, use local extension resources
        let highlightCss = 'https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.8.0/styles/github-dark.min.css';
        let highlightJs = 'https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.8.0/highlight.min.js';
        if (webview && this.vscode && this.vscode.Uri) {
            try {
                const cssPath = this.vscode.Uri.file(path.join(__dirname, '..', 'resources', 'highlightjs', 'github-dark.min.css'));
                const jsPath = this.vscode.Uri.file(path.join(__dirname, '..', 'resources', 'highlightjs', 'highlight.min.js'));
                highlightCss = webview.asWebviewUri(cssPath);
                highlightJs = webview.asWebviewUri(jsPath);
            } catch (e) {
                // fallback to CDN if anything goes wrong
            }
        }

        return `<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Solution: ${exercise.title}</title>
    <!-- Highlight.js styles and script (local extension resource when available, otherwise CDN) -->
    <link rel="stylesheet" href="${highlightCss}">
    <script src="${highlightJs}"></script>
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
        }
        h1 {
            color: var(--vscode-textLink-foreground);
            border-bottom: 2px solid var(--vscode-textLink-foreground);
            padding-bottom: 10px;
            font-size: 24px;
            font-weight: 600;
            margin-bottom: 20px;
        }
        .solution-container {
            background-color: var(--vscode-textCodeBlock-background);
            border-radius: 5px;
            padding: 20px;
            margin: 20px 0;
            border: 1px solid var(--vscode-textBlockQuote-border);
        }
        .solution-header {
            color: var(--vscode-textLink-activeForeground);
            font-size: 16px;
            font-weight: 600;
            margin-bottom: 15px;
        }
        pre {
            background-color: var(--vscode-textCodeBlock-background);
            padding: 15px;
            border-radius: 5px;
            overflow-x: auto;
            margin: 15px 0;
            line-height: 1.5;
            border: 1px solid var(--vscode-textBlockQuote-border);
        }
        code {
            font-family: var(--vscode-editor-font-family);
            font-size: 13px;
            line-height: 1.5;
            color: var(--vscode-textPreformat-foreground);
            display: block;
        }
        .code-comment {
            color: var(--vscode-editorLineNumber-foreground);
            font-style: italic;
        }
        .button-container {
            margin-top: 30px;
            text-align: center;
        }
        .solution-button {
            background-color: var(--vscode-button-background);
            color: var(--vscode-button-foreground);
            border: none;
            padding: 10px 20px;
            margin: 0 5px;
            border-radius: 3px;
            cursor: pointer;
            font-size: 14px;
            font-weight: 500;
        }
        .solution-button:hover {
            background-color: var(--vscode-button-hoverBackground);
        }
        .next-button {
            background-color: var(--vscode-button-secondaryBackground);
            color: var(--vscode-button-secondaryForeground);
        }
        .explanation {
            background-color: var(--vscode-textBlockQuote-background);
            border-left: 4px solid var(--vscode-textLink-foreground);
            padding: 15px 20px;
            margin: 20px 0;
            font-size: 14px;
            line-height: 1.6;
        }
    </style>
</head>
<body>
    <h1>📖 Solution: ${exercise.title}</h1>
    
    <div class="explanation">
        <strong>Explanation:</strong> ${solution.explanation || 'Complete solution for this exercise.'}
    </div>
    
        <div class="solution-container">
        <div class="solution-header">Complete Solution Code:</div>
        <pre><code class="language-${(exercise.language || 'plaintext')}">${this.escapeHtml(solution.code)}</code></pre>
    </div>
    
    <div class="button-container">
        <button class="solution-button next-button" onclick="nextLesson()">
            📚 Next Lesson
        </button>
        <button class="solution-button" onclick="browseLessons()">
            🔍 Browse All Lessons
        </button>
    </div>
    
    <script>
        const vscode = acquireVsCodeApi();
        
        function nextLesson() {
            vscode.postMessage({
                command: 'nextLesson'
            });
        }
        
        function browseLessons() {
            vscode.postMessage({
                command: 'browseLessons'
            });
        }

        // Highlight code blocks when the DOM is ready
        document.addEventListener('DOMContentLoaded', () => {
            try {
                if (window.hljs && typeof hljs.highlightAll === 'function') hljs.highlightAll();
            } catch (e) {
                // ignore highlight errors
            }
        });
    </script>
</body>
</html>`;
    }

    /**
     * Escape HTML special characters for code display
     * @param {string} text - Text to escape
     * @returns {string} Escaped text
     */
    escapeHtml(text) {
        return text
            .replace(/&/g, '&amp;')
            .replace(/</g, '&lt;')
            .replace(/>/g, '&gt;');
            // Don't escape quotes - they should display normally in <pre><code>
    }

    /**
     * Dispose of resources
     */
    dispose() {
        if (this.saveListener) {
            this.saveListener.dispose();
        }
        if (this.closeListener) {
            this.closeListener.dispose();
        }
        // Cleanup if needed
    }
}

module.exports = Learn;
