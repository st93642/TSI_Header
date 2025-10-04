/**
 * Learn Manager - Handles curriculum loading and lesson management
 */

const path = require('path');
const fs = require('fs').promises;
const ProgressTracker = require('./progress_tracker');

class LearnManager {
    constructor(context, vscode, progressTracker = null) {
        this.context = context;
        this.vscode = vscode;
        this.curriculumCache = new Map();
        this.currentLessonPanel = null; // Track current lesson panel
        this.currentExerciseEditor = null; // Track current exercise editor
        this.currentQuizPanel = null;
        this.progressTracker = progressTracker || new ProgressTracker(context);
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

        if (this.currentQuizPanel) {
            this.currentQuizPanel.dispose();
            this.currentQuizPanel = null;
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
                if (tab.input && (tab.input.viewType === 'tsiLearnLesson' || tab.input.viewType === 'tsiLearnQuiz')) {
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
     * Normalize curriculum structure to a list of sections (modules or chapters)
     * @param {Object} curriculum - Curriculum JSON object
     * @returns {Array} Array of section objects with consistent typing
     */
    getCurriculumSections(curriculum) {
        if (curriculum && Array.isArray(curriculum.chapters)) {
            return curriculum.chapters.map(chapter => ({
                ...chapter,
                type: 'chapter'
            }));
        }

        if (curriculum && Array.isArray(curriculum.modules)) {
            return curriculum.modules.map(module => ({
                ...module,
                type: 'module'
            }));
        }

        return [];
    }

    /**
     * Get the next lesson based on progress
     * @param {Object} curriculum - The curriculum object
     * @param {Object} progress - Current progress
     * @returns {Object|null} Next lesson or null if complete
     */
    getNextLesson(curriculum, progress) {
        const completedLessons = new Set(progress.completed || []);
        const languageSuffixPattern = /_(c|cpp|python|java|javascript|ruby|typescript|ts|csharp|cs|go|rust|swift|kotlin|php)$/i;
        const completedLegacyForms = new Set(
            Array.from(completedLessons)
                .filter(id => typeof id === 'string')
                .map(id => id.replace(languageSuffixPattern, ''))
        );
        
        // Find first incomplete lesson
        for (const section of this.getCurriculumSections(curriculum)) {
            const lessons = Array.isArray(section.lessons) ? section.lessons : [];
            for (const lesson of lessons) {
                const lessonId = lesson.id;
                const legacyLessonId = typeof lessonId === 'string'
                    ? lessonId.replace(languageSuffixPattern, '')
                    : lessonId;

                const isCompleted = completedLessons.has(lessonId) ||
                    completedLessons.has(legacyLessonId) ||
                    completedLegacyForms.has(lessonId) ||
                    completedLegacyForms.has(legacyLessonId);

                if (!isCompleted) {
                    const sectionTitle = section.title || section.id || 'Section';
                    return {
                        ...lesson,
                        sectionTitle,
                        sectionId: section.id,
                        sectionType: section.type || 'section',
                        // Maintain legacy fields for backward compatibility
                        moduleTitle: sectionTitle,
                        moduleId: section.id,
                        exerciseVariants: lesson.exerciseVariants || []
                    };
                }
            }
        }
        
        return null; // All lessons completed
    }

    getLessonAfter(curriculum, currentLessonId) {
        if (!curriculum || !currentLessonId) {
            return null;
        }

        const normalizedCurrent = this.normalizeLessonId(currentLessonId);
        if (!normalizedCurrent) {
            return null;
        }

        let returnNext = false;
        for (const section of this.getCurriculumSections(curriculum)) {
            const lessons = Array.isArray(section.lessons) ? section.lessons : [];
            for (const lesson of lessons) {
                const lessonId = typeof lesson.id === 'string' ? lesson.id : '';
                if (!lessonId) {
                    continue;
                }

                if (returnNext) {
                    const sectionTitle = section.title || section.id || 'Section';
                    return {
                        ...lesson,
                        sectionTitle,
                        sectionId: section.id,
                        sectionType: section.type || 'section',
                        moduleTitle: sectionTitle,
                        moduleId: section.id,
                        exerciseVariants: lesson.exerciseVariants || []
                    };
                }

                const normalizedLessonId = this.normalizeLessonId(lessonId);
                if (normalizedLessonId === normalizedCurrent) {
                    returnNext = true;
                }
            }
        }

        return null;
    }

    getNextLessonForSolution(curriculum, progress, currentLessonId) {
        const normalizedCurrent = this.normalizeLessonId(currentLessonId);

        if (normalizedCurrent) {
            const sequentialLesson = this.getLessonAfter(curriculum, normalizedCurrent);
            if (sequentialLesson) {
                return sequentialLesson;
            }
        }

        const nextLesson = this.getNextLesson(curriculum, progress) || null;
        if (!nextLesson) {
            return null;
        }

        const normalizedNext = nextLesson.id ? this.normalizeLessonId(nextLesson.id) : null;
        if (normalizedCurrent && normalizedNext && normalizedNext === normalizedCurrent) {
            return this.getLessonAfter(curriculum, normalizedCurrent) || null;
        }

        return nextLesson;
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
            const lessonDirectory = path.dirname(lessonPath);

            const resourceRoots = [];
            if (this.vscode && this.vscode.Uri && typeof this.vscode.Uri.file === 'function') {
                const resourcesRoot = path.join(__dirname, '..', '..', 'resources');
                resourceRoots.push(this.vscode.Uri.file(resourcesRoot));
                resourceRoots.push(this.vscode.Uri.file(lessonDirectory));
            }

            const lessonPanelOptions = {
                enableScripts: true,
                retainContextWhenHidden: true
            };

            if (resourceRoots.length > 0) {
                lessonPanelOptions.localResourceRoots = resourceRoots;
            }

            // Create webview panel for lesson
            const panel = this.vscode.window.createWebviewPanel(
                'tsiLearnLesson',
                `Learn ${language}: ${lesson.title || lesson.id}`,
                this.vscode.ViewColumn.One,
                lessonPanelOptions
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
            const resolveResourceUri = this.createResourceResolver(panel.webview, lessonDirectory);
            panel.webview.html = this.getLessonHtml(lessonContent, lesson, {
                resolveResourceUri
            });
            
            // Handle messages from webview
            panel.webview.onDidReceiveMessage(
                async message => {
                    switch (message.command) {
                        case 'startExercise':
                            await this.startExercise(language, lesson, message.exerciseId, {
                                variantId: message.exerciseVariantId,
                                exerciseLanguage: message.exerciseLanguage
                            });
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
    getLessonHtml(content, lesson, options = {}) {
        const resolveResourceUri = typeof options.resolveResourceUri === 'function'
            ? options.resolveResourceUri
            : null;

        // Convert markdown to HTML (simplified)
        let htmlContent = this.markdownToHtml(content);

        if (resolveResourceUri) {
            htmlContent = this.rewriteImageSources(htmlContent, resolveResourceUri);
        }
        
        const exerciseVariants = Array.isArray(lesson.exerciseVariants) ? lesson.exerciseVariants : [];
        const exercisesDisabled = lesson && lesson.exerciseAvailable === false;
        const exerciseConfig = {
            baseExerciseId: `${lesson.id}_exercise`,
            variants: exerciseVariants
        };

        const variantButtonsHtml = exercisesDisabled
            ? '<div class="exercise-unavailable">📘 Practice exercise for this chapter is covered in the textbook walkthrough.</div>'
            : exerciseVariants.length > 0
                ? exerciseVariants.map(variant => `
                <button class="exercise-button" onclick="startExerciseVariant('${variant.id}', '${variant.language}')">
                    ${variant.language === 'c' ? '🇨 ' : '🇨++ '} ${this.escapeHtml(variant.title || variant.id)}
                </button>
            `).join('\n')
                : '<button class="exercise-button" onclick="startDefaultExercise()">\n            📝 Start Practice Exercise\n        </button>';

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
            line-height: 1.0;
            white-space: pre;
            font-family: var(--vscode-editor-font-family);
        }
        pre code {
            background-color: transparent;
            padding: 0;
            font-size: 13px;
            line-height: 1.0;
            color: var(--vscode-textPreformat-foreground);
            white-space: pre;
            font-family: inherit;
        }
        pre code * {
            font-size: 13px !important;
            line-height: 1.0 !important;
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
        .exercise-unavailable {
            margin: 24px auto;
            padding: 14px 18px;
            border: 1px dashed var(--vscode-input-border);
            border-radius: 6px;
            font-style: italic;
            max-width: 520px;
            background-color: var(--vscode-editorGutter-background);
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
        ${variantButtonsHtml}
        <button class="exercise-button complete-button" onclick="completeLesson()">
            ✅ Mark as Complete
        </button>
    </div>
    
    <script>
        const vscode = acquireVsCodeApi();
        const exerciseConfig = ${JSON.stringify(exerciseConfig)};
        
        function startDefaultExercise() {
            vscode.postMessage({
                command: 'startExercise',
                exerciseId: exerciseConfig.baseExerciseId
            });
        }

        function startExerciseVariant(variantId, variantLanguage) {
            vscode.postMessage({
                command: 'startExercise',
                exerciseId: exerciseConfig.baseExerciseId,
                exerciseVariantId: variantId,
                exerciseLanguage: variantLanguage
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
            // Highlight comments in code blocks and normalize line spacing
            const processedCode = code
                .split('\n')
                .map(line => {
                    // Check if line is a comment
                    if (line.trim().startsWith('#')) {
                        return `<span class="code-comment">${this.escapeHtml(line)}</span>`;
                    }
                    return this.escapeHtml(line);
                })
                .join('\n')
                // Normalize line spacing: replace multiple consecutive empty lines with single empty lines
                .replace(/\n{3,}/g, '\n\n');
            return `<pre><code>${processedCode}</code></pre>`;
        });

        // Convert images
        html = html.replace(/!\[(.*?)\]\((.*?)\)/g, (match, alt, src) => {
            const rawSource = (src || '').trim();
            const altText = (alt || '').trim();
            const safeAlt = this.escapeHtml(altText);
            const safeSource = this.escapeHtml(rawSource);
            return `<img src="${safeSource}" alt="${safeAlt}" data-tsi-src="${safeSource}">`;
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

    rewriteImageSources(html, resolveResourceUri) {
        if (typeof html !== 'string') {
            return html;
        }

        return html.replace(/<img\s+([^>]*?)src="(.*?)"([^>]*)>/g, (match, before, src, after) => {
            const rawSource = src.trim();
            const resolved = rawSource.startsWith('http://') || rawSource.startsWith('https://')
                ? rawSource
                : resolveResourceUri(rawSource);

            const resolvedString = typeof resolved === 'string'
                ? resolved
                : (resolved && typeof resolved.toString === 'function'
                    ? resolved.toString()
                    : rawSource);

            const escapedSrc = this.escapeHtml(resolvedString);
            return `<img ${before || ''}src="${escapedSrc}"${after || ''}>`;
        });
    }

    createResourceResolver(webview, baseDirectory) {
        const vscodeUri = this.vscode && this.vscode.Uri;
        return (sourcePath) => {
            if (!sourcePath) {
                return sourcePath;
            }

            if (/^(?:https?:|data:)/i.test(sourcePath)) {
                return sourcePath;
            }

            const normalizedPath = path.isAbsolute(sourcePath)
                ? sourcePath
                : path.resolve(baseDirectory, sourcePath);

            if (!webview || typeof webview.asWebviewUri !== 'function' || !vscodeUri || typeof vscodeUri.file !== 'function') {
                return normalizedPath;
            }

            const fileUri = vscodeUri.file(normalizedPath);
            const webviewUri = webview.asWebviewUri(fileUri);
            return typeof webviewUri === 'string' ? webviewUri : webviewUri.toString();
        };
    }

    async startQuizExercise(language, lesson, exercise, context = {}) {
        try {
            if (this.currentQuizPanel) {
                this.currentQuizPanel.dispose();
                this.currentQuizPanel = null;
            }

            const resourceRoots = [];
            if (this.vscode && this.vscode.Uri && typeof this.vscode.Uri.file === 'function') {
                const resourcesRoot = path.join(__dirname, '..', '..', 'resources');
                resourceRoots.push(this.vscode.Uri.file(resourcesRoot));
            }

            const quizPanelOptions = {
                enableScripts: true,
                retainContextWhenHidden: true
            };

            if (resourceRoots.length > 0) {
                quizPanelOptions.localResourceRoots = resourceRoots;
            }

            const panel = this.vscode.window.createWebviewPanel(
                'tsiLearnQuiz',
                `${exercise.title || lesson.title || 'Quiz'}`,
                this.vscode.ViewColumn.One,
                quizPanelOptions
            );

            this.currentQuizPanel = panel;
            panel.onDidDispose(() => {
                if (this.currentQuizPanel === panel) {
                    this.currentQuizPanel = null;
                }
            }, null, this.context.subscriptions);

            panel.webview.html = this.getQuizHtml(exercise, lesson);

            panel.webview.onDidReceiveMessage(async (message) => {
                if (!message || typeof message.command !== 'string') {
                    return;
                }

                if (message.command === 'submitQuiz') {
                    await this.handleQuizSubmission({
                        language,
                        lesson,
                        exercise,
                        panel,
                        answers: message.answers || {}
                    });
                } else if (message.command === 'closeQuiz') {
                    panel.dispose();
                } else if (message.command === 'openNextLesson') {
                    await this.openNextLessonFromQuiz({ language, lesson, panel });
                }
            }, undefined, this.context.subscriptions);
        } catch (error) {
            this.vscode.window.showErrorMessage(
                `Failed to start quiz: ${error.message}`,
                { modal: true },
                'Got it!'
            );
        }
    }

    getQuizHtml(exercise, lesson) {
        const questions = Array.isArray(exercise.questions) ? exercise.questions : [];

        const questionMarkup = questions.map((question, index) => {
            const questionId = this.escapeHtml(question.id || `question_${index + 1}`);
            const prompt = this.markdownToHtml(question.prompt || `Question ${index + 1}`);
            const type = (question.type || 'single').toLowerCase();
            const options = Array.isArray(question.options) ? question.options : [];

            let inputs = '';
            if (type === 'multiple') {
                inputs = options.map((option, optionIndex) => {
                    const optionId = this.escapeHtml(option.id || `${questionId}_option_${optionIndex + 1}`);
                    const optionText = this.escapeHtml(option.text || `Option ${optionIndex + 1}`);
                    return `
                        <label class="quiz-option">
                            <input type="checkbox" name="${questionId}" value="${optionId}">
                            <span>${optionText}</span>
                        </label>`;
                }).join('\n');
            } else if (type === 'truefalse') {
                inputs = ['true', 'false'].map(value => {
                    return `
                        <label class="quiz-option">
                            <input type="radio" name="${questionId}" value="${value}">
                            <span>${value.charAt(0).toUpperCase() + value.slice(1)}</span>
                        </label>`;
                }).join('\n');
            } else {
                inputs = options.map((option, optionIndex) => {
                    const optionId = this.escapeHtml(option.id || `${questionId}_option_${optionIndex + 1}`);
                    const optionText = this.escapeHtml(option.text || `Option ${optionIndex + 1}`);
                    return `
                        <label class="quiz-option">
                            <input type="radio" name="${questionId}" value="${optionId}">
                            <span>${optionText}</span>
                        </label>`;
                }).join('\n');
            }

            return `
                <section class="quiz-question" data-question="${questionId}">
                    <h2>Question ${index + 1}</h2>
                    <div class="quiz-prompt">${prompt}</div>
                    <div class="quiz-options">${inputs}</div>
                    <div class="quiz-feedback" id="feedback-${questionId}"></div>
                </section>`;
        }).join('\n');

        return `<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>${this.escapeHtml(exercise.title || 'Quiz')}</title>
    <style>
        body {
            font-family: var(--vscode-font-family, sans-serif);
            color: var(--vscode-foreground);
            background: var(--vscode-editor-background);
            padding: 20px;
            line-height: 1.6;
        }
        h1 {
            color: var(--vscode-textLink-foreground);
            margin-bottom: 16px;
        }
        h2 {
            margin: 12px 0;
            color: var(--vscode-textLink-activeForeground);
        }
        .quiz-question {
            background: var(--vscode-textBlockQuote-background, rgba(0,0,0,0.1));
            border-left: 4px solid var(--vscode-textLink-foreground);
            padding: 16px;
            margin-bottom: 20px;
            border-radius: 6px;
        }
        .quiz-option {
            display: block;
            margin: 8px 0;
            font-size: 14px;
        }
        .quiz-option input {
            margin-right: 8px;
        }
        .quiz-actions {
            margin-top: 24px;
        }
        button {
            background-color: var(--vscode-button-background);
            color: var(--vscode-button-foreground);
            border: none;
            padding: 10px 20px;
            margin-right: 10px;
            border-radius: 4px;
            cursor: pointer;
            font-size: 14px;
        }
        button:hover {
            background-color: var(--vscode-button-hoverBackground);
        }
        button.secondary {
            background-color: transparent;
            border: 1px solid var(--vscode-button-border, var(--vscode-button-hoverBackground));
            color: var(--vscode-button-foreground);
        }
        button.secondary:hover {
            background-color: var(--vscode-button-hoverBackground);
        }
        .quiz-feedback {
            margin-top: 10px;
            font-style: italic;
        }
        .quiz-feedback.correct {
            color: var(--vscode-testing-iconPassed, #4caf50);
        }
        .quiz-feedback.incorrect {
            color: var(--vscode-testing-iconFailed, #f44336);
        }
        .quiz-score {
            margin-top: 20px;
            font-weight: 600;
        }
    </style>
</head>
<body>
    <h1>${this.escapeHtml(exercise.title || 'Quiz')}</h1>
    <p>${this.escapeHtml(exercise.description || '')}</p>
    <form id="tsi-quiz-form">
        ${questionMarkup}
        <div class="quiz-actions">
            <button type="button" onclick="submitQuiz()">Submit Answers</button>
            <button type="button" onclick="resetQuiz()">Reset</button>
            <button type="button" class="secondary" onclick="openNextLesson()">Next Lesson</button>
        </div>
        <div class="quiz-score" id="quiz-score"></div>
    </form>

    <script>
        const vscode = acquireVsCodeApi();

        function collectAnswers() {
            const form = document.getElementById('tsi-quiz-form');
            const formData = new FormData(form);
            const answers = {};

            form.querySelectorAll('.quiz-question').forEach(section => {
                const questionId = section.getAttribute('data-question');
                const inputs = section.querySelectorAll('input');

                if (inputs.length === 0) {
                    return;
                }

                const inputType = inputs[0].type;
                if (inputType === 'checkbox') {
                    answers[questionId] = Array.from(inputs)
                        .filter(input => input.checked)
                        .map(input => input.value);
                } else {
                    answers[questionId] = formData.get(questionId);
                }
            });

            return answers;
        }

        function submitQuiz() {
            const answers = collectAnswers();
            vscode.postMessage({ command: 'submitQuiz', answers });
        }

        function resetQuiz() {
            const form = document.getElementById('tsi-quiz-form');
            form.reset();
            document.querySelectorAll('.quiz-feedback').forEach(el => {
                el.textContent = '';
                el.classList.remove('correct', 'incorrect');
            });
            document.getElementById('quiz-score').textContent = '';
        }

        function openNextLesson() {
            vscode.postMessage({ command: 'openNextLesson' });
        }

        window.addEventListener('message', event => {
            const message = event.data || {};
            if (message.command === 'quizResult') {
                const { details = [], scoreText = '' } = message;
                details.forEach(detail => {
                    const el = document.getElementById('feedback-' + detail.id);
                    if (!el) return;
                    el.textContent = detail.message;
                    el.classList.remove('correct', 'incorrect');
                    if (detail.correct) {
                        el.classList.add('correct');
                    } else {
                        el.classList.add('incorrect');
                    }
                });
                document.getElementById('quiz-score').textContent = scoreText;
            }
        });
    </script>
</body>
</html>`;
    }

    async handleQuizSubmission({ language, lesson, exercise, panel, answers }) {
        const evaluation = this.evaluateQuizAnswers(exercise, answers);

        panel.webview.postMessage({
            command: 'quizResult',
            details: evaluation.details,
            scoreText: `Score: ${evaluation.correct}/${evaluation.total}`
        });

        if (!evaluation.passed) {
            const message = evaluation.feedbackMessage || 'Keep trying! Review the explanations and adjust your answers.';
            const progressLanguage = exercise.curriculumLanguage || language;
            const { nextLesson } = await this.findNextLessonCandidate(progressLanguage, lesson);
            const warningButtons = nextLesson
                ? ['Retry', 'Next Lesson', 'Close Quiz']
                : ['Retry', 'Close Quiz'];

            this.vscode.window.showWarningMessage(
                `❌ Quiz incomplete. ${message}`,
                { modal: true },
                ...warningButtons
            ).then(async selection => {
                if (selection === 'Next Lesson' && nextLesson) {
                    panel.dispose();
                    await this.openLesson(progressLanguage, nextLesson);
                } else if (selection === 'Close Quiz') {
                    panel.dispose();
                }
            });
            return;
        }

        const exerciseId = exercise.id || `${lesson.id}_quiz`;
        const progressLanguage = exercise.curriculumLanguage || language;
        await this.progressTracker.recordCompletion(progressLanguage, exerciseId, {
            lessonId: lesson.id,
            baseExerciseId: exercise.baseExerciseId || exerciseId,
            mode: 'quiz'
        });

        const curriculum = await this.loadCurriculum(progressLanguage);
        const progress = await this.progressTracker.getProgress(progressLanguage);
        const { nextLesson } = await this.findNextLessonCandidate(progressLanguage, lesson, {
            curriculum,
            progress
        });

        const infoButtons = nextLesson
            ? ['Next Lesson', 'Browse Lessons', 'Close Quiz']
            : ['Browse Lessons', 'Close Quiz'];

        this.vscode.window.showInformationMessage(
            `✅ Quiz "${exercise.title || lesson.title}" completed! ${evaluation.correct}/${evaluation.total} correct.`,
            { modal: true },
            ...infoButtons
        ).then(async selection => {
            if (selection === 'Next Lesson' && nextLesson) {
                panel.dispose();
                await this.openLesson(progressLanguage, nextLesson);
            } else if (selection === 'Browse Lessons') {
                panel.dispose();
                this.browseLessons(progressLanguage);
            } else if (selection === 'Close Quiz') {
                panel.dispose();
            }
        });
    }

    async findNextLessonCandidate(language, lesson, { curriculum, progress } = {}) {
        let activeCurriculum = curriculum;
        if (!activeCurriculum) {
            activeCurriculum = await this.loadCurriculum(language);
        }

        const lessonId = lesson && lesson.id ? lesson.id : null;
        let sequentialNext = null;
        if (lessonId) {
            sequentialNext = this.getLessonAfter(activeCurriculum, lessonId);
            if (sequentialNext && sequentialNext.id === lessonId) {
                sequentialNext = null;
            }
        }

        if (sequentialNext) {
            return { nextLesson: sequentialNext, curriculum: activeCurriculum };
        }

        let activeProgress = progress;
        if (!activeProgress) {
            activeProgress = await this.progressTracker.getProgress(language);
        }

        const fallback = this.getNextLesson(activeCurriculum, activeProgress) || null;
        if (fallback && lessonId && fallback.id === lessonId) {
            return { nextLesson: null, curriculum: activeCurriculum };
        }

        return { nextLesson: fallback, curriculum: activeCurriculum };
    }

    async openNextLessonFromQuiz({ language, lesson, panel }) {
        try {
            const { nextLesson } = await this.findNextLessonCandidate(language, lesson);

            if (nextLesson) {
                panel.dispose();
                await this.openLesson(language, nextLesson);
                return;
            }

            this.vscode.window.showInformationMessage('You have reached the end of the available lessons for now. Feel free to revisit previous chapters.');
        } catch (error) {
            this.vscode.window.showErrorMessage(`Unable to open the next lesson: ${error.message}`);
        }
    }

    evaluateQuizAnswers(exercise, answers = {}) {
        const questions = Array.isArray(exercise.questions) ? exercise.questions : [];
        const details = [];
        let correctCount = 0;

        for (const question of questions) {
            const questionId = question.id || question.prompt;
            const provided = answers && Object.prototype.hasOwnProperty.call(answers, questionId)
                ? answers[questionId]
                : undefined;

            const evaluation = this.evaluateQuizQuestion(question, provided);
            if (evaluation.correct) {
                correctCount += 1;
            }

            details.push({
                id: questionId,
                correct: evaluation.correct,
                message: evaluation.message
            });
        }

        const total = questions.length || 0;
        const threshold = typeof exercise.passScore === 'number'
            ? Math.max(0, Math.min(exercise.passScore, total))
            : total;

        const passed = correctCount >= threshold;

        return {
            passed,
            correct: correctCount,
            total,
            details,
            feedbackMessage: passed ? null : `You answered ${correctCount} out of ${total} correctly. Review the incorrect questions and try again.`
        };
    }

    evaluateQuizQuestion(question, providedAnswer) {
        const type = (question.type || 'single').toLowerCase();
        const options = Array.isArray(question.options) ? question.options : [];
        const explanation = question.explanation || '';

        const failResult = (message) => ({
            correct: false,
            message: explanation ? `${message} ${explanation}` : message
        });

        if (type === 'multiple') {
            const correctOptions = options.filter(option => option.correct || option.isCorrect || option.id === question.answer);
            const expectedIds = correctOptions.map(option => option.id || option.value).filter(Boolean).sort();
            const providedIds = Array.isArray(providedAnswer)
                ? providedAnswer.map(String).sort()
                : (typeof providedAnswer === 'string' && providedAnswer.length ? [providedAnswer] : []);

            if (expectedIds.length === 0) {
                return failResult('No correct answers were defined for this question.');
            }

            const matches = expectedIds.length === providedIds.length && expectedIds.every((id, index) => id === providedIds[index]);
            if (matches) {
                return {
                    correct: true,
                    message: '✅ Correct!'
                };
            }

            return failResult('Incorrect selection. Ensure you selected all correct answers.');
        }

        if (type === 'truefalse') {
            const expected = typeof question.answer === 'string'
                ? question.answer.toLowerCase()
                : (question.correct === true ? 'true' : (question.correct === false ? 'false' : ''));

            if (!expected) {
                return failResult('No answer key defined for this question.');
            }

            const provided = (providedAnswer || '').toString().toLowerCase();
            if (provided === expected) {
                return {
                    correct: true,
                    message: '✅ Correct!'
                };
            }

            return failResult('Incorrect. Review the concept and try again.');
        }

        const correctOption = options.find(option => option.correct || option.isCorrect);
        const expectedId = correctOption ? (correctOption.id || correctOption.value) : question.answer;

        if (!expectedId) {
            return failResult('No correct answer defined for this question.');
        }

        if (providedAnswer && providedAnswer.toString() === expectedId.toString()) {
            return {
                correct: true,
                message: '✅ Correct!'
            };
        }

        return failResult('Incorrect answer. Review the explanation and try again.');
    }

    /**
     * Start an exercise
     * @param {string} language - The programming language
     * @param {Object} lesson - The lesson
     * @param {string} exerciseId - Exercise ID
     */
    async startExercise(language, lesson, exerciseId, options = {}) {
        try {
            const exercisePath = path.join(__dirname, '..', 'curriculum', language, 'exercises', `${exerciseId}.json`);
            const exerciseContent = await fs.readFile(exercisePath, 'utf8');
            const exercise = JSON.parse(exerciseContent);

            const variantId = options.variantId;
            const exerciseLanguage = (options.exerciseLanguage || (variantId ? null : language) || language).toLowerCase();

            if (exercise.mode === 'quiz') {
                await this.startQuizExercise(language, lesson, exercise, {
                    variantId,
                    exerciseLanguage,
                    options
                });
                return;
            }

            let variant = null;
            if (Array.isArray(exercise.variants) && exercise.variants.length > 0) {
                variant = variantId
                    ? exercise.variants.find(v => v.id === variantId)
                    : exercise.variants[0];
                if (!variant) {
                    variant = exercise.variants[0];
                }
            }

            const runtimeLanguage = (variant && variant.language) ? variant.language.toLowerCase() : exerciseLanguage;
            const starterCode = variant?.starterCode ?? exercise.starterCode ?? '';
            const tests = variant?.tests ?? exercise.tests ?? [];
            const hints = variant?.hints ?? exercise.hints ?? [];
            const difficulty = variant?.difficulty ?? exercise.difficulty ?? lesson.difficulty;
            const variantProgressId = variant?.id || exercise.id;
            const variantTitle = variant?.title || exercise.title || lesson.title;
            
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
            
            const exerciseDirectoryLanguage = runtimeLanguage || language;
            const fileExtension = this.getFileExtension(exerciseDirectoryLanguage);
            const targetExerciseId = variant ? variant.id : exercise.id || exerciseId;
            const exerciseFilePath = path.join(
                workspaceFolder.uri.fsPath,
                'learn_exercises',
                exerciseDirectoryLanguage,
                `${targetExerciseId}.${fileExtension}`
            );
            
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
            
            // Check if exercise file already exists and has user content
            let shouldWriteStarterCode = true;
            try {
                const existingContent = await fs.readFile(exerciseFilePath, 'utf8');
                // Don't overwrite if file has meaningful content (more than just placeholder)
                const trimmedContent = existingContent.trim();
                if (trimmedContent && 
                    trimmedContent !== '# Write your code here' && 
                    trimmedContent !== '# Your code here' &&
                    trimmedContent.length > 20) { // Has substantial content
                    shouldWriteStarterCode = false;
                }
            } catch (error) {
                // File doesn't exist, so we should write starter code
                shouldWriteStarterCode = true;
            }
            
            // Write starter code only if file is empty or has minimal content
            if (shouldWriteStarterCode) {
                const starterContent = starterCode || '# Write your code here\n';
                await fs.writeFile(exerciseFilePath, starterContent);
            }
            
            // Store reference to current exercise file
            this.currentExerciseEditor = exerciseFilePath;
            const exerciseMetadata = {
                lessonId: lesson.id,
                sectionId: lesson.moduleId,
                sectionTitle: lesson.title,
                // Legacy fields retained for backwards compatibility with existing data
                moduleId: lesson.moduleId || lesson.sectionId,
                moduleTitle: lesson.moduleTitle || lesson.sectionTitle,
                baseExerciseId: exercise.id || exerciseId,
                baseExerciseFile: exerciseId,
                variantId: targetExerciseId,
                variantLanguage: exerciseDirectoryLanguage,
                difficulty,
                title: variantTitle,
                hasVariants: Array.isArray(exercise.variants) && exercise.variants.length > 0,
                curriculumLanguage: language
            };
            await this.context.workspaceState.update(`learn_exercise_meta_${exerciseFilePath}`, exerciseMetadata);
            
            // Open the file
            const doc = await this.vscode.workspace.openTextDocument(exerciseFilePath);
            const editor = await this.vscode.window.showTextDocument(doc);
            
            // Position cursor on an empty line after a comment marker
            // Look for patterns like "# Your code here", "# Use", etc.
            const text = doc.getText();
            const lines = text.split('\n');
            let targetLine = -1;
            const commentPrefixes = ['#', '//', '/*'];
            
            for (let i = 0; i < lines.length; i++) {
                const line = lines[i].trim();
                // Look for comment lines that indicate where to write code
                if (commentPrefixes.some(prefix => line.startsWith(prefix)) && 
                    (line.includes('Your code here') || 
                     line.includes('your code here') ||
                     line.includes('Use if') ||
                     line.includes('Use a') ||
                     line.includes('Use') ||
                     line.includes('Return') ||
                     line.includes('Calculate') ||
                     line.includes('TODO') ||
                     line.includes('todo'))) {
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
                `Exercise "${variantTitle}" opened (${exerciseDirectoryLanguage.toUpperCase()}). Complete the code and run tests!`,
                { modal: true },
                'Run Tests',
                'Got it!'
            ).then(selection => {
                if (selection === 'Run Tests') {
                    this.vscode.commands.executeCommand('tsiheader.runExerciseTests', exerciseDirectoryLanguage, {
                        baseExerciseId: exerciseMetadata.baseExerciseId,
                        variantId: exerciseMetadata.variantId,
                        curriculumLanguage: language,
                        title: variantTitle,
                        variantLanguage: exerciseMetadata.variantLanguage
                    });
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
        try {
            const progress = await this.progressTracker.markLessonComplete(language, lessonId);

            const curriculum = await this.loadCurriculum(language);
            const nextLesson = this.getNextLesson(curriculum, progress);

            if (nextLesson) {
                this.vscode.window.showInformationMessage(
                    `🎉 Lesson completed! Progress saved.`,
                    { modal: true },
                    'Next Lesson',
                    'Got it!'
                ).then(async selection => {
                    if (selection === 'Next Lesson') {
                        await this.openLesson(language, nextLesson);
                    }
                });
            } else {
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
        } catch (error) {
            this.vscode.window.showErrorMessage(
                `Failed to mark lesson complete: ${error.message}`,
                { modal: true },
                'Got it!'
            );
        }
    }

    /**
     * Load solution for an exercise
     * @param {string} language - Programming language
     * @param {string} exerciseId - Exercise ID
     * @returns {Promise<Object>} Solution object
     */
    async loadSolution(language, exerciseId, variantId = null) {
        try {
            const solutionPath = path.join(__dirname, '..', 'curriculum', language, 'solutions', `${exerciseId}.json`);
            const content = await fs.readFile(solutionPath, 'utf8');
            const solution = JSON.parse(content);

            if (Array.isArray(solution.variants) && solution.variants.length > 0) {
                const match = variantId
                    ? solution.variants.find(v => v.id === variantId)
                    : solution.variants[0];
                if (!match) {
                    throw new Error(`Solution variant '${variantId}' not found.`);
                }
                return {
                    exerciseId: solution.exerciseId || exerciseId,
                    ...match
                };
            }

            return solution;
        } catch (error) {
            throw new Error(`Failed to load solution: ${error.message}`);
        }
    }

    getExerciseMetadata(filePath) {
        return this.context.workspaceState.get(`learn_exercise_meta_${filePath}`, null);
    }

    normalizeLessonId(lessonId) {
        if (!lessonId) {
            return null;
        }

        let normalized = lessonId.toString();
        normalized = normalized.replace(/_exercise$/, '');
        normalized = normalized.replace(/_solution$/, '');
        normalized = normalized.replace(/_variant$/, '');

        return normalized;
    }

    /**
     * Clear the curriculum cache
     */
    clearCurriculumCache() {
        this.curriculumCache.clear();
    }
}

module.exports = LearnManager;
