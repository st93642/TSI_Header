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
        this.cacheDir = null;
    }

    /**
     * Get the cache directory path for mathematics workbooks
     * @returns {Promise<string>} Path to cache directory
     */
    async getCacheDir() {
        if (this.cacheDir) return this.cacheDir;

        try {
            // Use VS Code's global storage path for caching
            const globalStoragePath = this.vscode.env.globalStorageUri;

            if (globalStoragePath) {
                this.cacheDir = this.vscode.Uri.joinPath(globalStoragePath, 'math-cache').fsPath;
            } else {
                // Fallback to workspace directory if global storage not available
                const workspaceFolder = this.vscode.workspace.workspaceFolders?.[0];
                if (workspaceFolder) {
                    this.cacheDir = this.vscode.Uri.joinPath(workspaceFolder.uri, '.vscode', 'math-cache').fsPath;
                } else {
                    // Last resort: use temp directory
                    const os = require('os');
                    const path = require('path');
                    this.cacheDir = path.join(os.tmpdir(), 'vscode-math-cache');
                }
            }

            // Ensure cache directory exists
            const fs = require('fs').promises;
            await fs.mkdir(this.cacheDir, { recursive: true });
            return this.cacheDir;
        } catch (error) {
            console.error('Failed to create cache directory:', error);
            return null;
        }
    }

    /**
     * Generate a cache key for a workbook URL
     * @param {string} url - Workbook URL
     * @returns {string} Cache key
     */
    getCacheKey(url) {
        const crypto = require('crypto');
        return crypto.createHash('md5').update(url).digest('hex');
    }

    /**
     * Get cache file path for a workbook
     * @param {string} url - Workbook URL
     * @returns {Promise<string>} Path to cache file
     */
    async getCacheFilePath(url) {
        const cacheDir = await this.getCacheDir();
        if (!cacheDir) return null;

        const cacheKey = this.getCacheKey(url);
        return path.join(cacheDir, `${cacheKey}.pdf`);
    }

    /**
     * Get cache metadata file path
     * @param {string} url - Workbook URL
     * @returns {Promise<string>} Path to metadata file
     */
    async getCacheMetadataPath(url) {
        const cacheDir = await this.getCacheDir();
        if (!cacheDir) return null;

        const cacheKey = this.getCacheKey(url);
        return path.join(cacheDir, `${cacheKey}.json`);
    }

    /**
     * Save workbook to cache with metadata
     * @param {string} url - Workbook URL
     * @param {string} title - Workbook title
     * @param {string} filePath - Local file path
     * @returns {Promise<boolean>} Success status
     */
    async saveWorkbookToCache(url, title, filePath) {
        try {
            const metadataPath = await this.getCacheMetadataPath(url);
            if (!metadataPath) return false;

            const metadata = {
                url: url,
                title: title,
                cachedAt: new Date().toISOString(),
                fileSize: await this.getFileSize(filePath),
                version: '1.0'
            };

            const fs = require('fs').promises;
            await fs.writeFile(metadataPath, JSON.stringify(metadata, null, 2), 'utf8');
            return true;
        } catch (error) {
            console.error('Failed to save workbook to cache:', error);
            return false;
        }
    }

    /**
     * Load workbook from cache
     * @param {string} url - Workbook URL
     * @param {number} maxAgeHours - Maximum age of cache in hours (default: 168 = 1 week)
     * @returns {Promise<Object|null>} Cache metadata or null if not found/expired
     */
    async loadWorkbookFromCache(url, maxAgeHours = 168) {
        try {
            const metadataPath = await this.getCacheMetadataPath(url);
            if (!metadataPath) return null;

            const fs = require('fs').promises;
            const metadata = JSON.parse(await fs.readFile(metadataPath, 'utf8'));

            // Check if cache is expired
            const cachedAt = new Date(metadata.cachedAt);
            const now = new Date();
            const ageHours = (now - cachedAt) / (1000 * 60 * 60);

            if (ageHours > maxAgeHours) {
                // Cache is too old, remove it
                await this.removeCachedWorkbook(url);
                return null;
            }

            // Check if file still exists
            const filePath = await this.getCacheFilePath(url);
            try {
                await fs.access(filePath);
                return metadata;
            } catch (error) {
                // File doesn't exist, remove metadata
                await fs.unlink(metadataPath).catch(() => {});
                return null;
            }
        } catch (error) {
            // Cache file doesn't exist or is corrupted
            return null;
        }
    }

    /**
     * Remove cached workbook and metadata
     * @param {string} url - Workbook URL
     * @returns {Promise<boolean>} Success status
     */
    async removeCachedWorkbook(url) {
        try {
            const fs = require('fs').promises;
            const filePath = await this.getCacheFilePath(url);
            const metadataPath = await this.getCacheMetadataPath(url);

            if (filePath) {
                await fs.unlink(filePath).catch(() => {});
            }
            if (metadataPath) {
                await fs.unlink(metadataPath).catch(() => {});
            }

            return true;
        } catch (error) {
            console.error('Failed to remove cached workbook:', error);
            return false;
        }
    }

    /**
     * Clear all cached workbooks
     * @returns {Promise<boolean>} Success status
     */
    async clearWorkbookCache() {
        try {
            const cacheDir = await this.getCacheDir();
            if (!cacheDir) return false;

            const fs = require('fs').promises;
            const files = await fs.readdir(cacheDir);
            await Promise.all(
                files.map(file => fs.unlink(path.join(cacheDir, file)))
            );

            return true;
        } catch (error) {
            console.error('Failed to clear workbook cache:', error);
            return false;
        }
    }

    /**
     * Get cache statistics
     * @returns {Promise<Object>} Cache statistics
     */
    async getCacheStats() {
        try {
            const cacheDir = await this.getCacheDir();
            if (!cacheDir) return { totalWorkbooks: 0, totalSize: 0, oldestCache: null, newestCache: null };

            const fs = require('fs').promises;
            const files = await fs.readdir(cacheDir);
            const pdfFiles = files.filter(file => file.endsWith('.pdf'));

            let totalSize = 0;
            let oldestCache = null;
            let newestCache = null;

            for (const file of pdfFiles) {
                const filePath = path.join(cacheDir, file);
                const stats = await fs.stat(filePath);
                totalSize += stats.size;

                // Get metadata for timestamps
                const metadataFile = file.replace('.pdf', '.json');
                try {
                    const metadataPath = path.join(cacheDir, metadataFile);
                    const metadata = JSON.parse(await fs.readFile(metadataPath, 'utf8'));
                    const cachedAt = new Date(metadata.cachedAt);

                    if (!oldestCache || cachedAt < oldestCache) oldestCache = cachedAt;
                    if (!newestCache || cachedAt > newestCache) newestCache = cachedAt;
                } catch (error) {
                    // Metadata missing, use file modification time
                    if (!oldestCache || stats.mtime < oldestCache) oldestCache = stats.mtime;
                    if (!newestCache || stats.mtime > newestCache) newestCache = stats.mtime;
                }
            }

            return {
                totalWorkbooks: pdfFiles.length,
                totalSize: totalSize,
                oldestCache: oldestCache ? oldestCache.toISOString() : null,
                newestCache: newestCache ? newestCache.toISOString() : null
            };
        } catch (error) {
            console.error('Failed to get cache stats:', error);
            return { totalWorkbooks: 0, totalSize: 0, oldestCache: null, newestCache: null };
        }
    }

    /**
     * Get cache statistics for display
     * @returns {Promise<Object>} Cache statistics
     */
    async getCacheStatistics() {
        const stats = await this.getCacheStats();
        return {
            workbooks: stats.totalWorkbooks,
            size: this.formatFileSize(stats.totalSize),
            oldest: stats.oldestCache,
            newest: stats.newestCache
        };
    }

    /**
     * Clear all cached mathematics workbooks
     * @returns {Promise<boolean>} Success status
     */
    async clearCache() {
        return await this.clearWorkbookCache();
    }

    /**
     * Format file size for display
     * @param {number} bytes - Size in bytes
     * @returns {string} Formatted size string
     */
    formatFileSize(bytes) {
        if (bytes === 0) return '0 B';

        const k = 1024;
        const sizes = ['B', 'KB', 'MB', 'GB'];
        const i = Math.floor(Math.log(bytes) / Math.log(k));

        return parseFloat((bytes / Math.pow(k, i)).toFixed(2)) + ' ' + sizes[i];
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
            // HELM workbooks from Loughborough University
            const baseUrl = 'https://www.lboro.ac.uk';
            const workbooks = [
                { number: 1, title: 'Basic Algebra', filename: 'Basic Algebra.pdf' },
                { number: 2, title: 'Basic Functions', filename: 'Basic Functions.pdf' },
                { number: 3, title: 'Equations, Inequalities and Partial Fractions', filename: 'Equations Inequalities and Partial Fractions.pdf' },
                { number: 4, title: 'Trigonometry', filename: 'HELM Workbook 4 Trigonometry.pdf' },
                { number: 5, title: 'Functions and Modelling', filename: 'HELM Workbook 5 Functions and Modelling.pdf' },
                { number: 6, title: 'Exponential and Logarithmic Functions', filename: 'HELM Workbook 6 Exponential and Logarithmic Functions.pdf' },
                { number: 7, title: 'Matrices', filename: 'HELM Workbook 7 Matrices.pdf' },
                { number: 8, title: 'Matrix Solution of Equations', filename: 'HELM Workbook 8 Matrix solution of Equations.pdf' },
                { number: 9, title: 'Vectors', filename: 'HELM Workbook 9 Vectors.pdf' },
                { number: 10, title: 'Complex Numbers', filename: 'HELM Workbook 10 Complex Numbers.pdf' },
                { number: 11, title: 'Differentiation', filename: 'HELM Workbook 11 Differentiation.pdf' },
                { number: 12, title: 'Applications of Differentiation', filename: 'HELM Workbook 12 Applications of Differentiation.pdf' },
                { number: 13, title: 'Integration', filename: 'HELM Workbook 13 Integration.pdf' },
                { number: 14, title: 'Applications of Integration 1', filename: 'HELM Workbook 14 Applications of Integration 1.pdf' },
                { number: 15, title: 'Applications of Integration 2', filename: 'HELM Workbook 15 Applications of Integration 2.pdf' },
                { number: 16, title: 'Sequences and Series', filename: 'HELM Workbook 16 Sequences and Series.pdf' },
                { number: 17, title: 'Conics and Polar Coordinates', filename: 'HELM Workbook 17 Conics and Polar Coordinates.pdf' },
                { number: 18, title: 'Functions of Several Variables', filename: 'HELM Workbook 18 Functions of Several Variables.pdf' },
                { number: 19, title: 'Differential Equations', filename: 'HELM Workbook 19 Differential Equations.pdf' },
                { number: 20, title: 'Laplace Transforms', filename: 'HELM Workbook 20 Laplace Transforms.pdf' },
                { number: 21, title: 'z-Transforms', filename: 'HELM Workbook 21 z-Transforms.pdf' },
                { number: 22, title: 'Eigenvalues and Eigenvectors', filename: 'HELM Workbook 22 Eigenvalues and Eigenvectors.pdf' },
                { number: 23, title: 'Fourier Series', filename: 'HELM Workbook 23 Fourier Series.pdf' },
                { number: 24, title: 'Fourier Transforms', filename: 'HELM Workbook 24 Fourier Transforms.pdf' },
                { number: 25, title: 'Partial Differential Equations', filename: 'HELM Workbook 25 Partial Differential Equations.pdf' },
                { number: 26, title: 'Functions of a Complex Variable', filename: 'HELM Workbook 26 Functions of a Complex Variable.pdf' },
                { number: 27, title: 'Multiple Integration', filename: 'HELM Workbook 27 Multiple Integration.pdf' },
                { number: 28, title: 'Differential Vector Calculus', filename: 'HELM Workbook 28 Differential Vector Calculus.pdf' },
                { number: 29, title: 'Integral Vector Calculus', filename: 'HELM Workbook 29 Integral Vector Calculus.pdf' },
                { number: 30, title: 'Introduction to Numerical Methods', filename: 'HELM Workbook 30 Introduction to Numerical Methods.pdf' },
                { number: 31, title: 'Numerical Methods of Approximation', filename: 'HELM Workbook 31 Numerical Methods of Approximation.pdf' },
                { number: 32, title: 'Numerical Initial Value Problems', filename: 'HELM Workbook 32 Numerical Initial Value Problems.pdf' },
                { number: 33, title: 'Numerical Boundary Value Problems', filename: 'HELM Workbook 33 Numerical Boundary Value Problems.pdf' },
                { number: 34, title: 'Modelling Motion', filename: 'HELM Workbook 34 Modelling Motion.pdf' },
                { number: 35, title: 'Sets and Probability', filename: 'HELM Workbook 35 Sets and Probability.pdf' },
                { number: 36, title: 'Descriptive Statistics', filename: 'HELM Workbook 36 Descriptive Statistics.pdf' },
                { number: 37, title: 'Discrete Probability Distributions', filename: 'HELM Workbook 37 Discrete Probability Distributions.pdf' },
                { number: 38, title: 'Continuous Probability Distributions', filename: 'HELM Workbook 38 Continuous Probability Distributions.pdf' },
                { number: 39, title: 'The Normal Distribution', filename: 'HELM Workbook 39 The Normal Distribution.pdf' },
                { number: 40, title: 'Sampling Distributions and Estimation', filename: 'HELM Workbook 40 Sampling Distributions and Estimation.pdf' },
                { number: 41, title: 'Hypothesis Testing', filename: 'HELM Workbook 41 Hypothesis Testing.pdf' },
                { number: 42, title: 'Goodness of Fit and Contingency Tables', filename: 'HELM Workbook 42 Goodness of Fit and Contingency Tables.pdf' },
                { number: 43, title: 'Regression and Correlation', filename: 'HELM Workbook 43 Regression and Correlation.pdf' },
                { number: 44, title: 'Analysis of Variance', filename: 'HELM Workbook 44 Analysis of Variance.pdf' },
                { number: 45, title: 'Non-parametric Statistics', filename: 'HELM Workbook 45 Non-parametric Statistics.pdf' },
                { number: 46, title: 'Reliability and Quality Control', filename: 'HELM Workbook 46 Reliability and Quality Control.pdf' },
                { number: 47, title: 'Mathematics and Physics Miscellany', filename: 'HELM Workbook 47 Mathematics and Physics Miscellany.pdf' },
                { number: 48, title: 'Engineering Case Studies', filename: 'HELM Workbook 48 Engineering Case Studies.pdf' },
                { number: 49, title: 'Student\'s Guide', filename: 'HELM Workbook 49 Student\'s Guide.pdf' },
                { number: 50, title: 'Tutor\'s Guide', filename: 'HELM Workbook 50 Tutor\'s Guide.pdf' }
            ];

            return workbooks.map(workbook => ({
                id: `helm_${workbook.number}`,
                title: `HELM ${workbook.number}: ${workbook.title}`,
                filename: workbook.filename,
                url: `${baseUrl}/media/media/schoolanddepartments/mlsc/downloads/${encodeURIComponent(workbook.filename)}`,
                number: workbook.number
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
            // Check if workbook is cached
            const cachedMetadata = await this.loadWorkbookFromCache(workbook.url);

            if (cachedMetadata) {
                // Use cached file
                const cacheFilePath = await this.getCacheFilePath(workbook.url);
                const uri = this.vscode.Uri.file(cacheFilePath);
                await this.vscode.commands.executeCommand('vscode.open', uri);
                return;
            }

            // Not cached or expired, download and cache
            const https = require('https');
            const http = require('http');
            const fs = require('fs').promises;

            // Get cache file path
            const cacheFilePath = await this.getCacheFilePath(workbook.url);

            // Download the PDF
            const url = new URL(workbook.url);
            const protocol = url.protocol === 'https:' ? https : http;

            const response = await new Promise((resolve, reject) => {
                const request = protocol.get(workbook.url, (res) => {
                    if (res.statusCode !== 200) {
                        reject(new Error(`Failed to download PDF: HTTP ${res.statusCode}`));
                        return;
                    }

                    if (!res.headers['content-type']?.includes('application/pdf')) {
                        reject(new Error('URL does not point to a PDF file'));
                        return;
                    }

                    resolve(res);
                });

                request.on('error', reject);
                request.setTimeout(30000, () => {
                    request.destroy();
                    reject(new Error('Download timeout'));
                });
            });

            // Save the PDF to cache
            const fileStream = require('fs').createWriteStream(cacheFilePath);
            response.pipe(fileStream);

            await new Promise((resolve, reject) => {
                fileStream.on('finish', resolve);
                fileStream.on('error', reject);
            });

            // Save cache metadata
            await this.saveWorkbookToCache(workbook.url, workbook.title, cacheFilePath);

            // Open the cached PDF in VS Code
            const uri = this.vscode.Uri.file(cacheFilePath);
            await this.vscode.commands.executeCommand('vscode.open', uri);

        } catch (error) {
            // Fallback: try to open externally
            try {
                await this.vscode.env.openExternal(this.vscode.Uri.parse(workbook.url));
            } catch (externalError) {
                this.vscode.window.showErrorMessage(
                    `Failed to open workbook: ${error.message}`,
                    { modal: true },
                    'Got it!'
                );
            }
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
                            // Use intelligent workbook matching based on quiz theme
                            const matchedWorkbook = await this.findWorkbookForQuiz(quiz);
                            if (matchedWorkbook) {
                                await this.openWorkbook(matchedWorkbook);
                            } else {
                                // Fallback: try basic string matching with quiz workbook reference
                                const workbooks = await this.getWorkbooks();
                                let workbookToOpen;

                                if (quiz.workbook) {
                                    workbookToOpen = workbooks.find(wb =>
                                        wb.title.includes(quiz.workbook) ||
                                        quiz.workbook.includes(wb.title)
                                    );
                                }

                                if (workbookToOpen) {
                                    await this.openWorkbook(workbookToOpen);
                                } else if (workbooks.length > 0) {
                                    // Final fallback: open first available workbook
                                    await this.openWorkbook(workbooks[0]);
                                } else {
                                    this.vscode.window.showErrorMessage('No workbooks available');
                                }
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
            question: this.markdownToHtml(this.processMathContent(question.question)),
            options: question.options.map(option => {
                // Process math first, then markdown if needed
                const mathProcessed = this.processMathContent(option);
                return option.includes('*') || option.includes('`') || option.includes('\n') 
                    ? this.markdownToHtml(mathProcessed) 
                    : mathProcessed;
            }),
            explanation: question.explanation ? this.markdownToHtml(this.processMathContent(question.explanation)) : question.explanation
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

    /**
     * Find the appropriate workbook for a quiz based on its topic/theme
     * @param {Object} quiz - Quiz object
     * @returns {Promise<Object|null>} Matching workbook or null
     */
    async findWorkbookForQuiz(quiz) {
        const workbooks = await this.getWorkbooks();
        const quizTitle = quiz.title.toLowerCase();
        const quizId = quiz.id.toLowerCase();

        // First, check if quiz has explicit workbook reference
        if (quiz.workbook) {
            const explicitMatch = workbooks.find(wb =>
                wb.title.toLowerCase().includes(quiz.workbook.toLowerCase()) ||
                quiz.workbook.toLowerCase().includes(wb.title.toLowerCase())
            );
            if (explicitMatch) return explicitMatch;
        }

        // Topic-based mapping using keywords and patterns
        const topicMappings = [
            // Workbook 1: Basic Algebra
            {
                workbookNum: 1,
                keywords: ['basic algebra', 'algebra fundamentals', 'algebra basics'],
                patterns: []
            },
            // Workbook 2: Basic Functions
            {
                workbookNum: 2,
                keywords: ['basic functions', 'functions basics', 'function concepts'],
                patterns: [/function/i]
            },
            // Workbook 3: Equations, Inequalities and Partial Fractions
            {
                workbookNum: 3,
                keywords: ['equations', 'inequalities', 'partial fractions'],
                patterns: [/equation/i, /inequality/i, /fraction/i]
            },
            // Workbook 4: Trigonometry
            {
                workbookNum: 4,
                keywords: ['trigonometry', 'trig', 'sine', 'cosine', 'tangent'],
                patterns: [/trig/i, /sin|cos|tan/i]
            },
            // Workbook 5: Functions and Modelling
            {
                workbookNum: 5,
                keywords: ['functions and modelling', 'modelling', 'modeling'],
                patterns: [/model/i]
            },
            // Workbook 6: Exponential and Logarithmic Functions
            {
                workbookNum: 6,
                keywords: ['exponential', 'logarithmic', 'exp', 'log'],
                patterns: [/exp/i, /log/i]
            },
            // Workbook 7: Matrices
            {
                workbookNum: 7,
                keywords: ['matrices introduction', 'matrix introduction', 'matrices definitions'],
                patterns: [/matrices?.*introduction|matrix.*introduction/i]
            },
            // Workbook 8: Matrix Solution of Equations
            {
                workbookNum: 8,
                keywords: ['matrix solution', 'cramers rule', 'gaussian elimination', 'gauss elimination'],
                patterns: [/matrix.*solution|cramer|gauss/i]
            },
            // Workbook 9: Vectors
            {
                workbookNum: 9,
                keywords: ['vectors', 'vector', 'position vector', 'direction', 'magnitude'],
                patterns: [/vector/i, /direction|cosine|ratio/i]
            },
            // Workbook 10: Complex Numbers
            {
                workbookNum: 10,
                keywords: ['complex numbers', 'complex'],
                patterns: [/complex/i]
            },
            // Workbook 11: Differentiation
            {
                workbookNum: 11,
                keywords: ['differentiation', 'derivatives', 'derivative'],
                patterns: [/differenti|deriv/i]
            },
            // Workbook 12: Applications of Differentiation
            {
                workbookNum: 12,
                keywords: ['applications of differentiation', 'differentiation applications'],
                patterns: [/application.*differenti|differenti.*application/i]
            },
            // Workbook 13: Integration
            {
                workbookNum: 13,
                keywords: ['integration', 'integral'],
                patterns: [/integr/i]
            },
            // Workbook 14: Applications of Integration 1
            {
                workbookNum: 14,
                keywords: ['applications of integration', 'integration applications'],
                patterns: [/application.*integr|integr.*application/i]
            },
            // Workbook 15: Applications of Integration 2
            {
                workbookNum: 15,
                keywords: ['applications of integration 2', 'integration applications 2'],
                patterns: []
            },
            // Workbook 16: Sequences and Series
            {
                workbookNum: 16,
                keywords: ['sequences', 'series', 'sequence', 'serie'],
                patterns: [/sequence|serie/i]
            },
            // Workbook 17: Conics and Polar Coordinates
            {
                workbookNum: 17,
                keywords: ['conics', 'polar coordinates', 'polar', 'conic'],
                patterns: [/conic|polar/i]
            },
            // Workbook 18: Functions of Several Variables
            {
                workbookNum: 18,
                keywords: ['several variables', 'multiple variables', 'multivariable'],
                patterns: [/several.*variable|multiple.*variable|multivariable/i]
            },
            // Workbook 19: Differential Equations
            {
                workbookNum: 19,
                keywords: ['differential equations', 'diff eq'],
                patterns: [/differential.*equation/i]
            },
            // Workbook 20: Laplace Transforms
            {
                workbookNum: 20,
                keywords: ['laplace', 'laplace transforms'],
                patterns: [/laplace/i]
            },
            // Workbook 21: z-Transforms
            {
                workbookNum: 21,
                keywords: ['z-transforms', 'z transform'],
                patterns: [/z.transform/i]
            },
            // Workbook 22: Eigenvalues and Eigenvectors
            {
                workbookNum: 22,
                keywords: ['eigenvalues', 'eigenvectors', 'eigen'],
                patterns: [/eigen/i]
            },
            // Workbook 23: Fourier Series
            {
                workbookNum: 23,
                keywords: ['fourier series', 'fourier'],
                patterns: [/fourier/i]
            },
            // Workbook 24: Fourier Transforms
            {
                workbookNum: 24,
                keywords: ['fourier transforms', 'fourier transform'],
                patterns: [/fourier.*transform/i]
            },
            // Workbook 25: Partial Differential Equations
            {
                workbookNum: 25,
                keywords: ['partial differential equations', 'pde'],
                patterns: [/partial.*differential/i]
            },
            // Workbook 26: Functions of a Complex Variable
            {
                workbookNum: 26,
                keywords: ['complex variable', 'complex variables'],
                patterns: [/complex.*variable/i]
            },
            // Workbook 27: Multiple Integration
            {
                workbookNum: 27,
                keywords: ['multiple integration', 'double integral', 'triple integral'],
                patterns: [/multiple.*integr|double.*integr|triple.*integr/i]
            },
            // Workbook 28: Differential Vector Calculus
            {
                workbookNum: 28,
                keywords: ['differential vector calculus', 'vector calculus'],
                patterns: [/vector.*calculus/i]
            },
            // Workbook 29: Integral Vector Calculus
            {
                workbookNum: 29,
                keywords: ['integral vector calculus'],
                patterns: [/integral.*vector/i]
            },
            // Workbook 30: Introduction to Numerical Methods
            {
                workbookNum: 30,
                keywords: ['numerical methods', 'numerical'],
                patterns: [/numerical/i]
            }
        ];

        // Check for matches based on title and ID
        for (const mapping of topicMappings) {
            // Check keywords
            for (const keyword of mapping.keywords) {
                if (quizTitle.includes(keyword) || quizId.includes(keyword.replace(/\s+/g, '_'))) {
                    const workbook = workbooks.find(wb => wb.number === mapping.workbookNum);
                    if (workbook) return workbook;
                }
            }

            // Check patterns
            for (const pattern of mapping.patterns) {
                if (pattern.test(quizTitle) || pattern.test(quizId)) {
                    const workbook = workbooks.find(wb => wb.number === mapping.workbookNum);
                    if (workbook) return workbook;
                }
            }
        }

        // Special handling for matrix-related quizzes that might not have explicit workbook refs
        if (quizTitle.includes('matrix') || quizTitle.includes('matrices') || quizId.includes('matrix')) {
            // Try to determine specific matrix workbook based on content
            if (quizTitle.includes('multiplication') || quizId.includes('multiplication')) {
                return workbooks.find(wb => wb.number === 7); // Matrices workbook
            }
            if (quizTitle.includes('determinant') || quizId.includes('determinant')) {
                return workbooks.find(wb => wb.number === 7); // Matrices workbook
            }
            if (quizTitle.includes('inverse') || quizId.includes('inverse')) {
                return workbooks.find(wb => wb.number === 7); // Matrices workbook
            }
            if (quizTitle.includes('gaussian') || quizId.includes('gaussian') ||
                quizTitle.includes('elimination') || quizId.includes('elimination')) {
                return workbooks.find(wb => wb.number === 8); // Matrix Solution workbook
            }
            // Default to matrices workbook
            return workbooks.find(wb => wb.number === 7);
        }

        // Special handling for vector-related quizzes
        if (quizTitle.includes('vector') || quizId.includes('vector')) {
            return workbooks.find(wb => wb.number === 9); // Vectors workbook
        }

        // Special handling for calculus-related quizzes
        if (quizTitle.includes('limit') || quizId.includes('limit') ||
            quizTitle.includes('continuity') || quizId.includes('continuity')) {
            return workbooks.find(wb => wb.number === 11); // Differentiation workbook
        }

        // If no specific match found, return null to allow fallback
        return null;
    }
}

module.exports = MathematicsManager;