/**
 * TSI Header - Learn Mode
 * 
 * Interactive learning platform for programming languages within VS Code.
 * Provides structured lessons, exercises, and progress tracking.
 * 
 * @module learn
 */

const LearnManager = require('./lib/learn_manager');
const ProgressTracker = require('./lib/progress_tracker');
const ExerciseRunner = require('./lib/exercise_runner');

class Learn {
    constructor(context, vscode) {
        this.context = context;
        this.vscode = vscode;
        
        this.learnManager = new LearnManager(context, vscode);
        this.progressTracker = new ProgressTracker(context);
        this.exerciseRunner = new ExerciseRunner(vscode);
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
            const result = await this.exerciseRunner.run(language, exercise);
            
            // Handle manual exercises (no automated tests)
            if (result.isManual) {
                this.vscode.window.showInformationMessage(
                    `📝 Manual Exercise: "${exercise.title}"\n\n${result.message}`,
                    { modal: true },
                    'Mark Complete',
                    'Cancel'
                ).then(async selection => {
                    if (selection === 'Mark Complete') {
                        await this.progressTracker.recordCompletion(language, exercise.id);
                        
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
                await this.progressTracker.recordCompletion(language, exercise.id);
                
                // Get next lesson in curriculum
                const curriculum = await this.learnManager.loadCurriculum(language);
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
                        this.showSolution(language, exercise);
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
                        this.showSolution(language, exercise);
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

    /**
     * Show the solution for an exercise
     * @param {string} language - The programming language
     * @param {Object} exercise - The exercise
     */
    async showSolution(language, exercise) {
        try {
            const solution = await this.learnManager.loadSolution(language, exercise.id);
            
            // Create a new untitled document with the solution
            const doc = await this.vscode.workspace.openTextDocument({
                content: solution.code,
                language: solution.languageId
            });
            
            await this.vscode.window.showTextDocument(doc);
            
            // Show explanation
            if (solution.explanation) {
                this.vscode.window.showInformationMessage(
                    `📖 Solution Explanation: ${solution.explanation}`,
                    { modal: true }
                );
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
        
        // Show quick pick of completed lessons
        const items = completedLessons.map(lessonId => ({
            label: `Lesson: ${lessonId}`,
            description: 'Click to review',
            lessonId
        }));
        
        const selected = await this.vscode.window.showQuickPick(items, {
            placeHolder: 'Select a lesson to review'
        });
        
        if (selected) {
            await this.learnManager.openLesson(language, { id: selected.lessonId });
        }
    }

    /**
     * Browse and jump to any lesson in the curriculum
     * @param {string} language - The programming language
     */
    async browseLessons(language) {
        try {
            // Load curriculum
            const curriculum = await this.learnManager.loadCurriculum(language);
            const progress = await this.progressTracker.getProgress(language);
            const completedLessons = new Set(progress.completed || []);
            
            // Build lesson list grouped by module
            const items = [];
            
            for (const module of curriculum.modules) {
                // Add module header as separator (no icons needed)
                items.push({
                    label: `📁 ${module.title}`,
                    kind: this.vscode.QuickPickItemKind.Separator
                });
                
                // Add lessons in this module
                for (const lesson of module.lessons) {
                    const isCompleted = completedLessons.has(lesson.id);
                    const icon = isCompleted ? '✓' : '○';
                    const status = isCompleted ? 'Completed' : 'Not started';
                    
                    items.push({
                        label: `  ${icon} ${lesson.title}`,
                        description: `${lesson.duration || '?'} min • ${lesson.difficulty || 'beginner'}`,
                        detail: status,
                        lessonId: lesson.id,
                        moduleTitle: module.title,
                        moduleId: module.id,
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
                    id: selected.lessonId,
                    title: selected.lesson.title,
                    moduleTitle: selected.moduleTitle,
                    moduleId: selected.moduleId
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
     * Dispose of resources
     */
    dispose() {
        // Cleanup if needed
    }
}

module.exports = Learn;
