const path = require('path');
const fs = require('fs');

function register(context, deps) {
    const { vscode } = deps;
    
    // Register Learn commands
    const learnRubyCommand = vscode.commands.registerCommand('tsiheader.learnRuby', async () => {
        // Lazy load the Learn module
        const Learn = require(path.join(__dirname, '..', '..', '..', 'learn', 'index.js'));
        const learnInstance = new Learn(context, vscode);
        
        // Show centered modal dialog
        vscode.window.showInformationMessage(
            '📚 Start Ruby Learning Journey?\n\n' +
            'You will begin an interactive Ruby programming course with:\n' +
            '• 6 modules from beginner to advanced\n' +
            '• 18 lessons with hands-on exercises\n' +
            '• Progress tracking and achievements\n' +
            '• Instant feedback on your code\n\n' +
            'Ready to start learning?',
            { modal: true },
            'Start Learning',
            'Browse Lessons',
            'View Progress',
            'Cancel'
        ).then(async selection => {
            if (selection === 'Start Learning') {
                await learnInstance.startLearning('ruby');
            } else if (selection === 'Browse Lessons') {
                await learnInstance.browseLessons('ruby');
            } else if (selection === 'View Progress') {
                try {
                    const stats = await learnInstance.getStats('ruby');
                    vscode.window.showInformationMessage(
                        `📊 Your Ruby Learning Progress\n\n` +
                        `Lessons Completed: ${stats.lessonsCompleted}\n` +
                        `Exercises Completed: ${stats.exercisesCompleted}\n` +
                        `Current Streak: ${stats.currentStreak} days\n` +
                        `Study Time: ${stats.totalStudyTime} minutes\n` +
                        `Achievements: ${stats.achievements}`,
                        { modal: true },
                        'Continue Learning',
                        'Got it!'
                    ).then(choice => {
                        if (choice === 'Continue Learning') {
                            learnInstance.startLearning('ruby');
                        }
                    });
                } catch (error) {
                    console.error('Error getting progress stats:', error);
                    vscode.window.showErrorMessage(
                        `Error loading progress: ${error.message}`,
                        { modal: true },
                        'OK'
                    );
                }
            }
        });
    });
    
    context.subscriptions.push(learnRubyCommand);

    // Register Learn Rust command
    const learnRustCommand = vscode.commands.registerCommand('tsiheader.learnRust', async () => {
        const Learn = require(path.join(__dirname, '..', '..', '..', 'learn', 'index.js'));
        const learnInstance = new Learn(context, vscode);

        vscode.window.showInformationMessage(
            '📚 Start Rust Learning Journey?\n\n' +
            'You will begin a concise Rust course with:\n' +
            '• A small module and a hands-on exercise\n' +
            '• Progress tracking and instant feedback\n\n' +
            'Ready to start learning?',
            { modal: true },
            'Start Learning',
            'Browse Lessons',
            'View Progress',
            'Cancel'
        ).then(async selection => {
            if (selection === 'Start Learning') {
                await learnInstance.startLearning('rust');
            } else if (selection === 'Browse Lessons') {
                await learnInstance.browseLessons('rust');
            } else if (selection === 'View Progress') {
                try {
                    const stats = await learnInstance.getStats('rust');
                    vscode.window.showInformationMessage(
                        `📊 Your Rust Learning Progress\n\n` +
                        `Lessons Completed: ${stats.lessonsCompleted}\n` +
                        `Exercises Completed: ${stats.exercisesCompleted}\n` +
                        `Current Streak: ${stats.currentStreak} days\n` +
                        `Study Time: ${stats.totalStudyTime} minutes\n` +
                        `Achievements: ${stats.achievements}`,
                        { modal: true },
                        'Continue Learning',
                        'Got it!'
                    ).then(choice => {
                        if (choice === 'Continue Learning') {
                            learnInstance.startLearning('rust');
                        }
                    });
                } catch (error) {
                    console.error('Error getting rust progress stats:', error);
                    vscode.window.showErrorMessage(`Error loading progress: ${error.message}`, { modal: true }, 'OK');
                }
            }
        });
    });

    context.subscriptions.push(learnRustCommand);

    // Register Browse Lessons command
    const browseLessonsCommand = vscode.commands.registerCommand('tsiheader.browseLessons', async () => {
        // Lazy load the Learn module
        const Learn = require(path.join(__dirname, '..', '..', '..', 'learn', 'index.js'));
        const learnInstance = new Learn(context, vscode);
        
        // For now, only Ruby is supported
        await learnInstance.browseLessons('ruby');
    });
    
    context.subscriptions.push(browseLessonsCommand);

    // Register View Learn Progress command
    const viewLearnProgressCommand = vscode.commands.registerCommand('tsiheader.viewLearnProgress', async () => {
        // Lazy load the Learn module
        const Learn = require(path.join(__dirname, '..', '..', '..', 'learn', 'index.js'));
        const learnInstance = new Learn(context, vscode);
        
        try {
            const stats = await learnInstance.getStats('ruby');
            vscode.window.showInformationMessage(
                `📊 Your Ruby Learning Progress\n\n` +
                `Lessons Completed: ${stats.lessonsCompleted}\n` +
                `Exercises Completed: ${stats.exercisesCompleted}\n` +
                `Current Streak: ${stats.currentStreak} days\n` +
                `Study Time: ${stats.totalStudyTime} minutes\n` +
                `Achievements: ${stats.achievements}`,
                { modal: true },
                'Continue Learning',
                'Browse Lessons',
                'Got it!'
            ).then(async choice => {
                if (choice === 'Continue Learning') {
                    await learnInstance.startLearning('ruby');
                } else if (choice === 'Browse Lessons') {
                    await learnInstance.browseLessons('ruby');
                }
            });
        } catch (error) {
            console.error('Error getting progress stats:', error);
            vscode.window.showErrorMessage(
                `Error loading progress: ${error.message}`,
                { modal: true },
                'OK'
            );
        }
    });
    
    context.subscriptions.push(viewLearnProgressCommand);

    // Register Browse Lessons Rust command
    const browseLessonsRustCommand = vscode.commands.registerCommand('tsiheader.browseLessonsRust', async () => {
        const Learn = require(path.join(__dirname, '..', '..', '..', 'learn', 'index.js'));
        const learnInstance = new Learn(context, vscode);
        await learnInstance.browseLessons('rust');
    });
    context.subscriptions.push(browseLessonsRustCommand);

    // Register View Learn Progress Rust command
    const viewLearnProgressRustCommand = vscode.commands.registerCommand('tsiheader.viewLearnProgressRust', async () => {
        const Learn = require(path.join(__dirname, '..', '..', '..', 'learn', 'index.js'));
        const learnInstance = new Learn(context, vscode);
        try {
            const stats = await learnInstance.getStats('rust');
            vscode.window.showInformationMessage(
                `📊 Your Rust Learning Progress\n\n` +
                `Lessons Completed: ${stats.lessonsCompleted}\n` +
                `Exercises Completed: ${stats.exercisesCompleted}\n` +
                `Current Streak: ${stats.currentStreak} days\n` +
                `Study Time: ${stats.totalStudyTime} minutes\n` +
                `Achievements: ${stats.achievements}`,
                { modal: true },
                'Continue Learning',
                'Browse Lessons',
                'Got it!'
            ).then(async choice => {
                if (choice === 'Continue Learning') {
                    await learnInstance.startLearning('rust');
                } else if (choice === 'Browse Lessons') {
                    await learnInstance.browseLessons('rust');
                }
            });
        } catch (error) {
            console.error('Error getting rust progress stats:', error);
            vscode.window.showErrorMessage(`Error loading progress: ${error.message}`, { modal: true }, 'OK');
        }
    });
    context.subscriptions.push(viewLearnProgressRustCommand);

    // Register Learn exercise test command
    const runExerciseTestsCommand = vscode.commands.registerCommand('tsiheader.runExerciseTests', async (runtimeLanguageArg, exerciseMetaArg) => {
        let normalizedRuntimeArg = runtimeLanguageArg;
        let normalizedExerciseMeta = exerciseMetaArg;

        if (runtimeLanguageArg && typeof runtimeLanguageArg === 'object' && !Array.isArray(runtimeLanguageArg)) {
            normalizedRuntimeArg = runtimeLanguageArg.language || runtimeLanguageArg.lang || undefined;
            normalizedExerciseMeta = runtimeLanguageArg.exerciseMetadata || runtimeLanguageArg.metadata || exerciseMetaArg;
        }

        const editor = vscode.window.activeTextEditor;
        if (!editor) {
            vscode.window.showWarningMessage(
                'No Exercise File Open\n\nPlease open your exercise file before running tests.',
                { modal: true },
                'Got it!'
            );
            return;
        }

        const filePath = editor.document.fileName;
        const fileName = path.basename(filePath);
        
        // Check if this is a learn exercise file
        if (!filePath.includes('learn_exercises')) {
            vscode.window.showWarningMessage(
                'Not a Learn Exercise\n\nThis command only works with Learn exercise files.\nExercise files are located in the learn_exercises/ folder.',
                { modal: true },
                'Got it!'
            );
            return;
        }

        // Lazy load the Learn module
        const Learn = require(path.join(__dirname, '..', '..', '..', 'learn', 'index.js'));
        const learnInstance = new Learn(context, vscode);

        // Determine language from file path
        const pathParts = filePath.split(path.sep);
        const learnExercisesIndex = pathParts.indexOf('learn_exercises');
        let runtimeLanguage = normalizedRuntimeArg || pathParts[learnExercisesIndex + 1];

        try {
            const fs = require('fs');
            let exerciseMetadata = normalizedExerciseMeta || learnInstance.getExerciseMetadata(filePath);

            if (!runtimeLanguage && exerciseMetadata?.variantLanguage) {
                runtimeLanguage = exerciseMetadata.variantLanguage;
            }

            let baseExerciseId;
            let curriculumLanguage = exerciseMetadata?.curriculumLanguage || pathParts[learnExercisesIndex - 1] || runtimeLanguage;
            let variantId;

            let exerciseJsonPath;
            let exerciseData;

            if (exerciseMetadata) {
                baseExerciseId = exerciseMetadata.baseExerciseId || exerciseMetadata.baseExerciseFile || fileName.replace(/\.[^/.]+$/, '');
                variantId = exerciseMetadata.variantId || fileName.replace(/\.[^/.]+$/, '');
                exerciseJsonPath = path.join(__dirname, '..', '..', '..', 'learn', 'curriculum', curriculumLanguage, 'exercises', `${exerciseMetadata.baseExerciseFile || baseExerciseId}.json`);
            } else {
                const exerciseId = fileName.replace(/\.(rb|py|js|cpp|c)$/, '');
                baseExerciseId = exerciseId;
                variantId = exerciseId;
                exerciseJsonPath = path.join(__dirname, '..', '..', '..', 'learn', 'curriculum', runtimeLanguage, 'exercises', `${exerciseId}.json`);
                curriculumLanguage = runtimeLanguage;
            }

            if (!fs.existsSync(exerciseJsonPath)) {
                await vscode.window.showErrorMessage(
                    `❌ Exercise Not Found\n\nCouldn't find exercise definition: ${path.basename(exerciseJsonPath)}`,
                    { modal: true },
                    'Got it!'
                );
                return;
            }

            exerciseData = JSON.parse(fs.readFileSync(exerciseJsonPath, 'utf8'));

            let variant = null;
            if (Array.isArray(exerciseData.variants) && exerciseData.variants.length > 0) {
                variant = exerciseData.variants.find(v => v.id === variantId) || exerciseData.variants[0];
                if (!runtimeLanguage && variant?.language) {
                    runtimeLanguage = variant.language;
                }
            }

            const executionLanguage = (variant?.language || exerciseMetadata?.variantLanguage || runtimeLanguage || curriculumLanguage || 'cpp').toLowerCase();
            const runnerTests = variant?.tests || exerciseData.tests || [];
            const exerciseTitle = variant?.title || exerciseData.title || exerciseMetadata?.title || baseExerciseId;
            const exerciseDescription = variant?.description || exerciseData.description || '';
            const exerciseHints = variant?.hints || exerciseData.hints || [];
            const exerciseDifficulty = variant?.difficulty || exerciseMetadata?.difficulty || exerciseData.difficulty || 'beginner';
            const progressId = variant?.id || exerciseData.id || baseExerciseId;
            const normalizeLessonId = (value) => {
                if (!value) {
                    return null;
                }
                let normalized = value.toString();
                normalized = normalized.replace(/_exercise$/, '');
                normalized = normalized.replace(/_solution$/, '');
                normalized = normalized.replace(/_variant$/, '');
                return normalized;
            };

            const baseExerciseForLesson = exerciseMetadata?.baseExerciseFile || baseExerciseId;
            const lessonId = normalizeLessonId(
                exerciseMetadata?.lessonId ||
                baseExerciseForLesson ||
                progressId ||
                variant?.id
            );

            const exercise = {
                id: progressId,
                baseExerciseId,
                baseExerciseFile: exerciseMetadata?.baseExerciseFile || baseExerciseId,
                variantId: variant?.id,
                title: exerciseTitle,
                description: exerciseDescription,
                language: executionLanguage,
                curriculumLanguage,
                tests: runnerTests,
                hints: exerciseHints,
                difficulty: exerciseDifficulty,
                progressId,
                hasVariants: Array.isArray(exerciseData.variants) && exerciseData.variants.length > 0,
                lessonId
            };

            // Run with progress indicator
            await vscode.window.withProgress({
                location: vscode.ProgressLocation.Notification,
                title: "🔄 Running Tests...",
                cancellable: false
            }, async (progress) => {
                progress.report({ message: "Testing your solution..." });
                
                try {
                    // Run the exercise - this will show modal dialogs
                    const result = await learnInstance.runExercise(curriculumLanguage, exercise);
                    console.log('Exercise result:', result);
                    return result;
                } catch (error) {
                    console.error('Exercise run error:', error);
                    throw error;
                }
            });
            
        } catch (error) {
            console.error('Test execution error:', error);
            await vscode.window.showErrorMessage(
                `❌ Test Execution Failed\n\n${error.message}\n\n${error.stack || ''}`,
                { modal: true },
                'Got it!'
            );
        }
    });

    context.subscriptions.push(runExerciseTestsCommand);

    // C Learning Commands
    const learnCCommand = vscode.commands.registerCommand('tsiheader.learnC', async () => {
        try {
            const Learn = require(path.join(__dirname, '..', '..', '..', 'learn', 'index.js'));
            const learnInstance = new Learn(context, vscode);

            vscode.window.showInformationMessage(
                '⚙️ Start C Learning Journey?\n\n' +
                'Launch an interactive C curriculum featuring:\n' +
                '• Core concepts of memory, pointers, and data structures\n' +
                '• Hands-on exercises with automated feedback\n' +
                '• Progress tracking, streaks, and achievements\n\n' +
                'Ready to begin?',
                { modal: true },
                'Start Learning',
                'Browse Lessons',
                'View Progress',
                'Cancel'
            ).then(async selection => {
                if (selection === 'Start Learning') {
                    await learnInstance.startLearning('c');
                } else if (selection === 'Browse Lessons') {
                    await learnInstance.browseLessons('c');
                } else if (selection === 'View Progress') {
                    try {
                        const stats = await learnInstance.getStats('c');
                        vscode.window.showInformationMessage(
                            `📊 Your C Learning Progress\n\n` +
                            `Lessons Completed: ${stats.lessonsCompleted}\n` +
                            `Exercises Completed: ${stats.exercisesCompleted}\n` +
                            `Current Streak: ${stats.currentStreak} days\n` +
                            `Study Time: ${stats.totalStudyTime} minutes\n` +
                            `Achievements: ${stats.achievements}`,
                            { modal: true },
                            'Continue Learning',
                            'Got it!'
                        ).then(choice => {
                            if (choice === 'Continue Learning') {
                                learnInstance.startLearning('c');
                            }
                        });
                    } catch (progressError) {
                        vscode.window.showErrorMessage(`Error loading progress: ${progressError.message}`, { modal: true }, 'OK');
                    }
                }
            });
        } catch (error) {
            vscode.window.showErrorMessage(`Error starting C learning: ${error.message}`);
        }
    });

    context.subscriptions.push(learnCCommand);

    const browseLessonsCCommand = vscode.commands.registerCommand('tsiheader.browseLessonsC', async () => {
        try {
            const Learn = require(path.join(__dirname, '..', '..', '..', 'learn', 'index.js'));
            const learnInstance = new Learn(context, vscode);
            await learnInstance.browseLessons('c');
        } catch (error) {
            vscode.window.showErrorMessage(`Error browsing C lessons: ${error.message}`);
        }
    });

    context.subscriptions.push(browseLessonsCCommand);

    const viewLearnProgressCCommand = vscode.commands.registerCommand('tsiheader.viewLearnProgressC', async () => {
        try {
            const Learn = require(path.join(__dirname, '..', '..', '..', 'learn', 'index.js'));
            const learnInstance = new Learn(context, vscode);

            const stats = await learnInstance.getStats('c');

            const message = `📊 C Learning Progress\n\n`
                + `Lessons Completed: ${stats.lessonsCompleted}\n`
                + `Exercises Completed: ${stats.exercisesCompleted}\n`
                + `Current Streak: ${stats.currentStreak} days\n`
                + `Total Study Time: ${stats.totalStudyTime} minutes\n`
                + `Achievements: ${stats.achievements}\n\n`
                + `Keep up the great work! 🚀`;

            const action = await vscode.window.showInformationMessage(
                message,
                { modal: true },
                'Continue Learning',
                'View All Lessons'
            );

            if (action === 'Continue Learning') {
                await learnInstance.startLearning('c');
            } else if (action === 'View All Lessons') {
                await learnInstance.browseLessons('c');
            }
        } catch (error) {
            vscode.window.showErrorMessage(`Error viewing C progress: ${error.message}`);
        }
    });

    context.subscriptions.push(viewLearnProgressCCommand);

    const runExerciseTestsCCommand = vscode.commands.registerCommand('tsiheader.runExerciseTestsC', async () => {
        const activeEditor = vscode.window.activeTextEditor;
        if (!activeEditor) {
            vscode.window.showWarningMessage('Please open a C exercise file first.');
            return;
        }

        const filePath = activeEditor.document.fileName;
        const isCFile = filePath.endsWith('.c');
        if (!filePath.includes('learn_exercises') || !isCFile) {
            vscode.window.showWarningMessage('Please open a C exercise file (.c) from the learn_exercises directory.');
            return;
        }

        await vscode.commands.executeCommand('tsiheader.runExerciseTests', { language: 'c' });
    });

    context.subscriptions.push(runExerciseTestsCCommand);

    // C++ Learning Commands
    const learnCppCommand = vscode.commands.registerCommand('tsiheader.learnCpp', async () => {
        try {
            const Learn = require(path.join(__dirname, '..', '..', '..', 'learn', 'index.js'));
            const learnInstance = new Learn(context, vscode);

            vscode.window.showInformationMessage(
                '⚙️ Start C++ Learning Journey?\n\n' +
                'Launch an interactive C++ curriculum featuring:\n' +
                '• 8 modules covering fundamentals to advanced topics\n' +
                '• Hands-on exercises with automated feedback\n' +
                '• Progress tracking, streaks, and achievements\n\n' +
                'Ready to begin?',
                { modal: true },
                'Start Learning',
                'Browse Lessons',
                'View Progress',
                'Cancel'
            ).then(async selection => {
                if (selection === 'Start Learning') {
                    await learnInstance.startLearning('cpp');
                } else if (selection === 'Browse Lessons') {
                    await learnInstance.browseLessons('cpp');
                } else if (selection === 'View Progress') {
                    try {
                        const stats = await learnInstance.getStats('cpp');
                        vscode.window.showInformationMessage(
                            `📊 Your C++ Learning Progress\n\n` +
                            `Lessons Completed: ${stats.lessonsCompleted}\n` +
                            `Exercises Completed: ${stats.exercisesCompleted}\n` +
                            `Current Streak: ${stats.currentStreak} days\n` +
                            `Study Time: ${stats.totalStudyTime} minutes\n` +
                            `Achievements: ${stats.achievements}`,
                            { modal: true },
                            'Continue Learning',
                            'Got it!'
                        ).then(choice => {
                            if (choice === 'Continue Learning') {
                                learnInstance.startLearning('cpp');
                            }
                        });
                    } catch (progressError) {
                        vscode.window.showErrorMessage(`Error loading progress: ${progressError.message}`, { modal: true }, 'OK');
                    }
                }
            });
        } catch (error) {
            vscode.window.showErrorMessage(`Error starting C++ learning: ${error.message}`);
        }
    });

    context.subscriptions.push(learnCppCommand);

    const browseLessonsCppCommand = vscode.commands.registerCommand('tsiheader.browseLessonsCpp', async () => {
        try {
            const Learn = require(path.join(__dirname, '..', '..', '..', 'learn', 'index.js'));
            const learnInstance = new Learn(context, vscode);
            await learnInstance.browseLessons('cpp');
        } catch (error) {
            vscode.window.showErrorMessage(`Error browsing C++ lessons: ${error.message}`);
        }
    });

    context.subscriptions.push(browseLessonsCppCommand);

    const viewLearnProgressCppCommand = vscode.commands.registerCommand('tsiheader.viewLearnProgressCpp', async () => {
        try {
            const Learn = require(path.join(__dirname, '..', '..', '..', 'learn', 'index.js'));
            const learnInstance = new Learn(context, vscode);

            const stats = await learnInstance.getStats('cpp');

            const message = `📊 C++ Learning Progress\n\n`
                + `Lessons Completed: ${stats.lessonsCompleted}\n`
                + `Exercises Completed: ${stats.exercisesCompleted}\n`
                + `Current Streak: ${stats.currentStreak} days\n`
                + `Total Study Time: ${stats.totalStudyTime} minutes\n`
                + `Achievements: ${stats.achievements}\n\n`
                + `Keep up the great work! 🚀`;

            const action = await vscode.window.showInformationMessage(
                message,
                { modal: true },
                'Continue Learning',
                'View All Lessons'
            );

            if (action === 'Continue Learning') {
                await learnInstance.startLearning('cpp');
            } else if (action === 'View All Lessons') {
                await learnInstance.browseLessons('cpp');
            }
        } catch (error) {
            vscode.window.showErrorMessage(`Error viewing C++ progress: ${error.message}`);
        }
    });

    context.subscriptions.push(viewLearnProgressCppCommand);

    const runExerciseTestsCppCommand = vscode.commands.registerCommand('tsiheader.runExerciseTestsCpp', async () => {
        const activeEditor = vscode.window.activeTextEditor;
        if (!activeEditor) {
            vscode.window.showWarningMessage('Please open a C++ exercise file first.');
            return;
        }

        const filePath = activeEditor.document.fileName;
        const isCppFile = filePath.endsWith('.cpp');
        if (!filePath.includes('learn_exercises') || !isCppFile) {
            vscode.window.showWarningMessage('Please open a C++ exercise file (.cpp) from the learn_exercises directory.');
            return;
        }

        await vscode.commands.executeCommand('tsiheader.runExerciseTests');
    });

    context.subscriptions.push(runExerciseTestsCppCommand);

    const learnCppDsaCommand = vscode.commands.registerCommand('tsiheader.learnCppDsa', async () => {
        try {
            const Learn = require(path.join(__dirname, '..', '..', '..', 'learn', 'index.js'));
            const learnInstance = new Learn(context, vscode);

            const message = '🚀 C++ DSA Roadmap\n\n' +
                'Dive into a dedicated Data Structures & Algorithms journey built for modern C++:\n' +
                '• Foundations, arrays, linked lists, and more\n' +
                '• Sorting, trees, graphs, flows, and optimization\n' +
                '• Every lesson ships with rich visuals, 300+ lines of code, and a 10-question quiz\n\n' +
                'Choose how you want to begin.';

            const action = await vscode.window.showInformationMessage(
                message,
                { modal: true },
                'Start Roadmap',
                'Browse Lessons',
                'View Progress'
            );

            if (action === 'Start Roadmap') {
                await learnInstance.startLearning('dsa_cpp');
            } else if (action === 'Browse Lessons') {
                await learnInstance.browseLessons('dsa_cpp');
            } else if (action === 'View Progress') {
                await vscode.commands.executeCommand('tsiheader.viewLearnProgressCppDsa');
            }
        } catch (error) {
            vscode.window.showErrorMessage(`Error starting C++ DSA roadmap: ${error.message}`);
        }
    });

    context.subscriptions.push(learnCppDsaCommand);

    const browseLessonsCppDsaCommand = vscode.commands.registerCommand('tsiheader.browseLessonsCppDsa', async () => {
        try {
            const Learn = require(path.join(__dirname, '..', '..', '..', 'learn', 'index.js'));
            const learnInstance = new Learn(context, vscode);
            await learnInstance.browseLessons('dsa_cpp');
        } catch (error) {
            vscode.window.showErrorMessage(`Error browsing C++ DSA lessons: ${error.message}`);
        }
    });

    context.subscriptions.push(browseLessonsCppDsaCommand);

    const viewLearnProgressCppDsaCommand = vscode.commands.registerCommand('tsiheader.viewLearnProgressCppDsa', async () => {
        try {
            const Learn = require(path.join(__dirname, '..', '..', '..', 'learn', 'index.js'));
            const learnInstance = new Learn(context, vscode);

            const curriculum = await learnInstance.learnManager.loadCurriculum('dsa_cpp');
            const modules = Array.isArray(curriculum.modules) ? curriculum.modules : [];
            const allLessons = modules.flatMap(module => module.lessons || []);

            if (allLessons.length === 0) {
                vscode.window.showWarningMessage('No lessons were found in the C++ DSA roadmap.');
                return;
            }

            const progress = await learnInstance.progressTracker.getProgress('dsa_cpp');
            const normalize = id => learnInstance.progressTracker.normalizeLessonId(id);
            const completedLessons = new Set((progress.completed || [])
                .map(id => normalize(id))
                .filter(Boolean));

            const totalLessons = allLessons.length;
            const completedCount = allLessons.reduce((count, lesson) => {
                const normalizedId = normalize(lesson.id);
                return normalizedId && completedLessons.has(normalizedId) ? count + 1 : count;
            }, 0);

            const remainingCount = totalLessons - completedCount;
            const completionRate = totalLessons === 0 ? 0 : Math.round((completedCount / totalLessons) * 100);

            const nextLesson = allLessons.find(lesson => {
                const normalizedId = normalize(lesson.id);
                return !(normalizedId && completedLessons.has(normalizedId));
            }) || null;

            const messageLines = [
                '📊 C++ DSA Roadmap Progress',
                '',
                `Modules: ${modules.length}`,
                `Lessons Completed: ${completedCount}/${totalLessons} (${completionRate}%)`,
                `Remaining Lessons: ${remainingCount}`
            ];

            if (nextLesson) {
                messageLines.push(`Next Recommended Lesson: ${nextLesson.title || nextLesson.id}`);
            } else {
                messageLines.push('🎉 You have completed every lesson in the roadmap!');
            }

            const stats = await learnInstance.getStats('dsa_cpp');
            messageLines.push('', `Exercises Completed: ${stats.exercisesCompleted}`);
            messageLines.push(`Current Streak: ${stats.currentStreak} days`);
            messageLines.push(`Total Study Time: ${stats.totalStudyTime} minutes`);

            const message = messageLines.join('\n');

            const buttons = [];
            if (nextLesson) {
                buttons.push('Start Next Lesson');
            }
            buttons.push('Browse Lessons');

            const action = await vscode.window.showInformationMessage(
                message,
                { modal: true },
                ...buttons
            );

            if (action === 'Start Next Lesson') {
                await learnInstance.startLearning('dsa_cpp');
            } else if (action === 'Browse Lessons') {
                await learnInstance.browseLessons('dsa_cpp');
            }
        } catch (error) {
            vscode.window.showErrorMessage(`Error viewing C++ DSA progress: ${error.message}`);
        }
    });

    context.subscriptions.push(viewLearnProgressCppDsaCommand);

    const learnGitCommand = vscode.commands.registerCommand('tsiheader.learnGit', async () => {
        try {
            // Lazy load the Learn module to get progress tracker
            const Learn = require(path.join(__dirname, '..', '..', '..', 'learn', 'index.js'));
            const learnInstance = new Learn(context, vscode);

            // Lazy load the Git Book manager with progress tracker
            const GitBookManager = require(path.join(__dirname, '..', '..', '..', 'learn', 'git_manager.js'));
            const gitManager = new GitBookManager(vscode, learnInstance.progressTracker);

            const message = '📖 Pro Git Book - Complete Git Reference\n\n' +
                'Master Git with the official comprehensive guide:\n' +
                '• Getting Started: Installation, setup, and basics\n' +
                '• Git Fundamentals: Repository management, commits, history\n' +
                '• Branching & Merging: Workflows, rebasing, conflict resolution\n' +
                '• Git Server: Hosting, collaboration, and remote management\n' +
                '• Advanced Topics: Internals, customization, and automation\n\n' +
                'Lessons are fetched live from git-scm.com/book.\n\n' +
                'Ready to master Git?';

            const action = await vscode.window.showInformationMessage(
                message,
                { modal: true },
                'Start Getting Started',
                'Browse All Chapters',
                'View Progress'
            );

            if (action === 'Start Getting Started') {
                // Load curriculum and start with first lesson in Getting Started chapter
                const fs = require('fs');
                const curriculumPath = path.join(__dirname, '..', '..', '..', 'learn', 'curriculum', 'git', 'curriculum.json');
                const curriculum = JSON.parse(fs.readFileSync(curriculumPath, 'utf8'));
                const gettingStartedChapter = curriculum.chapters.find(chapter => chapter.id === 'getting-started');
                if (gettingStartedChapter && gettingStartedChapter.lessons && gettingStartedChapter.lessons.length > 0) {
                    const firstLesson = gettingStartedChapter.lessons[0];
                    await gitManager.openLesson(firstLesson, context);
                } else {
                    vscode.window.showErrorMessage('No lessons found in Getting Started chapter');
                }
            } else if (action === 'Browse All Chapters') {
                await vscode.commands.executeCommand('tsiheader.browseLessonsGit');
            } else if (action === 'View Progress') {
                await vscode.commands.executeCommand('tsiheader.viewLearnProgressGit');
            }
        } catch (error) {
            vscode.window.showErrorMessage(`Error starting Git Book: ${error.message}`);
        }
    });

    context.subscriptions.push(learnGitCommand);

    const browseLessonsGitCommand = vscode.commands.registerCommand('tsiheader.browseLessonsGit', async () => {
        try {
            // Lazy load the Learn module to get progress tracker
            const Learn = require(path.join(__dirname, '..', '..', '..', 'learn', 'index.js'));
            const learnInstance = new Learn(context, vscode);

            // Lazy load the Git Book manager with progress tracker
            const GitBookManager = require(path.join(__dirname, '..', '..', '..', 'learn', 'git_manager.js'));
            const gitManager = new GitBookManager(vscode, learnInstance.progressTracker);

            const fs = require('fs');
            const curriculumPath = path.join(__dirname, '..', '..', '..', 'learn', 'curriculum', 'git', 'curriculum.json');
            const curriculum = JSON.parse(fs.readFileSync(curriculumPath, 'utf8'));

            // Get progress to show completion status
            const progress = await gitManager.getProgressStats();
            const completedLessons = new Set(progress.completed || []);

            // Create quick pick items for all lessons
            const items = [];
            curriculum.chapters.forEach(chapter => {
                // Add chapter separator
                items.push({
                    label: `📚 ${chapter.title}`,
                    kind: vscode.QuickPickItemKind.Separator
                });

                // Add lessons within the chapter
                if (chapter.lessons && chapter.lessons.length > 0) {
                    chapter.lessons.forEach(lesson => {
                        const isCompleted = completedLessons.has(lesson.id);
                        const icon = isCompleted ? '✅' : '○';
                        items.push({
                            label: `    ${icon} ${lesson.title}`,
                            description: `${lesson.estimatedHours}h`,
                            detail: isCompleted ? 'Completed' : 'Not started',
                            lesson: lesson
                        });
                    });
                }
            });

            const selected = await vscode.window.showQuickPick(items, {
                placeHolder: 'Select a lesson from Pro Git Book',
                matchOnDescription: true
            });

            if (selected && selected.lesson) {
                await gitManager.openLesson(selected.lesson, context);
            }
        } catch (error) {
            vscode.window.showErrorMessage(`Error browsing Git lessons: ${error.message}`);
        }
    });

    context.subscriptions.push(browseLessonsGitCommand);

    const viewLearnProgressGitCommand = vscode.commands.registerCommand('tsiheader.viewLearnProgressGit', async () => {
        try {
            // Lazy load the Learn module to get progress tracker
            const Learn = require(path.join(__dirname, '..', '..', '..', 'learn', 'index.js'));
            const learnInstance = new Learn(context, vscode);

            // Lazy load the Git Book manager with progress tracker
            const GitBookManager = require(path.join(__dirname, '..', '..', '..', 'learn', 'git_manager.js'));
            const gitManager = new GitBookManager(vscode, learnInstance.progressTracker);

            // Get actual progress statistics
            const stats = await gitManager.getProgressStats();

            // Load curriculum to get total lesson count
            const fs = require('fs');
            const curriculumPath = path.join(__dirname, '..', '..', '..', 'learn', 'curriculum', 'git', 'curriculum.json');
            const curriculum = JSON.parse(fs.readFileSync(curriculumPath, 'utf8'));

            // Count total lessons in curriculum
            let totalLessons = 0;
            curriculum.chapters.forEach(chapter => {
                if (chapter.lessons) {
                    totalLessons += chapter.lessons.length;
                }
            });

            const completionRate = totalLessons > 0 ? Math.round((stats.lessonsCompleted / totalLessons) * 100) : 0;

            const message = `📊 Pro Git Book Progress\n\n` +
                `📚 Lessons Completed: ${stats.lessonsCompleted}/${totalLessons} (${completionRate}%)\n` +
                `🔥 Current Streak: ${stats.currentStreak} days\n` +
                `⏱️ Total Study Time: ${stats.totalStudyTime} minutes\n` +
                `🏆 Achievements: ${stats.achievements}\n` +
                `📅 Last Study Date: ${stats.lastStudyDate}\n\n` +
                `Keep up the great work mastering Git! 🚀`;

            const action = await vscode.window.showInformationMessage(
                message,
                { modal: true },
                'Continue Learning',
                'Browse Chapters',
                'Got it!'
            );

            if (action === 'Continue Learning') {
                // Start with Getting Started if no progress, otherwise continue from current progress
                if (stats.lessonsCompleted === 0) {
                    const gettingStartedChapter = curriculum.chapters.find(chapter => chapter.id === 'getting-started');
                    if (gettingStartedChapter && gettingStartedChapter.lessons && gettingStartedChapter.lessons.length > 0) {
                        const firstLesson = gettingStartedChapter.lessons[0];
                        await gitManager.openLesson(firstLesson, context);
                    }
                } else {
                    // For now, just open browse lessons - could be enhanced to find next lesson
                    await vscode.commands.executeCommand('tsiheader.browseLessonsGit');
                }
            } else if (action === 'Browse Chapters') {
                await vscode.commands.executeCommand('tsiheader.browseLessonsGit');
            }
        } catch (error) {
            vscode.window.showErrorMessage(`Error viewing Git progress: ${error.message}`);
        }
    });

    context.subscriptions.push(viewLearnProgressGitCommand);

    // Git Book Cache Management Commands
    const clearGitCacheCommand = vscode.commands.registerCommand('tsiheader.clearGitCache', async () => {
        try {
            // Lazy load the Git Book manager
            const GitBookManager = require(path.join(__dirname, '..', '..', '..', 'learn', 'git_manager.js'));
            const gitManager = new GitBookManager(vscode);

            const confirmed = await vscode.window.showWarningMessage(
                'Clear Git Book Cache?\n\n' +
                'This will delete all cached lessons. You will need to re-download lessons from git-scm.com next time you access them.\n\n' +
                'Cached lessons allow offline access when you don\'t have internet connection.',
                { modal: true },
                'Clear Cache',
                'Cancel'
            );

            if (confirmed === 'Clear Cache') {
                const success = await gitManager.clearLessonCache();
                if (success) {
                    vscode.window.showInformationMessage('✅ Git Book cache cleared successfully!');
                } else {
                    vscode.window.showErrorMessage('❌ Failed to clear Git Book cache');
                }
            }
        } catch (error) {
            vscode.window.showErrorMessage(`Error clearing cache: ${error.message}`);
        }
    });

    context.subscriptions.push(clearGitCacheCommand);

    const viewGitCacheStatsCommand = vscode.commands.registerCommand('tsiheader.viewGitCacheStats', async () => {
        try {
            // Lazy load the Git Book manager
            const GitBookManager = require(path.join(__dirname, '..', '..', '..', 'learn', 'git_manager.js'));
            const gitManager = new GitBookManager(vscode);

            const stats = await gitManager.getCacheStats();

            const formatSize = (bytes) => {
                if (bytes === 0) return '0 B';
                const k = 1024;
                const sizes = ['B', 'KB', 'MB', 'GB'];
                const i = Math.floor(Math.log(bytes) / Math.log(k));
                return parseFloat((bytes / Math.pow(k, i)).toFixed(2)) + ' ' + sizes[i];
            };

            const formatDate = (dateStr) => {
                if (!dateStr) return 'Never';
                return new Date(dateStr).toLocaleDateString();
            };

            const message = `📊 Git Book Cache Statistics\n\n` +
                `📚 Cached Lessons: ${stats.totalLessons}\n` +
                `💾 Cache Size: ${formatSize(stats.totalSize)}\n` +
                `📅 Oldest Cache: ${formatDate(stats.oldestCache)}\n` +
                `🆕 Newest Cache: ${formatDate(stats.newestCache)}\n\n` +
                `Cache helps provide offline access to lessons you've previously viewed.`;

            const action = await vscode.window.showInformationMessage(
                message,
                { modal: true },
                'Clear Cache',
                'Got it!'
            );

            if (action === 'Clear Cache') {
                await vscode.commands.executeCommand('tsiheader.clearGitCache');
            }
        } catch (error) {
            vscode.window.showErrorMessage(`Error viewing cache stats: ${error.message}`);
        }
    });

    context.subscriptions.push(viewGitCacheStatsCommand);

    // Odin Project Learning Commands
    const learnOdinCommand = vscode.commands.registerCommand('tsiheader.learnOdin', async () => {
        try {
            // Lazy load the Learn module to get progress tracker
            const Learn = require(path.join(__dirname, '..', '..', '..', 'learn', 'index.js'));
            const learnInstance = new Learn(context, vscode);
            
            // Lazy load the Odin Project manager with progress tracker
            const OdinProjectManager = require(path.join(__dirname, '..', '..', '..', 'top', 'odin_manager.js'));
            const odinManager = new OdinProjectManager(vscode, learnInstance.progressTracker);

            const message = '🚀 The Odin Project - Full Stack JavaScript\n\n' +
                'Embark on a comprehensive journey to become a full-stack developer:\n' +
                '• Foundations: HTML, CSS, JavaScript, Git\n' +
                '• Full Stack JavaScript: Node.js, Express, React, MongoDB\n' +
                '• Ruby on Rails: Complete web application development\n' +
                '• 300+ hours of interactive curriculum\n\n' +
                'Lessons are fetched live from The Odin Project website.\n\n' +
                'Ready to start your coding journey?';

            const action = await vscode.window.showInformationMessage(
                message,
                { modal: true },
                'Start Foundations',
                'Browse All Lessons',
                'View Progress'
            );

            if (action === 'Start Foundations') {
                // Load curriculum and start with first lesson in Foundations course
                const fs = require('fs');
                const curriculumPath = path.join(__dirname, '..', '..', '..', 'top', 'curriculum.json');
                const curriculum = JSON.parse(fs.readFileSync(curriculumPath, 'utf8'));
                const foundationsPath = curriculum.paths.find(path => path.id === 'foundations');
                if (foundationsPath && foundationsPath.courses.length > 0) {
                    const foundationsCourse = foundationsPath.courses[0];
                    if (foundationsCourse.lessons && foundationsCourse.lessons.length > 0) {
                        const firstLesson = foundationsCourse.lessons[0];
                        await odinManager.openLesson(firstLesson, context);
                    } else {
                        vscode.window.showErrorMessage('No lessons found in Foundations course');
                    }
                } else {
                    vscode.window.showErrorMessage('Foundations path not found in curriculum');
                }
            } else if (action === 'Browse All Lessons') {
                await vscode.commands.executeCommand('tsiheader.browseLessonsOdin');
            } else if (action === 'View Progress') {
                await vscode.commands.executeCommand('tsiheader.viewLearnProgressOdin');
            }
        } catch (error) {
            vscode.window.showErrorMessage(`Error starting Odin Project: ${error.message}`);
        }
    });

    context.subscriptions.push(learnOdinCommand);

    const browseLessonsOdinCommand = vscode.commands.registerCommand('tsiheader.browseLessonsOdin', async () => {
        try {
            // Lazy load the Learn module to get progress tracker
            const Learn = require(path.join(__dirname, '..', '..', '..', 'learn', 'index.js'));
            const learnInstance = new Learn(context, vscode);
            
            // Lazy load the Odin Project manager with progress tracker
            const OdinProjectManager = require(path.join(__dirname, '..', '..', '..', 'top', 'odin_manager.js'));
            const odinManager = new OdinProjectManager(vscode, learnInstance.progressTracker);
            
            const fs = require('fs');
            const curriculumPath = path.join(__dirname, '..', '..', '..', 'top', 'curriculum.json');
            const curriculum = JSON.parse(fs.readFileSync(curriculumPath, 'utf8'));

            // Get progress to show completion status
            const progress = await odinManager.getProgressStats();
            const completedLessons = new Set(progress.completed || []);

            // Create quick pick items for all lessons
            const items = [];
            curriculum.paths.forEach(path => {
                // Add path separator
                items.push({
                    label: `📚 ${path.title}`,
                    kind: vscode.QuickPickItemKind.Separator
                });

                path.courses.forEach(course => {
                    // Add course as separator
                    items.push({
                        label: `  📖 ${course.title}`,
                        kind: vscode.QuickPickItemKind.Separator
                    });

                    // Add lessons within the course
                    if (course.lessons && course.lessons.length > 0) {
                        course.lessons.forEach(lesson => {
                            const isCompleted = completedLessons.has(lesson.id);
                            const icon = isCompleted ? '✅' : '○';
                            items.push({
                                label: `    ${icon} ${lesson.title}`,
                                description: `${lesson.estimatedHours}h`,
                                detail: isCompleted ? 'Completed' : 'Not started',
                                lesson: lesson
                            });
                        });
                    }
                });
            });

            const selected = await vscode.window.showQuickPick(items, {
                placeHolder: 'Select a lesson from The Odin Project',
                matchOnDescription: true
            });

            if (selected && selected.lesson) {
                await odinManager.openLesson(selected.lesson, context);
            }
        } catch (error) {
            vscode.window.showErrorMessage(`Error browsing Odin lessons: ${error.message}`);
        }
    });

    context.subscriptions.push(browseLessonsOdinCommand);

    const viewLearnProgressOdinCommand = vscode.commands.registerCommand('tsiheader.viewLearnProgressOdin', async () => {
        try {
            // Lazy load the Learn module to get progress tracker
            const Learn = require(path.join(__dirname, '..', '..', '..', 'learn', 'index.js'));
            const learnInstance = new Learn(context, vscode);
            
            // Lazy load the Odin Project manager with progress tracker
            const OdinProjectManager = require(path.join(__dirname, '..', '..', '..', 'top', 'odin_manager.js'));
            const odinManager = new OdinProjectManager(vscode, learnInstance.progressTracker);
            
            // Get actual progress statistics
            const stats = await odinManager.getProgressStats();
            
            // Load curriculum to get total lesson count
            const fs = require('fs');
            const curriculumPath = path.join(__dirname, '..', '..', '..', 'top', 'curriculum.json');
            const curriculum = JSON.parse(fs.readFileSync(curriculumPath, 'utf8'));
            
            // Count total lessons in curriculum
            let totalLessons = 0;
            curriculum.paths.forEach(path => {
                path.courses.forEach(course => {
                    if (course.lessons) {
                        totalLessons += course.lessons.length;
                    }
                });
            });
            
            const completionRate = totalLessons > 0 ? Math.round((stats.lessonsCompleted / totalLessons) * 100) : 0;
            
            const message = `📊 The Odin Project Progress\n\n` +
                `📚 Lessons Completed: ${stats.lessonsCompleted}/${totalLessons} (${completionRate}%)\n` +
                `🔥 Current Streak: ${stats.currentStreak} days\n` +
                `⏱️ Total Study Time: ${stats.totalStudyTime} minutes\n` +
                `🏆 Achievements: ${stats.achievements}\n` +
                `📅 Last Study Date: ${stats.lastStudyDate}\n\n` +
                `Keep up the great work on your full-stack development journey! 🚀`;

            const action = await vscode.window.showInformationMessage(
                message,
                { modal: true },
                'Continue Learning',
                'Browse Lessons',
                'Got it!'
            );

            if (action === 'Continue Learning') {
                // Start with foundations if no progress, otherwise continue from current progress
                if (stats.lessonsCompleted === 0) {
                    const foundationsPath = curriculum.paths.find(path => path.id === 'foundations');
                    if (foundationsPath && foundationsPath.courses.length > 0) {
                        const foundationsCourse = foundationsPath.courses[0];
                        if (foundationsCourse.lessons && foundationsCourse.lessons.length > 0) {
                            const firstLesson = foundationsCourse.lessons[0];
                            await odinManager.openLesson(firstLesson, context);
                        }
                    }
                } else {
                    // For now, just open browse lessons - could be enhanced to find next lesson
                    await vscode.commands.executeCommand('tsiheader.browseLessonsOdin');
                }
            } else if (action === 'Browse Lessons') {
                await vscode.commands.executeCommand('tsiheader.browseLessonsOdin');
            }
        } catch (error) {
            vscode.window.showErrorMessage(`Error viewing Odin progress: ${error.message}`);
        }
    });

    context.subscriptions.push(viewLearnProgressOdinCommand);

    // Odin Project Cache Management Commands
    const clearOdinCacheCommand = vscode.commands.registerCommand('tsiheader.clearOdinCache', async () => {
        try {
            // Lazy load the Odin Project manager
            const OdinProjectManager = require(path.join(__dirname, '..', '..', '..', 'top', 'odin_manager.js'));
            const odinManager = new OdinProjectManager(vscode);

            const confirmed = await vscode.window.showWarningMessage(
                'Clear Odin Project Cache?\n\n' +
                'This will delete all cached lessons. You will need to re-download lessons from The Odin Project website next time you access them.\n\n' +
                'Cached lessons allow offline access when you don\'t have internet connection.',
                { modal: true },
                'Clear Cache',
                'Cancel'
            );

            if (confirmed === 'Clear Cache') {
                const success = await odinManager.clearLessonCache();
                if (success) {
                    vscode.window.showInformationMessage('✅ Odin Project cache cleared successfully!');
                } else {
                    vscode.window.showErrorMessage('❌ Failed to clear Odin Project cache');
                }
            }
        } catch (error) {
            vscode.window.showErrorMessage(`Error clearing cache: ${error.message}`);
        }
    });

    context.subscriptions.push(clearOdinCacheCommand);

    const viewOdinCacheStatsCommand = vscode.commands.registerCommand('tsiheader.viewOdinCacheStats', async () => {
        try {
            // Lazy load the Odin Project manager
            const OdinProjectManager = require(path.join(__dirname, '..', '..', '..', 'top', 'odin_manager.js'));
            const odinManager = new OdinProjectManager(vscode);

            const stats = await odinManager.getCacheStats();

            const formatSize = (bytes) => {
                if (bytes === 0) return '0 B';
                const k = 1024;
                const sizes = ['B', 'KB', 'MB', 'GB'];
                const i = Math.floor(Math.log(bytes) / Math.log(k));
                return parseFloat((bytes / Math.pow(k, i)).toFixed(2)) + ' ' + sizes[i];
            };

            const formatDate = (dateStr) => {
                if (!dateStr) return 'Never';
                return new Date(dateStr).toLocaleDateString();
            };

            const message = `📊 Odin Project Cache Statistics\n\n` +
                `📚 Cached Lessons: ${stats.totalLessons}\n` +
                `💾 Cache Size: ${formatSize(stats.totalSize)}\n` +
                `📅 Oldest Cache: ${formatDate(stats.oldestCache)}\n` +
                `🆕 Newest Cache: ${formatDate(stats.newestCache)}\n\n` +
                `Cache helps provide offline access to lessons you've previously viewed.`;

            const action = await vscode.window.showInformationMessage(
                message,
                { modal: true },
                'Clear Cache',
                'Got it!'
            );

            if (action === 'Clear Cache') {
                await vscode.commands.executeCommand('tsiheader.clearOdinCache');
            }
        } catch (error) {
            vscode.window.showErrorMessage(`Error viewing cache stats: ${error.message}`);
        }
    });

    context.subscriptions.push(viewOdinCacheStatsCommand);

    // Mathematics Learning Commands
    const learnMathematicsCommand = vscode.commands.registerCommand('tsiheader.learnMathematics', async () => {
        try {
            // Lazy load the Mathematics manager
            const MathematicsManager = require(path.join(__dirname, '..', '..', '..', 'learn', 'lib', 'mathematics_manager.js'));
            const mathManager = new MathematicsManager(context, vscode);

            const workbooks = await mathManager.getWorkbooks();
            
            vscode.window.showInformationMessage(
                '🔢 Start Higher Mathematics Learning?\n\n' +
                'Explore advanced mathematics with HELM workbooks:\n' +
                '• Matrices fundamentals and operations\n' +
                '• Matrix solution of equations\n' +
                '• Interactive exercises and quizzes\n' +
                '• PDF workbooks with detailed explanations\n\n' +
                'Ready to begin your mathematics journey?',
                { modal: true },
                'Browse Workbooks',
                'Cancel'
            ).then(async selection => {
                if (selection === 'Browse Workbooks') {
                    // Show available workbooks
                    const workbookItems = workbooks.map(workbook => ({
                        label: `📚 ${workbook.title}`,
                        description: 'Open PDF workbook',
                        workbook: workbook
                    }));
                    
                    const selected = await vscode.window.showQuickPick(workbookItems, {
                        placeHolder: 'Select a mathematics workbook'
                    });
                    
                    if (selected) {
                        await mathManager.openWorkbook(selected.workbook);
                    }
                }
            });
        } catch (error) {
            vscode.window.showErrorMessage(`Error starting mathematics learning: ${error.message}`);
        }
    });

    context.subscriptions.push(learnMathematicsCommand);

    const browseMathematicsWorkbooksCommand = vscode.commands.registerCommand('tsiheader.browseMathematicsWorkbooks', async () => {
        try {
            const MathematicsManager = require(path.join(__dirname, '..', '..', '..', 'learn', 'lib', 'mathematics_manager.js'));
            const mathManager = new MathematicsManager(context, vscode);
            
            const workbooks = await mathManager.getWorkbooks();
            const workbookItems = workbooks.map(workbook => ({
                label: `📚 ${workbook.title}`,
                description: 'Open PDF workbook',
                workbook: workbook
            }));
            
            const selected = await vscode.window.showQuickPick(workbookItems, {
                placeHolder: 'Select a mathematics workbook to open'
            });
            
            if (selected) {
                await mathManager.openWorkbook(selected.workbook);
            }
        } catch (error) {
            vscode.window.showErrorMessage(`Error browsing mathematics workbooks: ${error.message}`);
        }
    });

    context.subscriptions.push(browseMathematicsWorkbooksCommand);

    const takeMathematicsQuizCommand = vscode.commands.registerCommand('tsiheader.takeMathematicsQuiz', async () => {
        try {
            const MathematicsManager = require(path.join(__dirname, '..', '..', '..', 'learn', 'lib', 'mathematics_manager.js'));
            const mathManager = new MathematicsManager(context, vscode);
            
            const quizzes = await mathManager.getQuizzes();
            const quizItems = quizzes.map(quiz => ({
                label: `🧠 ${quiz.title}`,
                description: 'Test your knowledge',
                quiz: quiz
            }));
            
            const selected = await vscode.window.showQuickPick(quizItems, {
                placeHolder: 'Select a mathematics quiz'
            });
            
            if (selected) {
                // Load and display the quiz in webview
                const quizData = await mathManager.loadQuiz(selected.quiz.id);
                await mathManager.openQuiz(quizData);
            }
        } catch (error) {
            vscode.window.showErrorMessage(`Error loading mathematics quiz: ${error.message}`);
        }
    });

    context.subscriptions.push(takeMathematicsQuizCommand);

    // Git quizzes - list and open interactive quizzes defined under learn/curriculum/git/exercises
    const takeGitQuizCommand = vscode.commands.registerCommand('tsiheader.takeGitQuiz', async () => {
        try {
            const fs = require('fs');
            const path = require('path');

            const quizzesDir = path.join(__dirname, '..', '..', '..', 'learn', 'curriculum', 'git', 'exercises');

            // Read available quiz files
            let files = [];
            try {
                files = fs.readdirSync(quizzesDir).filter(f => f.endsWith('_quiz.json'));
            } catch (e) {
                // Directory missing or unreadable
                files = [];
            }

            if (files.length === 0) {
                vscode.window.showInformationMessage('No Git quizzes are available right now.');
                return;
            }

            const quizItems = files.map(filename => {
                const id = filename.replace('_quiz.json', '');
                const title = id.replace(/_/g, ' ').replace(/\b\w/g, l => l.toUpperCase());
                return {
                    label: `🧠 ${title}`,
                    description: 'Test your Git knowledge',
                    filename,
                    id
                };
            });

            const selected = await vscode.window.showQuickPick(quizItems, { placeHolder: 'Select a Git quiz' });

            if (!selected) return;

            // Load the selected quiz JSON
            const quizPath = path.join(quizzesDir, selected.filename);
            const raw = fs.readFileSync(quizPath, 'utf8');
            const quizData = JSON.parse(raw);

            // Reuse MathematicsManager's openQuiz UI since it provides a solid quiz viewer
            const MathematicsManager = require(path.join(__dirname, '..', '..', '..', 'learn', 'lib', 'mathematics_manager.js'));
            const mathManager = new MathematicsManager(context, vscode);

            // Open the quiz using the quiz viewer
            await mathManager.openQuiz(quizData);

        } catch (error) {
            vscode.window.showErrorMessage(`Error loading Git quiz: ${error.message}`);
        }
    });

    context.subscriptions.push(takeGitQuizCommand);

    const viewMathematicsCacheStatsCommand = vscode.commands.registerCommand('tsiheader.viewMathematicsCacheStats', async () => {
        try {
            const MathematicsManager = require(path.join(__dirname, '..', '..', '..', 'learn', 'lib', 'mathematics_manager.js'));
            const mathManager = new MathematicsManager(context, vscode);
            
            const stats = await mathManager.getCacheStatistics();
            
            const message = `📊 Mathematics Cache Statistics\n\n` +
                `Workbooks Cached: ${stats.workbooks}\n` +
                `Cache Size: ${stats.size}\n` +
                `Oldest Cache: ${stats.oldest || 'None'}\n` +
                `Newest Cache: ${stats.newest || 'None'}\n\n` +
                `Cache helps reduce download times and enables offline access.`;
            
            const action = await vscode.window.showInformationMessage(
                message,
                { modal: true },
                'Clear Cache',
                'Close'
            );
            
            if (action === 'Clear Cache') {
                const confirm = await vscode.window.showWarningMessage(
                    'Clear all cached mathematics workbooks?',
                    { modal: true },
                    'Yes, Clear Cache',
                    'Cancel'
                );
                
                if (confirm === 'Yes, Clear Cache') {
                    const cleared = await mathManager.clearCache();
                    if (cleared) {
                        vscode.window.showInformationMessage('Mathematics cache cleared successfully!');
                    } else {
                        vscode.window.showErrorMessage('Failed to clear mathematics cache.');
                    }
                }
            }
        } catch (error) {
            vscode.window.showErrorMessage(`Error viewing cache stats: ${error.message}`);
        }
    });

    context.subscriptions.push(viewMathematicsCacheStatsCommand);

    const clearMathematicsCacheCommand = vscode.commands.registerCommand('tsiheader.clearMathematicsCache', async () => {
        try {
            const MathematicsManager = require(path.join(__dirname, '..', '..', '..', 'learn', 'lib', 'mathematics_manager.js'));
            const mathManager = new MathematicsManager(context, vscode);
            
            const confirm = await vscode.window.showWarningMessage(
                'Clear all cached mathematics workbooks?\n\nThis will remove all downloaded PDFs and require re-downloading them.',
                { modal: true },
                'Yes, Clear Cache',
                'Cancel'
            );
            
            if (confirm === 'Yes, Clear Cache') {
                const cleared = await mathManager.clearCache();
                if (cleared) {
                    vscode.window.showInformationMessage('Mathematics cache cleared successfully!');
                } else {
                    vscode.window.showErrorMessage('Failed to clear mathematics cache.');
                }
            }
        } catch (error) {
            vscode.window.showErrorMessage(`Error clearing cache: ${error.message}`);
        }
    });

    context.subscriptions.push(clearMathematicsCacheCommand);
}

module.exports = { register };
