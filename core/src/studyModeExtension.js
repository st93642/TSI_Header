/**
 * Study Mode Extension Integration
 * Integrates the Pomodoro timer with VS Code extension
 */

const { StudyModeTimer } = require('../../studyMode/timer');

class StudyModeExtension {
    constructor(vscode, context) {
        this.vscode = vscode;
        this.context = context;
        this.timer = null;
        this.isEnabled = false;
    }

    activate() {
        try {
            // Read configuration
            const config = this.vscode.workspace.getConfiguration('tsiheader.studyMode');
            const timerConfig = {
                workDuration: config.get('workDuration', 25),
                shortBreakDuration: config.get('shortBreakDuration', 5),
                longBreakDuration: config.get('longBreakDuration', 15),
                sessionsBeforeLongBreak: config.get('sessionsBeforeLongBreak', 4)
            };

            // Create timer instance with configuration
            this.timer = new StudyModeTimer(this.vscode, this.context, timerConfig, () => {
                this.savePersistedState();
            });

            // Load persisted state
            this.loadPersistedState();

            // Register commands
            this.registerCommands();

            // Register status bar item click handler
            this.setupStatusBarInteraction();

            // Register state persistence on deactivation
            this.setupStatePersistence();

            console.log('Study Mode extension activated');
        } catch (error) {
            console.warn('Study Mode extension failed to activate:', error.message);
            // Continue without Study Mode if VS Code APIs are not available (e.g., in tests)
        }
    }

    registerCommands() {
        if (!this.timer) return;

        // Start Study Session
        const startCommand = this.vscode.commands.registerCommand('tsiheader.startStudySession', () => {
            this.startStudySession();
        });

        // Pause/Resume Timer
        const pauseCommand = this.vscode.commands.registerCommand('tsiheader.pauseStudyTimer', () => {
            this.pauseResumeTimer();
        });

        // Stop Study Session
        const stopCommand = this.vscode.commands.registerCommand('tsiheader.stopStudySession', () => {
            this.stopStudySession();
        });

        // View Study Statistics
        const statsCommand = this.vscode.commands.registerCommand('tsiheader.viewStudyStats', () => {
            this.showStudyStats();
        });

        // Configure Study Mode
        const configCommand = this.vscode.commands.registerCommand('tsiheader.configureStudyMode', () => {
            this.openConfiguration();
        });

        // Reset Study Progress
        const resetCommand = this.vscode.commands.registerCommand('tsiheader.resetStudyProgress', () => {
            this.resetProgress();
        });

        // Add to subscriptions
        this.context.subscriptions.push(startCommand, pauseCommand, stopCommand, statsCommand, configCommand, resetCommand);
    }

    setupStatusBarInteraction() {
        // Handle status bar item clicks
        if (this.timer && this.timer.statusBarItem) {
            this.timer.statusBarItem.command = 'tsiheader.pauseStudyTimer';
        }
    }

    startStudySession() {
        if (!this.timer) return;

        if (this.timer.currentPhase === 'stopped') {
            // Read fresh configuration from settings
            const config = this.vscode.workspace.getConfiguration('tsiheader.studyMode');
            const timerConfig = {
                workDuration: config.get('workDuration', 25),
                shortBreakDuration: config.get('shortBreakDuration', 5),
                longBreakDuration: config.get('longBreakDuration', 15),
                sessionsBeforeLongBreak: config.get('sessionsBeforeLongBreak', 4)
            };

            // Update timer with fresh configuration
            this.timer.updateConfiguration(timerConfig);

            const workDuration = timerConfig.workDuration;
            const breakDuration = timerConfig.shortBreakDuration;

            this.vscode.window.showInformationMessage(
                `🍅 Study Session Started!\n\nFocus for ${workDuration} minutes.\nGood luck with your work!`,
                { modal: true },
                'Got it!'
            ).then(() => {
                this.timer.start();
            });
        } else {
            this.vscode.window.showWarningMessage(
                `Study Session Already Running\n\nA study session is already active (${this.timer.currentPhase}).\nUse "Pause/Resume Timer" to control it.`,
                { modal: true },
                'Got it!'
            );
        }
    }

    pauseResumeTimer() {
        if (!this.timer) return;

        if (this.timer.currentPhase === 'stopped') {
            // If stopped, start a new session
            this.startStudySession();
        } else {
            const remainingTime = this.timer.formatTime(this.timer.isRunning ? this.timer.getRemainingTime() : this.timer.remainingTime);
            const phaseName = this.timer.currentPhase === 'work' ? 'Work Session' :
                             this.timer.currentPhase === 'shortBreak' ? 'Short Break' : 'Long Break';
            const isRunning = this.timer.isRunning;

            if (isRunning) {
                // Currently running - show dialog first, then pause after confirmation
                this.vscode.window.showInformationMessage(
                    `⏸️ Pause Timer?\n\n${phaseName} will be paused at ${remainingTime}.\nYou can resume it anytime from the status bar.`,
                    { modal: true },
                    'Pause',
                    'Cancel'
                ).then(selection => {
                    if (selection === 'Pause') {
                        this.timer.pause();
                    }
                });
            } else {
                // Currently paused - show dialog first, then resume after confirmation
                this.vscode.window.showInformationMessage(
                    `▶️ Resume Timer?\n\n${phaseName} will resume.\n${remainingTime} remaining.`,
                    { modal: true },
                    'Resume',
                    'Cancel'
                ).then(selection => {
                    if (selection === 'Resume') {
                        this.timer.resume();
                    }
                });
            }
        }
    }

    stopStudySession() {
        if (!this.timer) return;

        if (this.timer.currentPhase !== 'stopped') {
            this.vscode.window.showWarningMessage(
                `🛑 Stop Study Session?\n\nYour study session will be stopped and reset.\nAll progress for this session will be lost.`,
                { modal: true },
                'Stop Session',
                'Cancel'
            ).then(selection => {
                if (selection === 'Stop Session') {
                    this.timer.stop();
                }
            });
        } else {
            this.vscode.window.showInformationMessage(
                `No Active Session\n\nThere is no active study session to stop.\nStart a new session to begin studying.`,
                { modal: true },
                'Got it!'
            );
        }
    }

    showStudyStats() {
        if (!this.timer) {
            this.vscode.window.showErrorMessage('Study Mode is not available. Please restart VS Code.');
            return;
        }

        const config = this.vscode.workspace.getConfiguration('tsiheader.studyMode');
        const analytics = this.getProductivityAnalytics();

        const currentStatus = this.timer.currentPhase === 'stopped' ? 'Not Active' :
                             `${this.timer.currentPhase} (${this.timer.isRunning ? 'Running' : 'Paused'})`;

        const statsMessage = `📊 Study Mode Statistics

Current Status: ${currentStatus}
Session Progress: ${this.timer.currentSession}/${config.get('sessionsBeforeLongBreak', 4)}

📈 Today's Progress:
• Sessions Completed: ${analytics.today.completedSessions}
• Focus Time: ${this.formatDuration(analytics.today.totalFocusTime)}
• Completion Rate: ${Math.round(analytics.today.completionRate)}%

📅 This Week:
• Sessions Completed: ${analytics.week.completedSessions}
• Focus Time: ${this.formatDuration(analytics.week.totalFocusTime)}
• Completion Rate: ${Math.round(analytics.week.completionRate)}%

📆 This Month:
• Sessions Completed: ${analytics.month.completedSessions}
• Focus Time: ${this.formatDuration(analytics.month.totalFocusTime)}
• Completion Rate: ${Math.round(analytics.month.completionRate)}%

🏆 All-Time Records:
• Total Sessions: ${analytics.allTime.totalSessions}
• Total Focus Time: ${this.formatDuration(analytics.allTime.totalFocusTime)}
• Average Session: ${this.formatDuration(analytics.allTime.avgSessionTime)}
• Best Completion Rate: ${Math.round(analytics.allTime.completionRate)}%

⚙️ Current Configuration:
• Work Duration: ${config.get('workDuration', 25)} minutes
• Short Break: ${config.get('shortBreakDuration', 5)} minutes
• Long Break: ${config.get('longBreakDuration', 15)} minutes
• Sessions Before Long Break: ${config.get('sessionsBeforeLongBreak', 4)}`;

        this.vscode.window.showInformationMessage(
            statsMessage,
            { modal: true },
            'Reset Statistics',
            'Got it!'
        ).then(selection => {
            if (selection === 'Reset Statistics') {
                this.resetProgress();
            }
        });
    }

    openConfiguration() {
        this.vscode.commands.executeCommand('workbench.action.openSettings', '@ext:st93642.uni-header tsiheader.studyMode');
    }

    setupStatePersistence() {
        // Save state when configuration changes
        const onConfigChange = this.vscode.workspace.onDidChangeConfiguration(() => {
            this.savePersistedState();
        });

        // Save state when window loses focus (VS Code might close) - if API is available
        let onWindowFocus;
        if (this.vscode.window.onDidChangeWindowState) {
            onWindowFocus = this.vscode.window.onDidChangeWindowState((state) => {
                if (!state.focused) {
                    this.savePersistedState();
                }
            });
        }

        // Save state periodically (every 30 seconds) for running timers - only in production
        let saveInterval;
        if (typeof process !== 'undefined' && process.env && !process.env.NODE_TEST_CONTEXT) {
            saveInterval = setInterval(() => {
                if (this.timer && (this.timer.isRunning || this.timer.currentPhase !== 'stopped')) {
                    this.savePersistedState();
                }
            }, 30000);
        }

        // Clear interval on dispose
        const subscriptions = [onConfigChange];
        if (onWindowFocus) {
            subscriptions.push(onWindowFocus);
        }
        if (saveInterval) {
            subscriptions.push({ dispose: () => clearInterval(saveInterval) });
        }

        this.context.subscriptions.push(...subscriptions);
    }

    loadPersistedState() {
        try {
            const persistedState = this.context.globalState.get('studyMode.state');
            if (persistedState && this.timer) {
                // Restore timer state
                this.timer.currentPhase = persistedState.currentPhase || 'stopped';
                this.timer.currentSession = persistedState.currentSession || 0;
                this.timer.isRunning = false; // Always start as paused after restart
                
                // Restore elapsed time and calculate remaining time
                this.timer.elapsedTime = persistedState.elapsedTime || 0;
                
                if (this.timer.currentPhase !== 'stopped') {
                    const currentDuration = this.timer.getCurrentDuration();
                    this.timer.remainingTime = Math.max(0, currentDuration - this.timer.elapsedTime);
                } else {
                    this.timer.remainingTime = 0;
                }
                
                // Restore phase start timestamp for logging
                if (persistedState.phaseStartTimestamp) {
                    if (typeof persistedState.phaseStartTimestamp === 'string') {
                        this.timer.phaseStartTimestamp = new Date(persistedState.phaseStartTimestamp).getTime();
                    } else {
                        this.timer.phaseStartTimestamp = persistedState.phaseStartTimestamp;
                    }
                } else {
                    this.timer.phaseStartTimestamp = null;
                }
                
                // Restore session log
                this.timer.sessionLog = persistedState.sessionLog || [];
                
                // Reset tick time (will be set when resumed)
                this.timer.lastTickTime = null;

                // Update status bar to reflect restored state
                this.timer.updateStatusBar();

                console.log('Study Mode state restored from persistence');
            }
        } catch (error) {
            console.warn('Failed to load persisted Study Mode state:', error.message);
        }
    }

    savePersistedState() {
        try {
            if (this.timer) {
                // Update elapsed time if running before saving
                if (this.timer.isRunning) {
                    this.timer.updateElapsedTime();
                }
                
                const stateToSave = {
                    currentPhase: this.timer.currentPhase,
                    currentSession: this.timer.currentSession,
                    isRunning: this.timer.isRunning,
                    elapsedTime: this.timer.elapsedTime,
                    phaseStartTimestamp: this.timer.phaseStartTimestamp ? new Date(this.timer.phaseStartTimestamp).toISOString() : null,
                    sessionLog: this.timer.sessionLog,
                    lastSaved: new Date().toISOString()
                };

                this.context.globalState.update('studyMode.state', stateToSave);
            }
        } catch (error) {
            console.warn('Failed to save Study Mode state:', error.message);
        }
    }

    getProductivityAnalytics() {
        const sessions = this.timer.sessionLog;
        const now = new Date();
        const today = new Date(now.getFullYear(), now.getMonth(), now.getDate());
        const thisWeek = new Date(today);
        thisWeek.setDate(today.getDate() - today.getDay());
        const thisMonth = new Date(now.getFullYear(), now.getMonth(), 1);

        // Filter sessions by time periods
        const todaySessions = sessions.filter(s => new Date(s.startTime) >= today);
        const weekSessions = sessions.filter(s => new Date(s.startTime) >= thisWeek);
        const monthSessions = sessions.filter(s => new Date(s.startTime) >= thisMonth);

        // Calculate metrics for each period
        const calculateMetrics = (sessionList) => {
            const completed = sessionList.filter(s => s.completed);
            const workSessions = sessionList.filter(s => s.type === 'work');
            const totalFocusTime = workSessions.reduce((total, s) => total + s.duration, 0);

            return {
                totalSessions: sessionList.length,
                completedSessions: completed.length,
                workSessions: workSessions.length,
                totalFocusTime,
                completionRate: sessionList.length > 0 ? (completed.length / sessionList.length) * 100 : 0,
                avgSessionTime: workSessions.length > 0 ? totalFocusTime / workSessions.length : 0
            };
        };

        return {
            today: calculateMetrics(todaySessions),
            week: calculateMetrics(weekSessions),
            month: calculateMetrics(monthSessions),
            allTime: calculateMetrics(sessions)
        };
    }

    resetProgress() {
        this.vscode.window.showWarningMessage(
            'Reset Study Mode Progress',
            { modal: true },
            'Cancel',
            'Reset Everything',
            'Reset Today Only'
        ).then(selection => {
            if (selection === 'Reset Everything') {
                this.vscode.window.showWarningMessage(
                    '⚠️ This will permanently delete ALL your study progress, session logs, and statistics. This action cannot be undone.',
                    { modal: true },
                    'Cancel',
                    'Yes, Reset Everything'
                ).then(confirm => {
                    if (confirm === 'Yes, Reset Everything') {
                        this.performFullReset();
                    }
                });
            } else if (selection === 'Reset Today Only') {
                this.resetTodayProgress();
            }
        });
    }

    performFullReset() {
        if (this.timer) {
            // Stop any running timer
            this.timer.stop();

            // Clear all session data
            this.timer.sessionLog = [];
            this.timer.currentSession = 0;
            this.timer.currentPhase = 'stopped';
            this.timer.isRunning = false;
            this.timer.elapsedTime = 0;
            this.timer.lastTickTime = null;
            this.timer.remainingTime = 0;
            this.timer.phaseStartTimestamp = null;

            // Clear persisted state
            this.context.globalState.update('studyMode.state', undefined);

            // Update status bar
            this.timer.updateStatusBar();

            this.vscode.window.showInformationMessage(
                '🔄 Study Mode Progress Reset\n\nAll study progress, session logs, and statistics have been cleared.',
                { modal: true },
                'Got it!'
            );
        }
    }

    resetTodayProgress() {
        if (this.timer) {
            const now = new Date();
            const today = new Date(now.getFullYear(), now.getMonth(), now.getDate());

            // Filter out today's sessions
            this.timer.sessionLog = this.timer.sessionLog.filter(s =>
                new Date(s.startTime) < today
            );

            // Reset current session if it's today
            if (this.timer.currentPhase !== 'stopped') {
                this.timer.stop();
            }

            // Save updated state
            this.savePersistedState();

            this.vscode.window.showInformationMessage(
                '🔄 Today\'s Progress Reset\n\nToday\'s study sessions have been cleared. Previous progress remains intact.',
                { modal: true },
                'Got it!'
            );
        }
    }

    formatDuration(milliseconds) {
        if (milliseconds <= 0) return '0m';

        const totalMinutes = milliseconds / (1000 * 60);
        if (totalMinutes < 1) return '0m';

        const roundedMinutes = Math.ceil(totalMinutes);
        const hours = Math.floor(roundedMinutes / 60);
        const minutes = roundedMinutes % 60;

        if (hours > 0) {
            return `${hours}h ${minutes}m`;
        } else {
            return `${roundedMinutes}m`;
        }
    }

    dispose() {
        // Save state before disposing
        this.savePersistedState();

        // Dispose of all subscriptions
        if (this.context && this.context.subscriptions) {
            this.context.subscriptions.forEach(subscription => {
                if (subscription && typeof subscription.dispose === 'function') {
                    subscription.dispose();
                }
            });
            this.context.subscriptions = [];
        }

        if (this.timer) {
            this.timer.dispose();
        }
    }
}

module.exports = { StudyModeExtension };