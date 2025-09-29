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
            // Create timer instance
            this.timer = new StudyModeTimer(this.vscode, this.context);

            // Register commands
            this.registerCommands();

            // Register status bar item click handler
            this.setupStatusBarInteraction();

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

        // Add to subscriptions
        this.context.subscriptions.push(startCommand, pauseCommand, stopCommand, statsCommand, configCommand);
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
            const workDuration = Math.floor(this.timer.workDuration / (1000 * 60));
            const breakDuration = Math.floor(this.timer.shortBreakDuration / (1000 * 60));
            const longBreakDuration = Math.floor(this.timer.longBreakDuration / (1000 * 60));

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
            const remainingTime = this.timer.formatTime(this.timer.getRemainingTime());
            const phaseName = this.timer.currentPhase === 'work' ? 'Work Session' :
                             this.timer.currentPhase === 'shortBreak' ? 'Short Break' : 'Long Break';
            const isRunning = this.timer.isRunning;

            if (isRunning) {
                // Currently running - pause
                this.timer.pause();
                this.vscode.window.showInformationMessage(
                    `⏸️ Timer Paused\n\n${phaseName} paused at ${remainingTime}.\nClick the status bar timer to resume.`,
                    { modal: true },
                    'Got it!'
                );
            } else {
                // Currently paused - resume
                this.timer.resume();
                this.vscode.window.showInformationMessage(
                    `▶️ Timer Resumed\n\n${phaseName} resumed.\n${remainingTime} remaining.`,
                    { modal: true },
                    'Got it!'
                );
            }
        }
    }

    stopStudySession() {
        if (!this.timer) return;

        if (this.timer.currentPhase !== 'stopped') {
            this.timer.stop();
            this.vscode.window.showInformationMessage(
                `🛑 Study Session Stopped\n\nYour study session has been stopped and reset.\nAll progress for this session has been lost.`,
                { modal: true },
                'Got it!'
            );
        } else {
            this.vscode.window.showInformationMessage(
                `No Active Session\n\nThere is no active study session to stop.\nStart a new session to begin studying.`,
                { modal: true },
                'Got it!'
            );
        }
    }

    showStudyStats() {
        if (!this.timer) return;

        const sessions = this.timer.sessionLog;
        const totalSessions = sessions.length;
        const completedSessions = sessions.filter(s => s.completed).length;
        const totalFocusTime = sessions.reduce((total, s) => total + s.duration, 0);
        const avgSessionTime = totalSessions > 0 ? totalFocusTime / totalSessions : 0;

        const currentStatus = this.timer.currentPhase === 'stopped' ? 'Not Active' :
                             `${this.timer.currentPhase} (${this.timer.isRunning ? 'Running' : 'Paused'})`;

        const statsMessage = `📊 Study Mode Statistics

Current Status: ${currentStatus}
Session Progress: ${this.timer.currentSession}/${this.timer.sessionsBeforeLongBreak}

📈 Session Summary:
• Total Sessions Started: ${totalSessions}
• Completed Sessions: ${completedSessions}
• Completion Rate: ${totalSessions > 0 ? Math.round((completedSessions / totalSessions) * 100) : 0}%

⏱️ Time Tracking:
• Total Focus Time: ${this.formatDuration(totalFocusTime)}
• Average Session Time: ${this.formatDuration(avgSessionTime)}

⚙️ Timer Configuration:
• Work Duration: ${Math.floor(this.timer.workDuration / (1000 * 60))} minutes
• Short Break: ${Math.floor(this.timer.shortBreakDuration / (1000 * 60))} minutes
• Long Break: ${Math.floor(this.timer.longBreakDuration / (1000 * 60))} minutes
• Sessions Before Long Break: ${this.timer.sessionsBeforeLongBreak}`;

        this.vscode.window.showInformationMessage(
            statsMessage,
            { modal: true },
            'Got it!'
        );
    }

    openConfiguration() {
        this.vscode.commands.executeCommand('workbench.action.openSettings', '@ext:st93642.tsi-header tsiheader.studyMode');
    }

    formatDuration(milliseconds) {
        const totalMinutes = Math.floor(milliseconds / (1000 * 60));
        const hours = Math.floor(totalMinutes / 60);
        const minutes = totalMinutes % 60;

        if (hours > 0) {
            return `${hours}h ${minutes}m`;
        } else {
            return `${minutes}m`;
        }
    }

    dispose() {
        if (this.timer) {
            this.timer.dispose();
        }
    }
}

module.exports = { StudyModeExtension };