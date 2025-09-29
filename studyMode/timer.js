/**
 * Study Mode Timer - Core Timer Logic
 * Implements Pomodoro timer with work/break phases
 */

class StudyModeTimer {
    constructor(vscode, context, config = {}) {
        this.vscode = vscode;
        this.context = context;

        // Default configuration
        this.workDuration = (config.workDuration || 25) * 60 * 1000; // minutes to ms
        this.shortBreakDuration = (config.shortBreakDuration || 5) * 60 * 1000;
        this.longBreakDuration = (config.longBreakDuration || 15) * 60 * 1000;
        this.sessionsBeforeLongBreak = config.sessionsBeforeLongBreak || 4;

        // Timer state
        this.currentPhase = 'stopped'; // 'stopped', 'work', 'shortBreak', 'longBreak'
        this.currentSession = 0;
        this.isRunning = false;
        this.startTime = null;
        this.pausedTime = null;
        this.remainingTime = 0;

        // Session logging
        this.sessionLog = [];

        // Status bar item
        this.statusBarItem = null;
        if (vscode && vscode.window && vscode.window.createStatusBarItem) {
            this.statusBarItem = vscode.window.createStatusBarItem(vscode.StatusBarAlignment.Left, 100);
            this.statusBarItem.command = 'tsiheader.pauseStudyTimer';
            this.context.subscriptions.push(this.statusBarItem);
        }

        // Timer interval
        this.timerInterval = null;
    }

    start() {
        if (this.currentPhase === 'stopped') {
            this.currentPhase = 'work';
            this.currentSession = 0;
            this.startTime = Date.now();
            this.remainingTime = this.workDuration;
        }

        this.isRunning = true;
        this.pausedTime = null;
        this.startTimerInterval();
        this.updateStatusBar();
    }

    pause() {
        if (this.isRunning) {
            this.isRunning = false;
            this.pausedTime = Date.now();
            this.clearTimerInterval();
            this.updateStatusBar();
        }
    }

    resume() {
        if (!this.isRunning && this.pausedTime) {
            const pausedDuration = Date.now() - this.pausedTime;
            this.startTime += pausedDuration;
            this.isRunning = true;
            this.pausedTime = null;
            this.startTimerInterval();
            this.updateStatusBar();
        }
    }

    stop() {
        this.clearTimerInterval();
        this.logSession(false); // Log as incomplete
        this.reset();
        this.updateStatusBar();
    }

    reset() {
        this.currentPhase = 'stopped';
        this.currentSession = 0;
        this.isRunning = false;
        this.startTime = null;
        this.pausedTime = null;
        this.remainingTime = 0;
    }

    startTimerInterval() {
        this.clearTimerInterval();
        this.timerInterval = setInterval(() => {
            this.tick();
        }, 1000);
    }

    clearTimerInterval() {
        if (this.timerInterval) {
            clearInterval(this.timerInterval);
            this.timerInterval = null;
        }
    }

    tick() {
        if (!this.isRunning) return;

        const elapsed = Date.now() - this.startTime;
        this.remainingTime = Math.max(0, this.getCurrentDuration() - elapsed);

        if (this.remainingTime <= 0) {
            this.transitionToNextPhase();
        }

        this.updateStatusBar();
    }

    transitionToNextPhase() {
        this.logSession(true); // Log completed session

        switch (this.currentPhase) {
            case 'work':
                this.currentSession++;
                if (this.currentSession >= this.sessionsBeforeLongBreak) {
                    this.currentPhase = 'longBreak';
                    this.currentSession = 0;
                } else {
                    this.currentPhase = 'shortBreak';
                }
                break;
            case 'shortBreak':
            case 'longBreak':
                this.currentPhase = 'work';
                break;
        }

        this.startTime = Date.now();
        this.remainingTime = this.getCurrentDuration();
        this.showPhaseNotification();
    }

    getCurrentDuration() {
        switch (this.currentPhase) {
            case 'work': return this.workDuration;
            case 'shortBreak': return this.shortBreakDuration;
            case 'longBreak': return this.longBreakDuration;
            default: return 0;
        }
    }

    getRemainingTime() {
        if (!this.isRunning || !this.startTime) return 0;

        const elapsed = Date.now() - this.startTime;
        return Math.max(0, this.getCurrentDuration() - elapsed);
    }

    formatTime(milliseconds) {
        const totalSeconds = Math.ceil(milliseconds / 1000);
        const minutes = Math.floor(totalSeconds / 60);
        const seconds = totalSeconds % 60;
        return `${minutes.toString().padStart(2, '0')}:${seconds.toString().padStart(2, '0')}`;
    }

    updateStatusBar() {
        if (!this.statusBarItem) return; // Skip if no status bar item (e.g., in tests)

        let icon = '';
        let text = '';
        let tooltip = '';

        switch (this.currentPhase) {
            case 'work':
                icon = this.isRunning ? '🍅' : '⏸️🍅';
                text = this.formatTime(this.getRemainingTime());
                tooltip = `Work Session ${this.currentSession + 1}/${this.sessionsBeforeLongBreak}`;
                break;
            case 'shortBreak':
                icon = this.isRunning ? '☕' : '⏸️☕';
                text = this.formatTime(this.getRemainingTime());
                tooltip = 'Short Break';
                break;
            case 'longBreak':
                icon = this.isRunning ? '🏖️' : '⏸️🏖️';
                text = this.formatTime(this.getRemainingTime());
                tooltip = 'Long Break';
                break;
            case 'stopped':
                icon = '⏹️';
                text = 'Study Mode';
                tooltip = 'Click to start study session';
                break;
        }

        this.statusBarItem.text = `${icon} ${text}`;
        this.statusBarItem.tooltip = tooltip;

        if (this.currentPhase !== 'stopped') {
            this.statusBarItem.show();
        } else {
            this.statusBarItem.hide();
        }
    }

    showPhaseNotification() {
        if (!vscode || !vscode.window || !vscode.window.showInformationMessage) return; // Skip if no vscode API (e.g., in tests)

        let message = '';
        switch (this.currentPhase) {
            case 'work':
                message = 'Time to focus! Work session started.';
                break;
            case 'shortBreak':
                message = 'Great work! Take a short break.';
                break;
            case 'longBreak':
                message = 'Well done! Time for a longer break.';
                break;
        }

        if (message) {
            this.vscode.window.showInformationMessage(message);
        }
    }

    logSession(completed) {
        if (this.startTime && this.currentPhase !== 'stopped') {
            const session = {
                type: this.currentPhase,
                startTime: new Date(this.startTime).toISOString(),
                endTime: new Date().toISOString(),
                duration: Date.now() - this.startTime,
                completed: completed,
                sessionNumber: this.currentSession
            };
            this.sessionLog.push(session);
        }
    }

    dispose() {
        this.clearTimerInterval();
        if (this.statusBarItem && this.statusBarItem.dispose) {
            this.statusBarItem.dispose();
        }
    }
}

module.exports = { StudyModeTimer };