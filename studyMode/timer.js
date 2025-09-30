/**
 * Study Mode Timer - Core Timer Logic
 * Implements Pomodoro timer with work/break phases
 */

class StudyModeTimer {
    constructor(vscode, context, config = {}, onStateChange = null) {
        this.vscode = vscode;
        this.context = context;
        this.onStateChange = onStateChange; // Callback for state persistence

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
        this.notifyStateChange();
    }

    pause() {
        if (this.isRunning) {
            // Store the remaining time before setting isRunning to false
            this.remainingTime = this.getRemainingTime();
            this.isRunning = false;
            this.pausedTime = Date.now();
            this.clearTimerInterval();
            this.updateStatusBar();
            this.notifyStateChange();
        }
    }

    resume() {
        if (!this.isRunning && this.currentPhase !== 'stopped') {
            // If pausedTime exists (normal pause/resume), adjust startTime for paused duration
            if (this.pausedTime) {
                const pausedDuration = Date.now() - this.pausedTime;
                this.startTime += pausedDuration;
                this.pausedTime = null;
            }
            // For timers loaded from persistence, startTime is already correctly set

            this.isRunning = true;
            this.startTimerInterval();
            this.updateStatusBar();
            this.notifyStateChange();
        }
    }

    stop() {
        this.clearTimerInterval();
        this.logSession(false); // Log as incomplete
        this.reset();
        this.updateStatusBar();
        this.notifyStateChange();
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
        // Log the completed session
        this.logSession(true);

        // Update session counter
        if (this.currentPhase === 'work') {
            this.currentSession++;
        }

        // Determine next phase
        if (this.currentPhase === 'work') {
            if (this.currentSession % this.sessionsBeforeLongBreak === 0) {
                this.currentPhase = 'longBreak';
            } else {
                this.currentPhase = 'shortBreak';
            }
        } else {
            this.currentPhase = 'work';
        }

        this.startTime = Date.now();
        this.remainingTime = this.getCurrentDuration();
        
        // For break phases, pause timer immediately, then play audio signal and show popup
        if (this.currentPhase === 'shortBreak' || this.currentPhase === 'longBreak') {
            // Pause timer immediately when entering break phase
            this.isRunning = false;
            this.clearTimerInterval();
            this.updateStatusBar();
            
            this.playBreakAudioSignal(() => {
                this.showBreakPopup();
            });
        } else if (this.currentPhase === 'work' && this.currentSession > 0) {
            // For work phase transitions (returning from break), pause timer and show popup
            this.isRunning = false;
            this.clearTimerInterval();
            this.updateStatusBar();
            
            this.playWorkAudioSignal(() => {
                this.showWorkPopup();
            });
        } else {
            this.showPhaseNotification();
        }
        
        this.notifyStateChange();
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
                text = this.isRunning ? this.formatTime(this.getRemainingTime()) : this.formatTime(this.remainingTime);
                tooltip = `Work Session ${this.currentSession + 1}/${this.sessionsBeforeLongBreak}`;
                break;
            case 'shortBreak':
                icon = this.isRunning ? '☕' : '⏸️☕';
                text = this.isRunning ? this.formatTime(this.getRemainingTime()) : this.formatTime(this.remainingTime);
                tooltip = this.isRunning ? 'Short Break' : 'Short Break - Choose Take Break or Skip Break';
                break;
            case 'longBreak':
                icon = this.isRunning ? '🏖️' : '⏸️🏖️';
                text = this.isRunning ? this.formatTime(this.getRemainingTime()) : this.formatTime(this.remainingTime);
                tooltip = this.isRunning ? 'Long Break' : 'Long Break - Choose Take Break or Skip Break';
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
        if (!this.vscode || !this.vscode.window || !this.vscode.window.showInformationMessage) return; // Skip if no vscode API (e.g., in tests)

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

    showBreakPopup() {
        // Pause the timer while popup is active
        const wasRunning = this.isRunning;
        if (this.isRunning) {
            this.isRunning = false;
            this.clearTimerInterval();
            this.updateStatusBar();
        }

        if (!this.vscode || !this.vscode.window || !this.vscode.window.showInformationMessage) return; // Skip if no vscode API (e.g., in tests)

        const isLongBreak = this.currentPhase === 'longBreak';
        const breakDuration = isLongBreak ? this.longBreakDuration : this.shortBreakDuration;
        const breakMinutes = Math.floor(breakDuration / (60 * 1000));
        
        const message = isLongBreak 
            ? `🎉 Excellent work! You've completed ${this.sessionsBeforeLongBreak} sessions.\n\nTime for a well-deserved ${breakMinutes}-minute break.`
            : `☕ Great job on that work session!\n\nTake a ${breakMinutes}-minute break to recharge.`;

        this.vscode.window.showInformationMessage(
            message,
            { modal: true },
            'Take Break',
            'Skip Break'
        ).then(selection => {
            if (selection === 'Take Break') {
                this.startBreak();
            } else if (selection === 'Skip Break') {
                this.skipBreak();
            } else {
                // If popup was dismissed without selection, resume previous state
                if (wasRunning) {
                    this.isRunning = true;
                    this.startTimerInterval();
                    this.updateStatusBar();
                }
            }
        });
    }

    startBreak() {
        // Start the break timer from now (when user chooses Take Break)
        this.startTime = Date.now();
        this.remainingTime = this.getCurrentDuration();
        this.isRunning = true;
        this.startTimerInterval();
        this.updateStatusBar();

        this.notifyStateChange();
    }

    skipBreak() {
        // Skip the break and immediately start the next work session
        this.currentPhase = 'work';
        this.startTime = Date.now();
        this.remainingTime = this.getCurrentDuration();
        this.isRunning = true;
        this.startTimerInterval();
        this.updateStatusBar();
        this.showPhaseNotification();
        this.notifyStateChange();
    }

    showWorkPopup() {
        // Pause the timer while popup is active
        const wasRunning = this.isRunning;
        if (this.isRunning) {
            this.isRunning = false;
            this.clearTimerInterval();
            this.updateStatusBar();
        }

        if (!this.vscode || !this.vscode.window || !this.vscode.window.showInformationMessage) return; // Skip if no vscode API (e.g., in tests)

        const workMinutes = Math.floor(this.workDuration / (60 * 1000));
        
        const message = `💼 Break time is over!\n\nReady to start your ${workMinutes}-minute work session?`;

        this.vscode.window.showInformationMessage(
            message,
            { modal: true },
            'Start Work',
            'Take More Break'
        ).then(selection => {
            if (selection === 'Start Work') {
                this.startWork();
            } else if (selection === 'Take More Break') {
                this.takeMoreBreak();
            } else {
                // If popup was dismissed without selection, resume previous state
                if (wasRunning) {
                    this.isRunning = true;
                    this.startTimerInterval();
                    this.updateStatusBar();
                }
            }
        });
    }

    startWork() {
        // Start the work timer from now (when user chooses Start Work)
        this.startTime = Date.now();
        this.remainingTime = this.getCurrentDuration();
        this.isRunning = true;
        this.startTimerInterval();
        this.updateStatusBar();

        this.notifyStateChange();
    }

    takeMoreBreak() {
        // Take additional break time
        this.currentPhase = this.currentPhase === 'longBreak' ? 'longBreak' : 'shortBreak';
        this.startTime = Date.now();
        this.remainingTime = this.getCurrentDuration();
        this.isRunning = true;
        this.startTimerInterval();
        this.updateStatusBar();
        this.showPhaseNotification();
        this.notifyStateChange();
    }

    playBreakAudioSignal(callback) {
        if (!this.vscode) {
            // In tests or when no vscode API, just call callback immediately
            setTimeout(callback, 10);
            return;
        }

        // In test environment, call callback immediately without any delays
        const isTestEnvironment = process.env.NODE_ENV === 'test' ||
            (typeof process !== 'undefined' && process.env && process.env.NODE_TEST_CONTEXT);

        if (isTestEnvironment) {
            callback(); // Call synchronously in test environment
            return;
        }

        // Check if sounds are enabled in configuration
        const config = this.vscode.workspace.getConfiguration('tsiheader.studyMode');
        const soundsEnabled = config.get('enableSounds', false);

        if (!soundsEnabled) {
            console.log('Audio signals disabled in configuration');
            setTimeout(callback, 10);
            return;
        }

        console.log('Playing break audio signal...');

        try {
            // Method 1: Try VS Code's built-in notification sound
            if (this.vscode.window && this.vscode.window.showInformationMessage) {
                // Use VS Code's notification system which may trigger system sounds
                this.vscode.window.showInformationMessage('🚨 BREAK TIME! 🚨 Take a break!', { modal: false });
            }

            // Method 2: Try system bell via terminal
            if (this.vscode.window && this.vscode.window.createTerminal) {
                try {
                    const terminal = this.vscode.window.createTerminal('BreakBell');
                    terminal.sendText('echo -e "\a\a\a\a\a"'); // Multiple loud bells
                    terminal.sendText('tput bel 2>/dev/null || printf "\a\a\a\a\a"');
                    // Try to play system sound
                    terminal.sendText('play /usr/share/sounds/freedesktop/stereo/bell.oga 2>/dev/null || paplay /usr/share/sounds/freedesktop/stereo/bell.oga 2>/dev/null || aplay /usr/share/sounds/alsa/Front_Center.wav 2>/dev/null || echo -e "\a\a\a\a\a"');

                    setTimeout(() => {
                        try {
                            terminal.dispose();
                        } catch (e) {}
                    }, 1000);
                } catch (e) {
                    console.log('Terminal bell failed:', e.message);
                }
            }

            // Method 3: Try VS Code commands for bell
            if (this.vscode.commands) {
                try {
                    // Try multiple bell commands
                    this.vscode.commands.executeCommand('workbench.action.terminal.sendSequence', { text: '\u0007\u0007\u0007' });
                    this.vscode.commands.executeCommand('workbench.action.terminal.sendSequence', { text: '\x07\x07\x07' });
                } catch (e) {
                    console.log('VS Code bell commands failed:', e.message);
                }
            }

            // Method 4: Try to trigger system notification
            if (this.vscode.env && this.vscode.env.openExternal) {
                try {
                    this.vscode.env.openExternal('bell://notification');
                } catch (e) {
                    console.log('External bell URL failed:', e.message);
                }
            }

            // Call callback after attempting all methods
            setTimeout(callback, 500);

        } catch (error) {
            console.warn('Failed to play break audio signal:', error.message);
            setTimeout(callback, 10);
        }
    }

    playWorkAudioSignal(callback) {
        if (!this.vscode) {
            // In tests or when no vscode API, just call callback immediately
            setTimeout(callback, 10);
            return;
        }

        // In test environment, call callback immediately without any delays
        const isTestEnvironment = process.env.NODE_ENV === 'test' ||
            (typeof process !== 'undefined' && process.env && process.env.NODE_TEST_CONTEXT);

        if (isTestEnvironment) {
            callback(); // Call synchronously in test environment
            return;
        }

        // Check if sounds are enabled in configuration
        const config = this.vscode.workspace.getConfiguration('tsiheader.studyMode');
        const soundsEnabled = config.get('enableSounds', false);

        if (!soundsEnabled) {
            console.log('Audio signals disabled in configuration');
            setTimeout(callback, 10);
            return;
        }

        console.log('Playing work audio signal...');

        try {
            // Method 1: Try VS Code's built-in notification sound
            if (this.vscode.window && this.vscode.window.showInformationMessage) {
                // Use VS Code's notification system which may trigger system sounds
                this.vscode.window.showInformationMessage('💼 WORK TIME! 💼 Focus session starting!', { modal: false });
            }

            // Method 2: Try system bell via terminal
            if (this.vscode.window && this.vscode.window.createTerminal) {
                try {
                    const terminal = this.vscode.window.createTerminal('WorkBell');
                    terminal.sendText('echo -e "\a\a\a\a\a"'); // Multiple loud bells for work start
                    terminal.sendText('tput bel 2>/dev/null || printf "\a\a\a\a\a"');
                    // Try to play system sound for work
                    terminal.sendText('play /usr/share/sounds/freedesktop/stereo/message.oga 2>/dev/null || paplay /usr/share/sounds/freedesktop/stereo/message.oga 2>/dev/null || aplay /usr/share/sounds/alsa/Side_Right.wav 2>/dev/null || echo -e "\a\a\a\a\a"');

                    setTimeout(() => {
                        try {
                            terminal.dispose();
                        } catch (e) {}
                    }, 1000);
                } catch (e) {
                    console.log('Terminal work bell failed:', e.message);
                }
            }

            // Method 3: Try VS Code commands for bell
            if (this.vscode.commands) {
                try {
                    // Try multiple bell commands for work start
                    this.vscode.commands.executeCommand('workbench.action.terminal.sendSequence', { text: '\u0007\u0007\u0007\u0007\u0007' });
                    this.vscode.commands.executeCommand('workbench.action.terminal.sendSequence', { text: '\x07\x07\x07\x07\x07' });
                } catch (e) {
                    console.log('VS Code work bell commands failed:', e.message);
                }
            }

            // Method 4: Try to trigger system notification
            if (this.vscode.env && this.vscode.env.openExternal) {
                try {
                    this.vscode.env.openExternal('work://notification');
                } catch (e) {
                    console.log('External work URL failed:', e.message);
                }
            }

            // Call callback after attempting all methods
            setTimeout(callback, 500);

        } catch (error) {
            console.warn('Failed to play work audio signal:', error.message);
            setTimeout(callback, 10);
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

    notifyStateChange() {
        if (this.onStateChange) {
            try {
                this.onStateChange();
            } catch (error) {
                console.warn('Error in state change callback:', error.message);
            }
        }
    }

    updateConfiguration(config = {}) {
        this.workDuration = (config.workDuration || 25) * 60 * 1000;
        this.shortBreakDuration = (config.shortBreakDuration || 5) * 60 * 1000;
        this.longBreakDuration = (config.longBreakDuration || 15) * 60 * 1000;
        this.sessionsBeforeLongBreak = config.sessionsBeforeLongBreak || 4;
    }

    dispose() {
        this.clearTimerInterval();
        if (this.statusBarItem && this.statusBarItem.dispose) {
            this.statusBarItem.dispose();
        }
    }
}

module.exports = { StudyModeTimer };