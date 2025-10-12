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

        // Timer state - using elapsed time instead of wall clock for reliability
        this.currentPhase = 'stopped'; // 'stopped', 'work', 'shortBreak', 'longBreak'
        this.currentSession = 0;
        this.isRunning = false;
        this.elapsedTime = 0; // Time elapsed in current phase (milliseconds) - persisted
        this.lastTickTime = null; // High-resolution time for accurate tick calculation
        this.remainingTime = 0; // Calculated from elapsedTime
        
        // For session logging only
        this.phaseStartTimestamp = null; // Wall clock timestamp when phase started (for logs only)

        // Session logging
        this.sessionLog = [];

        // Status bar item (positioned on the right side)
        this.statusBarItem = null;
        if (vscode && vscode.window && vscode.window.createStatusBarItem) {
            this.statusBarItem = vscode.window.createStatusBarItem(vscode.StatusBarAlignment.Right, 1000);
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
            this.elapsedTime = 0;
            this.remainingTime = this.workDuration;
            this.phaseStartTimestamp = Date.now(); // For logging only
        }

        this.isRunning = true;
        this.lastTickTime = process.hrtime.bigint();
        this.startTimerInterval();
        this.updateStatusBar();
        this.notifyStateChange();
    }

    pause() {
        if (this.isRunning) {
            // Update elapsed time before pausing
            this.updateElapsedTime();
            this.remainingTime = this.getCurrentDuration() - this.elapsedTime;
            
            this.isRunning = false;
            this.lastTickTime = null;
            this.clearTimerInterval();
            this.updateStatusBar();
            this.notifyStateChange();
        }
    }

    resume() {
        if (!this.isRunning && this.currentPhase !== 'stopped') {
            this.isRunning = true;
            this.lastTickTime = process.hrtime.bigint();
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
        this.elapsedTime = 0;
        this.lastTickTime = null;
        this.remainingTime = 0;
        this.phaseStartTimestamp = null;
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

        this.updateElapsedTime();
        this.remainingTime = Math.max(0, this.getCurrentDuration() - this.elapsedTime);

        if (this.remainingTime <= 0) {
            this.transitionToNextPhase();
        }

        this.updateStatusBar();
    }
    
    updateElapsedTime() {
        if (!this.isRunning || !this.lastTickTime) return;
        
        const currentTime = process.hrtime.bigint();
        const tickDuration = Number(currentTime - this.lastTickTime) / 1e6; // Convert to milliseconds
        this.elapsedTime += tickDuration;
        this.lastTickTime = currentTime;
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

        // Reset elapsed time for new phase
        this.elapsedTime = 0;
        this.remainingTime = this.getCurrentDuration();
        this.phaseStartTimestamp = Date.now(); // For logging only
        
        // For break phases, pause timer immediately, then play audio signal and show popup
        if (this.currentPhase === 'shortBreak' || this.currentPhase === 'longBreak') {
            // Pause timer immediately when entering break phase
            this.isRunning = false;
            this.lastTickTime = null;
            this.clearTimerInterval();
            this.updateStatusBar();
            
            this.playBreakAudioSignal(() => {
                this.showBreakPopup();
            });
        } else if (this.currentPhase === 'work' && this.currentSession > 0) {
            // For work phase transitions (returning from break), pause timer and show popup
            this.isRunning = false;
            this.lastTickTime = null;
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
        if (this.currentPhase === 'stopped') return 0;
        
        if (this.isRunning) {
            this.updateElapsedTime();
        }
        
        return Math.max(0, this.getCurrentDuration() - this.elapsedTime);
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
        let visualCountdown = '';
        let tooltip = '';

        switch (this.currentPhase) {
            case 'work':
                icon = this.isRunning ? '🍅' : '⏸️🍅';
                visualCountdown = this.generateTomatoCountdown();
                const workTime = this.isRunning ? this.getRemainingTime() : this.remainingTime;
                tooltip = `Work Session ${this.currentSession + 1}/${this.sessionsBeforeLongBreak} - ${this.formatTime(workTime)} remaining`;
                break;
            case 'shortBreak':
                icon = this.isRunning ? '☕' : '⏸️☕';
                visualCountdown = this.generateCoffeeCountdown();
                const shortBreakTime = this.isRunning ? this.getRemainingTime() : this.remainingTime;
                tooltip = this.isRunning ? `Short Break - ${this.formatTime(shortBreakTime)} remaining` : 'Short Break - Choose Take Break or Skip Break';
                break;
            case 'longBreak':
                icon = this.isRunning ? '🏖️' : '⏸️🏖️';
                visualCountdown = this.generateCoffeeCountdown();
                const longBreakTime = this.isRunning ? this.getRemainingTime() : this.remainingTime;
                tooltip = this.isRunning ? `Long Break - ${this.formatTime(longBreakTime)} remaining` : 'Long Break - Choose Take Break or Skip Break';
                break;
            case 'stopped':
                icon = '⏹️';
                visualCountdown = 'Study Mode';
                tooltip = 'Click to start study session';
                break;
        }

        this.statusBarItem.text = `${icon} ${visualCountdown}`;
        this.statusBarItem.tooltip = tooltip;

        if (this.currentPhase !== 'stopped') {
            this.statusBarItem.show();
        } else {
            this.statusBarItem.hide();
        }
    }

    generateTomatoCountdown() {
        const remainingTime = this.isRunning ? this.getRemainingTime() : this.remainingTime;
        const remainingMinutes = Math.ceil(remainingTime / (60 * 1000)); // Round up to next minute
        
        // For sessions > 30 minutes, show remaining minutes modulo 30
        // This creates a "renewing" effect every 30 minutes
        const displayMinutes = remainingMinutes > 30 ? remainingMinutes % 30 || 30 : remainingMinutes;
        
        // Generate tomato string
        let tomatoes = '';
        for (let i = 0; i < displayMinutes; i++) {
            tomatoes += '🍅';
        }
        
        return tomatoes || '🍅'; // Always show at least one tomato
    }

    generateCoffeeCountdown() {
        const remainingTime = this.isRunning ? this.getRemainingTime() : this.remainingTime;
        const remainingMinutes = Math.ceil(remainingTime / (60 * 1000)); // Round up to next minute
        
        // For sessions > 30 minutes, show remaining minutes modulo 30
        // This creates a "renewing" effect every 30 minutes
        const displayMinutes = remainingMinutes > 30 ? remainingMinutes % 30 || 30 : remainingMinutes;
        
        // Generate coffee string
        let coffees = '';
        for (let i = 0; i < displayMinutes; i++) {
            coffees += '☕';
        }
        
        return coffees || '☕'; // Always show at least one coffee cup
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
        this.elapsedTime = 0;
        this.remainingTime = this.getCurrentDuration();
        this.phaseStartTimestamp = Date.now(); // For logging only
        this.isRunning = true;
        this.lastTickTime = process.hrtime.bigint();
        this.startTimerInterval();
        this.updateStatusBar();

        this.notifyStateChange();
    }

    skipBreak() {
        // Skip the break and immediately start the next work session
        this.currentPhase = 'work';
        this.elapsedTime = 0;
        this.remainingTime = this.getCurrentDuration();
        this.phaseStartTimestamp = Date.now(); // For logging only
        this.isRunning = true;
        this.lastTickTime = process.hrtime.bigint();
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
        this.elapsedTime = 0;
        this.remainingTime = this.getCurrentDuration();
        this.phaseStartTimestamp = Date.now(); // For logging only
        this.isRunning = true;
        this.lastTickTime = process.hrtime.bigint();
        this.startTimerInterval();
        this.updateStatusBar();

        this.notifyStateChange();
    }

    takeMoreBreak() {
        // Take additional break time
        this.currentPhase = this.currentPhase === 'longBreak' ? 'longBreak' : 'shortBreak';
        this.elapsedTime = 0;
        this.remainingTime = this.getCurrentDuration();
        this.phaseStartTimestamp = Date.now(); // For logging only
        this.isRunning = true;
        this.lastTickTime = process.hrtime.bigint();
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
        if (this.phaseStartTimestamp && this.currentPhase !== 'stopped') {
            const endTime = Date.now();
            const session = {
                type: this.currentPhase,
                startTime: new Date(this.phaseStartTimestamp).toISOString(),
                endTime: new Date(endTime).toISOString(),
                duration: endTime - this.phaseStartTimestamp,
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