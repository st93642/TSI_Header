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

        // Audio handler for cleanup
        this.currentAudioHandler = null;

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
        
        // For break phases, show popup instead of auto-starting
        if (this.currentPhase === 'shortBreak' || this.currentPhase === 'longBreak') {
            this.showBreakPopup();
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
                tooltip = 'Short Break';
                break;
            case 'longBreak':
                icon = this.isRunning ? '🏖️' : '⏸️🏖️';
                text = this.isRunning ? this.formatTime(this.getRemainingTime()) : this.formatTime(this.remainingTime);
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
            }
        });
    }

    startBreak() {
        // Start the break timer
        this.isRunning = true;
        this.startTimerInterval();
        this.updateStatusBar();
        
        // Play audio signal
        this.playBreakAudio();
        
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

    playBreakAudio() {
        if (!this.vscode || !this.vscode.window || !this.vscode.window.createWebviewPanel) return; // Skip if no vscode API

        try {
            // Create a temporary webview for audio playback
            const panel = this.vscode.window.createWebviewPanel(
                'studyModeAudio',
                'Study Mode Audio',
                { viewColumn: -1, preserveFocus: true }, // Hidden panel
                { enableScripts: true }
            );

            const audioType = this.currentPhase === 'longBreak' ? 'longBreak' : 'shortBreak';

            // Generate HTML with audio element that auto-plays
            panel.webview.html = this.getAudioWebviewContent(audioType);

            // Listen for messages from the webview
            const messageHandler = panel.webview.onDidReceiveMessage(message => {
                if (message.type === 'audioStarted') {
                    // Audio started successfully
                    console.log('Break audio started');
                } else if (message.type === 'audioError') {
                    console.warn('Break audio failed:', message.error);
                }
            });

            // Store the message handler to clean it up
            this.currentAudioHandler = messageHandler;

            // Send message to webview to play audio after a short delay to ensure it's loaded
            setTimeout(() => {
                panel.webview.postMessage({ type: 'playAudio', audioType });
            }, 100);

            // Auto-dispose after audio duration + buffer
            setTimeout(() => {
                panel.dispose();
                if (this.currentAudioHandler) {
                    this.currentAudioHandler.dispose();
                    this.currentAudioHandler = null;
                }
            }, 2000); // 2 seconds should be enough for 1 second of audio

        } catch (error) {
            console.warn('Failed to play break audio:', error.message);
        }
    }

    getAudioWebviewContent(audioType) {
        // Generate different tones for different break types
        const frequency = audioType === 'longBreak' ? 800 : 600; // Higher pitch for long break
        const duration = 1000; // 1 second

        return `
            <!DOCTYPE html>
            <html>
            <head>
                <meta charset="UTF-8">
                <title>Study Mode Audio</title>
            </head>
            <body>
                <script>
                    // VS Code API
                    const vscode = acquireVsCodeApi();

                    // Generate audio tone
                    function playTone(frequency, duration) {
                        try {
                            const audioContext = new (window.AudioContext || window.webkitAudioContext)();

                            // Resume audio context if suspended (required by some browsers)
                            if (audioContext.state === 'suspended') {
                                audioContext.resume();
                            }

                            const oscillator = audioContext.createOscillator();
                            const gainNode = audioContext.createGain();

                            oscillator.connect(gainNode);
                            gainNode.connect(audioContext.destination);

                            oscillator.frequency.value = frequency;
                            oscillator.type = 'sine';

                            // Set volume (0.3 for audible but not too loud)
                            gainNode.gain.setValueAtTime(0.3, audioContext.currentTime);
                            gainNode.gain.exponentialRampToValueAtTime(0.01, audioContext.currentTime + duration / 1000);

                            oscillator.start(audioContext.currentTime);
                            oscillator.stop(audioContext.currentTime + duration / 1000);

                            // Notify that audio started
                            vscode.postMessage({ type: 'audioStarted' });

                            console.log('Playing break audio tone');
                        } catch (error) {
                            console.warn('Audio playback failed:', error);
                            vscode.postMessage({ type: 'audioError', error: error.message });
                        }
                    }

                    // Listen for messages from extension
                    window.addEventListener('message', event => {
                        const message = event.data;
                        if (message.type === 'playAudio') {
                            playTone(${frequency}, ${duration});
                        }
                    });

                    // Auto-play on load as fallback
                    setTimeout(() => {
                        playTone(${frequency}, ${duration});
                    }, 50);
                </script>
            </body>
            </html>
        `;
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
        if (this.currentAudioHandler && this.currentAudioHandler.dispose) {
            this.currentAudioHandler.dispose();
            this.currentAudioHandler = null;
        }
    }
}

module.exports = { StudyModeTimer };