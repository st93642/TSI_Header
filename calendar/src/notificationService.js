/**
 * Notification Service
 * Handles email notifications for calendar events and deadlines
 */

const vscode = require('vscode');
const https = require('https');
const http = require('http');

class NotificationService {
    constructor(eventManager) {
        this.eventManager = eventManager;
        this.checkInterval = null;
        this.lastCheckTime = Date.now();
        this.sentNotifications = new Set(); // Track sent notifications in memory
        this.initialize();
    }

    /**
     * Initialize the notification service
     */
    async initialize() {
        const config = vscode.workspace.getConfiguration('tsiheader.notifications');

        // Load previously sent notifications from persistent storage
        const context = await this.getExtensionContext();
        if (context) {
            const stored = context.globalState.get('sentNotifications', []);
            this.sentNotifications = new Set(stored);
            console.log(`Loaded ${this.sentNotifications.size} previously sent notifications`);
        }

        if (config.get('enableEmail', false)) {
            this.startNotificationChecker();
            console.log('Email notifications enabled');
        } else {
            this.stopNotificationChecker();
            console.log('Email notifications disabled');
        }
    }

    /**
     * Get extension context for persistent storage
     */
    async getExtensionContext() {
        // This will be set by the calendar manager when creating the service
        return this.context || null;
    }

    /**
     * Set extension context for persistent storage
     */
    setContext(context) {
        this.context = context;
        // Load previously sent notifications
        const stored = context.globalState.get('sentNotifications', []);
        this.sentNotifications = new Set(stored);
    }

    /**
     * Start the periodic notification checker
     */
    startNotificationChecker() {
        if (this.checkInterval) {
            clearInterval(this.checkInterval);
        }

        // Check every hour instead of every 15 minutes to reduce frequency
        this.checkInterval = setInterval(() => {
            this.checkUpcomingEvents();
        }, 60 * 60 * 1000); // 1 hour

        // Initial check with a small delay
        setTimeout(() => {
            this.checkUpcomingEvents();
        }, 5000); // 5 second delay on startup
    }

    /**
     * Stop the notification checker
     */
    stopNotificationChecker() {
        if (this.checkInterval) {
            clearInterval(this.checkInterval);
            this.checkInterval = null;
        }
    }

    /**
     * Check for upcoming events that need notifications
     */
    async checkUpcomingEvents() {
        try {
            const config = vscode.workspace.getConfiguration('tsiheader.notifications');
            const advanceNotice = config.get('advanceNotice', 24); // hours
            const now = new Date();
            const notificationTime = new Date(now.getTime() + (advanceNotice * 60 * 60 * 1000));

            console.log(`Checking for upcoming events (advance notice: ${advanceNotice} hours)`);

            // Get events in the notification window
            const events = await this.getUpcomingEvents(notificationTime);

            console.log(`Found ${events.length} events requiring notifications`);

            for (const event of events) {
                const result = await this.sendNotification(event);
                if (result.success && !result.alreadySent) {
                    console.log(`Sent notification for: ${event.title}`);
                } else if (result.alreadySent) {
                    console.log(`Notification already sent for: ${event.title}`);
                }
            }
        } catch (error) {
            console.error('Error checking upcoming events:', error);
        }
    }

    /**
     * Get upcoming events that need notifications
     */
    async getUpcomingEvents(notificationTime) {
        try {
            const now = new Date();
            const endTime = new Date(notificationTime);

            // Get events in the notification window
            const events = await this.eventManager.getEventsInRange(
                now.toISOString().split('T')[0],
                endTime.toISOString().split('T')[0]
            );

            // Filter events that haven't been notified yet and are within the time window
            const upcomingEvents = events.filter(event => {
                const eventTime = new Date(event.start || event.date);
                const timeDiff = eventTime.getTime() - now.getTime();
                const hoursDiff = timeDiff / (1000 * 60 * 60);

                // Check if event is within notification window and not already notified
                const notificationKey = `${event.id}-${event.start || event.date}`;
                return hoursDiff > 0 && hoursDiff <= 24 && !this.sentNotifications.has(notificationKey);
            });

            return upcomingEvents;
        } catch (error) {
            console.error('Error getting upcoming events:', error);
            return [];
        }
    }

    /**
     * Send notification for an event
     */
    async sendNotification(event, customMessage = null) {
        const config = vscode.workspace.getConfiguration('tsiheader.notifications');
        const service = config.get('emailService');

        // Create a more specific notification key
        const eventTime = event.start || event.date;
        const notificationKey = `${event.id}-${event.title}-${eventTime}`;

        // Check if we've already sent this notification
        if (this.sentNotifications.has(notificationKey)) {
            console.log(`Notification already sent for event: ${event.title}`);
            return { success: true, alreadySent: true };
        }

        try {
            switch (service) {
                case 'smtp':
                    await this.sendSMTPNotification(event, customMessage);
                    break;
                default:
                    return { success: false, error: 'No email service configured' };
            }

            // Mark notification as sent
            this.sentNotifications.add(notificationKey);

            // Persist to storage
            await this.persistSentNotifications();

            console.log(`Notification sent for event: ${event.title}`);
            return { success: true };

        } catch (error) {
            console.error('Error sending notification:', error);
            return { success: false, error: error.message };
        }
    }

    /**
     * Persist sent notifications to storage
     */
    async persistSentNotifications() {
        try {
            const context = await this.getExtensionContext();
            if (context) {
                // Convert Set to Array for storage and limit size to prevent unbounded growth
                const notificationsArray = Array.from(this.sentNotifications);
                // Keep only the most recent 1000 notifications to prevent storage bloat
                if (notificationsArray.length > 1000) {
                    notificationsArray.splice(0, notificationsArray.length - 1000);
                    this.sentNotifications = new Set(notificationsArray);
                }
                await context.globalState.update('sentNotifications', notificationsArray);
            }
        } catch (error) {
            console.error('Error persisting sent notifications:', error);
        }
    }

    /**
     * Send notification via SMTP
     */
    async sendSMTPNotification(event, customMessage = null) {
        const config = vscode.workspace.getConfiguration('tsiheader.notifications');
        const host = config.get('smtpHost');
        const port = config.get('smtpPort', 587);
        const user = config.get('smtpUser');
        const password = config.get('smtpPassword');
        const emailAddress = config.get('emailAddress');

        if (!host || !user || !password || !emailAddress) {
            throw new Error('SMTP host, user, password, and email address required');
        }

        const emailContent = this.formatEmailContent(event, customMessage);
        const subject = `Upcoming: ${event.title}`;

        // Create SMTP email
        const fromEmail = user.includes('@') ? user : `${user}@${host}`;
        const emailData = this.createSMTPEmail(fromEmail, emailAddress, subject, emailContent);

        // Send via SMTP
        await this.sendViaSMTP(host, port, user, password, emailData, emailAddress);
    }

    /**
     * Format email content for notifications
     */
    formatEmailContent(event, customMessage = null) {
        const eventTime = event.start ? new Date(event.start).toLocaleString() : 'No time specified';

        let content = `
Hello!

You have an upcoming ${event.extendedProps?.type || 'event'}:

Title: ${event.title}
Time: ${eventTime}
Type: ${event.extendedProps?.type || 'Event'}

${event.extendedProps?.data?.description ? `Description: ${event.extendedProps.data.description}` : ''}

${customMessage ? `\n${customMessage}\n` : ''}

This is an automated notification from the TSI Header Extension.

Best regards,
TSI Header Extension
        `.trim();

        return content;
    }

    /**
     * Create SMTP email data
     */
    createSMTPEmail(from, to, subject, body) {
        const boundary = '----=_NextPart_' + Date.now();
        const date = new Date().toUTCString();

        const email = [
            `From: TSI Header Extension <${from}>`,
            `To: ${to}`,
            `Subject: ${subject}`,
            `Date: ${date}`,
            `MIME-Version: 1.0`,
            `Content-Type: text/plain; charset=utf-8`,
            `Content-Transfer-Encoding: 7bit`,
            '',
            body
        ].join('\r\n');

        return email;
    }

    /**
     * Send email via SMTP
     */
    sendViaSMTP(host, port, user, password, emailData, recipientEmail) {
        return new Promise((resolve, reject) => {
            const net = require('net');
            const tls = require('tls');

            let socket;
            let buffer = '';
            let currentCommand = null;
            let useTLS = port === 465;
            let tlsUpgraded = false;
            let expectingMultiline = false;

            const commands = [
                { cmd: () => `EHLO ${host}\r\n`, description: 'EHLO' },
                { cmd: () => `AUTH LOGIN\r\n`, description: 'AUTH LOGIN' },
                { cmd: () => Buffer.from(user).toString('base64') + '\r\n', description: 'Username' },
                { cmd: () => Buffer.from(password).toString('base64') + '\r\n', description: 'Password' },
                { cmd: () => `MAIL FROM:<${user.includes('@') ? user : user + '@' + host}>\r\n`, description: 'MAIL FROM' },
                { cmd: () => `RCPT TO:<${recipientEmail}>\r\n`, description: 'RCPT TO' },
                { cmd: () => `DATA\r\n`, description: 'DATA' },
                { cmd: () => emailData + '\r\n.\r\n', description: 'Email data' },
                { cmd: () => `QUIT\r\n`, description: 'QUIT' }
            ];

            let commandIndex = 0;
            let waitingForGreeting = true; // Wait for initial 220 greeting

            function createSocket() {
                if (useTLS) {
                    socket = tls.connect({
                        host: host,
                        port: port,
                        rejectUnauthorized: false // For development/testing
                    }, () => {
                        console.log('Connected to SMTP server with TLS');
                        // For TLS connections, server might send greeting immediately
                    });
                } else {
                    socket = net.createConnection({
                        host: host,
                        port: port
                    }, () => {
                        console.log('Connected to SMTP server');
                        // Don't send commands yet - wait for server greeting
                    });
                }

                socket.on('data', handleData);
                socket.on('error', (error) => {
                    console.error('SMTP socket error:', error);
                    reject(new Error(`SMTP connection error: ${error.message}`));
                });

                socket.on('end', () => {
                    console.log('SMTP connection closed');
                });
            }

            function sendNextCommand() {
                if (commandIndex >= commands.length) {
                    socket.end();
                    resolve();
                    return;
                }

                currentCommand = commands[commandIndex];
                console.log(`Sending SMTP command: ${currentCommand.description}`);
                socket.write(currentCommand.cmd());
            }

            function handleData(data) {
                buffer += data.toString();
                console.log('SMTP Response Buffer:', buffer.trim());

                // Process complete responses
                while (buffer.length > 0) {
                    const lines = buffer.split('\r\n');
                    if (lines.length < 2) break; // Wait for more data

                    const response = lines[0];
                    console.log('Processing SMTP response:', response);

                    if (!response || !/^\d{3}/.test(response)) {
                        // Not a valid response line, wait for more data
                        break;
                    }

                    const code = parseInt(response.substring(0, 3));
                    const isMultiline = response.charAt(3) === '-';

                    if (isMultiline) {
                        // Multi-line response, continue reading
                        expectingMultiline = true;
                        buffer = lines.slice(1).join('\r\n');
                        continue;
                    }

                    // Single line or end of multi-line response
                    expectingMultiline = false;

                    // Remove this response from buffer
                    buffer = lines.slice(1).join('\r\n');

                    // Handle the response
                    if (handleResponse(code, response)) {
                        return; // Command handled, exit loop
                    }
                }
            }

            function handleResponse(code, response) {
                console.log(`Handling SMTP response ${code}: ${response}`);

                if (waitingForGreeting && code === 220) {
                    // Initial server greeting received
                    console.log('Server greeting received, starting command sequence');
                    waitingForGreeting = false;
                    sendNextCommand();
                    return true;
                } else if (code === 250 && currentCommand && currentCommand.description === 'EHLO' && !useTLS) {
                    // EHLO response - check for STARTTLS capability
                    if (response.includes('STARTTLS')) {
                        console.log('Server supports STARTTLS, initiating TLS upgrade');
                        socket.write('STARTTLS\r\n');
                        return true;
                    }
                    // Fall through to success handling
                } else if (code === 220 && response.includes('TLS')) {
                    // STARTTLS successful, upgrade to TLS
                    upgradeToTLS();
                    return true;
                }

                // Generic response handling
                if (code >= 200 && code < 400) {
                    // Success response
                    commandIndex++;
                    sendNextCommand();
                    return true;
                } else {
                    // Error response
                    reject(new Error(`SMTP error: ${response}`));
                    return true;
                }
            }

            function upgradeToTLS() {
                console.log('Upgrading connection to TLS...');

                // Remove current data handler temporarily
                socket.removeListener('data', handleData);

                // Create TLS socket over existing connection
                const tlsSocket = tls.connect({
                    socket: socket,
                    host: host,
                    port: port,
                    rejectUnauthorized: false
                }, () => {
                    console.log('TLS upgrade successful');
                    socket = tlsSocket;
                    tlsUpgraded = true;
                    useTLS = true;
                    commandIndex = 0; // Reset to send EHLO again over TLS

                    // Reattach data handler
                    socket.on('data', handleData);
                    sendNextCommand();
                });

                tlsSocket.on('error', (error) => {
                    console.error('TLS upgrade error:', error);
                    reject(new Error(`SMTP TLS upgrade error: ${error.message}`));
                });
            }

            createSocket();
        });
    }

    /**
     * Make HTTPS request
     */
    makeHttpsRequest(options, data) {
        return new Promise((resolve, reject) => {
            const req = https.request(options, (res) => {
                let body = '';
                res.on('data', (chunk) => body += chunk);
                res.on('end', () => {
                    if (res.statusCode >= 200 && res.statusCode < 300) {
                        resolve(body);
                    } else {
                        reject(new Error(`HTTP ${res.statusCode}: ${body}`));
                    }
                });
            });

            req.on('error', reject);
            req.write(data);
            req.end();
        });
    }

    /**
     * Make HTTP/HTTPS request
     */
    makeRequest(protocol, options, data) {
        return new Promise((resolve, reject) => {
            const req = protocol.request(options, (res) => {
                let body = '';
                res.on('data', (chunk) => body += chunk);
                res.on('end', () => {
                    if (res.statusCode >= 200 && res.statusCode < 300) {
                        resolve(body);
                    } else {
                        reject(new Error(`HTTP ${res.statusCode}: ${body}`));
                    }
                });
            });

            req.on('error', reject);
            if (data) {
                req.write(data);
            }
            req.end();
        });
    }

    /**
     * Update notification settings when configuration changes
     */
    onConfigurationChange() {
        this.initialize();
    }

    /**
     * Clean up resources
     */
    dispose() {
        this.stopNotificationChecker();
    }
}

module.exports = { NotificationService };