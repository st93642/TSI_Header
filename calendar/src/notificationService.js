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
        this.sentNotifications = new Set(); // Track sent notifications
        this.initialize();
    }

    /**
     * Initialize the notification service
     */
    async initialize() {
        const config = vscode.workspace.getConfiguration('tsiheader.notifications');

        if (config.get('enableEmail', false)) {
            this.startNotificationChecker();
            console.log('Email notifications enabled');
        } else {
            this.stopNotificationChecker();
            console.log('Email notifications disabled');
        }
    }

    /**
     * Start the periodic notification checker
     */
    startNotificationChecker() {
        if (this.checkInterval) {
            clearInterval(this.checkInterval);
        }

        // Check every 15 minutes
        this.checkInterval = setInterval(() => {
            this.checkUpcomingEvents();
        }, 15 * 60 * 1000);

        // Initial check
        this.checkUpcomingEvents();
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

            // Get events in the notification window
            const events = await this.getUpcomingEvents(notificationTime);

            for (const event of events) {
                await this.sendNotification(event);
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

        try {
            switch (service) {
                case 'sendgrid':
                    await this.sendSendGridNotification(event, customMessage);
                    break;
                case 'mailgun':
                    await this.sendMailgunNotification(event, customMessage);
                    break;
                case 'smtp':
                    await this.sendSMTPNotification(event, customMessage);
                    break;
                case 'webhook':
                    await this.sendWebhookNotification(event, customMessage);
                    break;
                default:
                    return { success: false, error: 'No email service configured' };
            }

            // Mark notification as sent
            const notificationKey = `${event.id}-${event.start || event.date}`;
            this.sentNotifications.add(notificationKey);

            return { success: true };

        } catch (error) {
            console.error('Error sending notification:', error);
            return { success: false, error: error.message };
        }
    }

    /**
     * Send notification via SendGrid
     */
    async sendSendGridNotification(event, customMessage = null) {
        const config = vscode.workspace.getConfiguration('tsiheader.notifications');
        const apiKey = config.get('sendgridApiKey');
        const emailAddress = config.get('emailAddress');

        if (!apiKey || !emailAddress) {
            throw new Error('SendGrid API key and email address required');
        }

        const emailData = {
            personalizations: [{
                to: [{ email: emailAddress }],
                subject: `Upcoming: ${event.title}`
            }],
            from: { email: 'noreply@tsi-header.com', name: 'TSI Header Extension' },
            content: [{
                type: 'text/plain',
                value: this.formatEmailContent(event, customMessage)
            }]
        };

        const options = {
            hostname: 'api.sendgrid.com',
            path: '/v3/mail/send',
            method: 'POST',
            headers: {
                'Authorization': `Bearer ${apiKey}`,
                'Content-Type': 'application/json'
            }
        };

        await this.makeHttpsRequest(options, JSON.stringify(emailData));
    }

    /**
     * Send notification via Mailgun
     */
    async sendMailgunNotification(event, customMessage = null) {
        const config = vscode.workspace.getConfiguration('tsiheader.notifications');
        const apiKey = config.get('mailgunApiKey');
        const domain = config.get('mailgunDomain');
        const emailAddress = config.get('emailAddress');

        if (!apiKey || !domain || !emailAddress) {
            throw new Error('Mailgun API key, domain, and email address required');
        }

        const auth = Buffer.from(`api:${apiKey}`).toString('base64');
        const emailData = new URLSearchParams({
            from: `TSI Header Extension <noreply@${domain}>`,
            to: emailAddress,
            subject: `Upcoming: ${event.title}`,
            text: this.formatEmailContent(event, customMessage)
        });

        const options = {
            hostname: 'api.mailgun.net',
            path: `/v3/${domain}/messages`,
            method: 'POST',
            headers: {
                'Authorization': `Basic ${auth}`,
                'Content-Type': 'application/x-www-form-urlencoded'
            }
        };

        await this.makeHttpsRequest(options, emailData.toString());
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
        await this.sendViaSMTP(host, port, user, password, emailData);
    }

    /**
     * Send notification via webhook
     */
    async sendWebhookNotification(event, customMessage = null) {
        const config = vscode.workspace.getConfiguration('tsiheader.notifications');
        const webhookUrl = config.get('webhookUrl');

        if (!webhookUrl) {
            throw new Error('Webhook URL required');
        }

        const payload = {
            event: 'calendar_notification',
            data: event,
            customMessage: customMessage,
            timestamp: new Date().toISOString()
        };

        const url = new URL(webhookUrl);
        const options = {
            hostname: url.hostname,
            path: url.pathname + url.search,
            method: 'POST',
            headers: {
                'Content-Type': 'application/json',
                'User-Agent': 'TSI-Header-Extension/1.0'
            }
        };

        const protocol = url.protocol === 'https:' ? https : http;
        await this.makeRequest(protocol, options, JSON.stringify(payload));
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
    sendViaSMTP(host, port, user, password, emailData) {
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
                { cmd: () => `RCPT TO:<${user.includes('@') ? user : user + '@' + host}>\r\n`, description: 'RCPT TO' },
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