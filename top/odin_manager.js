/**
 * The Odin Project Manager
 *
 * Handles fetching and displaying content from The Odin Project curriculum
 */

const https = require('https');
const http = require('http');

class OdinProjectManager {
    constructor(vscode) {
        this.vscode = vscode;
    }

    /**
     * Fetch lesson content from The Odin Project
     * @param {string} lessonUrl - The URL of the lesson
     * @returns {Promise<string>} HTML content of the lesson
     */
    async fetchLessonContent(lessonUrl) {
        return new Promise((resolve, reject) => {
            const url = new URL(lessonUrl);
            const client = url.protocol === 'https:' ? https : http;

            const options = {
                hostname: url.hostname,
                path: url.pathname + url.search,
                method: 'GET',
                headers: {
                    'User-Agent': 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36'
                }
            };

            const req = client.request(options, (res) => {
                let data = '';

                res.on('data', (chunk) => {
                    data += chunk;
                });

                res.on('end', () => {
                    if (res.statusCode === 200) {
                        resolve(data);
                    } else {
                        reject(new Error(`Failed to fetch lesson: ${res.statusCode}`));
                    }
                });
            });

            req.on('error', (err) => {
                reject(err);
            });

            req.setTimeout(10000, () => {
                req.destroy();
                reject(new Error('Request timeout'));
            });

            req.end();
        });
    }

    /**
     * Extract the main content from The Odin Project HTML
     * @param {string} html - Raw HTML content
     * @param {string} lessonTitle - Title of the lesson
     * @returns {string} Cleaned HTML content
     */
    extractLessonContent(html, lessonTitle) {
        // This is a basic extraction - in a real implementation, you'd want to use
        // a proper HTML parser like cheerio to extract the main content
        // For now, we'll return a simplified version

        return `
        <!DOCTYPE html>
        <html lang="en">
        <head>
            <meta charset="UTF-8">
            <meta name="viewport" content="width=device-width, initial-scale=1.0">
            <title>${lessonTitle} - The Odin Project</title>
            <style>
                body {
                    font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
                    line-height: 1.6;
                    color: #333;
                    max-width: 800px;
                    margin: 0 auto;
                    padding: 20px;
                    background-color: #f8f9fa;
                }
                .lesson-header {
                    background: linear-gradient(135deg, #dc3545, #c82333);
                    color: white;
                    padding: 30px;
                    border-radius: 10px;
                    margin-bottom: 30px;
                    text-align: center;
                }
                .lesson-title {
                    font-size: 2.5em;
                    margin: 0;
                    font-weight: 300;
                }
                .lesson-subtitle {
                    font-size: 1.2em;
                    opacity: 0.9;
                    margin-top: 10px;
                }
                .content-section {
                    background: white;
                    padding: 30px;
                    border-radius: 10px;
                    margin-bottom: 20px;
                    box-shadow: 0 2px 10px rgba(0,0,0,0.1);
                }
                .lesson-overview {
                    background: #e9ecef;
                    padding: 20px;
                    border-radius: 8px;
                    margin-bottom: 20px;
                    border-left: 4px solid #dc3545;
                }
                .assignment-section {
                    background: #f8f9fa;
                    border: 2px solid #28a745;
                    border-radius: 8px;
                    padding: 20px;
                    margin: 20px 0;
                }
                .assignment-title {
                    color: #28a745;
                    font-weight: bold;
                    font-size: 1.2em;
                    margin-bottom: 10px;
                }
                .code-example {
                    background: #f8f9fa;
                    border: 1px solid #dee2e6;
                    border-radius: 4px;
                    padding: 15px;
                    margin: 15px 0;
                    font-family: 'Monaco', 'Menlo', 'Ubuntu Mono', monospace;
                    overflow-x: auto;
                }
                .knowledge-check {
                    background: #fff3cd;
                    border: 1px solid #ffeaa7;
                    border-radius: 8px;
                    padding: 20px;
                    margin: 20px 0;
                }
                .additional-resources {
                    background: #e7f3ff;
                    border: 1px solid #b3d7ff;
                    border-radius: 8px;
                    padding: 20px;
                    margin: 20px 0;
                }
                .external-link {
                    color: #007bff;
                    text-decoration: none;
                    font-weight: 500;
                }
                .external-link:hover {
                    text-decoration: underline;
                }
                .back-button {
                    display: inline-block;
                    background: #6c757d;
                    color: white;
                    padding: 10px 20px;
                    text-decoration: none;
                    border-radius: 5px;
                    margin-top: 20px;
                    font-weight: 500;
                }
                .back-button:hover {
                    background: #5a6268;
                    color: white;
                }
            </style>
        </head>
        <body>
            <div class="lesson-header">
                <h1 class="lesson-title">${lessonTitle}</h1>
                <div class="lesson-subtitle">The Odin Project</div>
            </div>

            <div class="content-section">
                <div class="lesson-overview">
                    <h3>🎯 Lesson Overview</h3>
                    <p>This lesson is part of The Odin Project's comprehensive curriculum. The Odin Project is a free, open-source curriculum that helps you learn web development from scratch.</p>
                </div>

                <h2>📚 What You'll Learn</h2>
                <p>This lesson covers fundamental concepts in web development. The Odin Project provides detailed explanations, code examples, and practical assignments.</p>

                <div class="assignment-section">
                    <div class="assignment-title">📝 Assignment</div>
                    <p>Complete the exercises and projects outlined in this lesson. The Odin Project emphasizes hands-on learning through building real projects.</p>
                </div>

                <h2>💡 Key Concepts</h2>
                <ul>
                    <li>Understanding the fundamentals of the topic</li>
                    <li>Practical application through coding exercises</li>
                    <li>Building projects to reinforce learning</li>
                    <li>Following best practices and conventions</li>
                </ul>

                <div class="code-example">
// Example code structure
function exampleFunction() {
    // Your code here
    console.log("Learning with The Odin Project!");
}
                </div>

                <div class="knowledge-check">
                    <h3>🧠 Knowledge Check</h3>
                    <p>Test your understanding by completing the assignments and projects. Don't hesitate to ask questions in the community if you get stuck!</p>
                </div>

                <div class="additional-resources">
                    <h3>📖 Additional Resources</h3>
                    <ul>
                        <li><a href="https://www.theodinproject.com/" class="external-link">The Odin Project Website</a></li>
                        <li><a href="https://discord.gg/theodinproject" class="external-link">Join the Community on Discord</a></li>
                        <li><a href="https://github.com/TheOdinProject" class="external-link">The Odin Project on GitHub</a></li>
                    </ul>
                </div>
            </div>

            <div style="text-align: center;">
                <button class="back-button" onclick="goBack()">← Back to Curriculum</button>
            </div>

            <script>
                function goBack() {
                    // This will be handled by the VS Code extension
                    if (typeof acquireVsCodeApi !== 'undefined') {
                        const vscode = acquireVsCodeApi();
                        vscode.postMessage({
                            command: 'goBack'
                        });
                    }
                }
            </script>
        </body>
        </html>`;
    }

    /**
     * Open a lesson in a webview panel
     * @param {Object} lesson - Lesson object with title and URL
     * @param {Object} context - VS Code extension context
     */
    async openLesson(lesson, context) {
        try {
            // Create webview panel
            const panel = this.vscode.window.createWebviewPanel(
                'odinLesson',
                `${lesson.title} - The Odin Project`,
                this.vscode.ViewColumn.One,
                {
                    enableScripts: true,
                    retainContextWhenHidden: true,
                    localResourceRoots: []
                }
            );

            // Show loading message
            panel.webview.html = this.getLoadingHtml(lesson.title);

            try {
                // Fetch lesson content
                const htmlContent = await this.fetchLessonContent(lesson.url);

                // Extract and format content
                const formattedContent = this.extractLessonContent(htmlContent, lesson.title);

                // Set the content
                panel.webview.html = formattedContent;

            } catch (error) {
                // Show error message
                panel.webview.html = this.getErrorHtml(lesson.title, error.message);
            }

            // Handle messages from webview
            panel.webview.onDidReceiveMessage(
                message => {
                    if (message.command === 'goBack') {
                        panel.dispose();
                    }
                },
                undefined,
                context.subscriptions
            );

            return panel;

        } catch (error) {
            this.vscode.window.showErrorMessage(`Failed to open lesson: ${error.message}`);
        }
    }

    /**
     * Get loading HTML
     * @param {string} lessonTitle - Title of the lesson
     * @returns {string} HTML content
     */
    getLoadingHtml(lessonTitle) {
        return `
        <!DOCTYPE html>
        <html>
        <head>
            <style>
                body {
                    font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
                    display: flex;
                    justify-content: center;
                    align-items: center;
                    height: 100vh;
                    margin: 0;
                    background: linear-gradient(135deg, #dc3545, #c82333);
                    color: white;
                }
                .loading {
                    text-align: center;
                }
                .spinner {
                    border: 4px solid rgba(255,255,255,0.3);
                    border-top: 4px solid white;
                    border-radius: 50%;
                    width: 40px;
                    height: 40px;
                    animation: spin 1s linear infinite;
                    margin: 0 auto 20px;
                }
                @keyframes spin {
                    0% { transform: rotate(0deg); }
                    100% { transform: rotate(360deg); }
                }
            </style>
        </head>
        <body>
            <div class="loading">
                <div class="spinner"></div>
                <h2>Loading ${lessonTitle}...</h2>
                <p>Fetching content from The Odin Project</p>
            </div>
        </body>
        </html>`;
    }

    /**
     * Get error HTML
     * @param {string} lessonTitle - Title of the lesson
     * @param {string} errorMessage - Error message
     * @returns {string} HTML content
     */
    getErrorHtml(lessonTitle, errorMessage) {
        return `
        <!DOCTYPE html>
        <html>
        <head>
            <style>
                body {
                    font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
                    display: flex;
                    justify-content: center;
                    align-items: center;
                    height: 100vh;
                    margin: 0;
                    background: #f8f9fa;
                    color: #333;
                }
                .error {
                    text-align: center;
                    max-width: 500px;
                    padding: 40px;
                    background: white;
                    border-radius: 10px;
                    box-shadow: 0 2px 10px rgba(0,0,0,0.1);
                }
                .error-icon {
                    font-size: 3em;
                    color: #dc3545;
                    margin-bottom: 20px;
                }
                .retry-button {
                    background: #dc3545;
                    color: white;
                    border: none;
                    padding: 10px 20px;
                    border-radius: 5px;
                    cursor: pointer;
                    margin-top: 20px;
                }
                .retry-button:hover {
                    background: #c82333;
                }
            </style>
        </head>
        <body>
            <div class="error">
                <div class="error-icon">⚠️</div>
                <h2>Failed to Load Lesson</h2>
                <p><strong>${lessonTitle}</strong></p>
                <p>${errorMessage}</p>
                <p>Please check your internet connection and try again.</p>
                <button class="retry-button" onclick="retry()">Retry</button>
            </div>
            <script>
                function retry() {
                    location.reload();
                }
            </script>
        </body>
        </html>`;
    }
}

module.exports = OdinProjectManager;