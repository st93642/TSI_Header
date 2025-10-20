/**
 * Git Book Manager
 *
 * Handles fetching and displaying content from the Git book (git-scm.com/book)
 */

const https = require('https');
const http = require('http');
const path = require('path');
const fs = require('fs').promises;
const crypto = require('crypto');

class GitBookManager {
    constructor(vscode, progressTracker = null) {
        this.vscode = vscode;
        this.progressTracker = progressTracker;
        this.cacheDir = null;
        this.baseUrl = 'https://git-scm.com/book/en/v2';
    }

    /**
     * Get the cache directory path for Git book lessons
     * @returns {Promise<string>} Path to cache directory
     */
    async getCacheDir() {
        if (this.cacheDir) return this.cacheDir;

        try {
            // Use VS Code's global storage path for caching
            const extensionId = 'st93642.uni-header';
            const globalStoragePath = this.vscode.env.globalStorageUri;

            if (globalStoragePath) {
                this.cacheDir = this.vscode.Uri.joinPath(globalStoragePath, 'git-book-cache').fsPath;
            } else {
                // Fallback to workspace directory if global storage not available
                const workspaceFolder = this.vscode.workspace.workspaceFolders?.[0];
                if (workspaceFolder) {
                    this.cacheDir = this.vscode.Uri.joinPath(workspaceFolder.uri, '.vscode', 'git-book-cache').fsPath;
                } else {
                    // Last resort: use temp directory
                    const os = require('os');
                    const path = require('path');
                    this.cacheDir = path.join(os.tmpdir(), 'vscode-git-book-cache');
                }
            }

            // Ensure cache directory exists
            await fs.mkdir(this.cacheDir, { recursive: true });
            return this.cacheDir;
        } catch (error) {
            console.error('Failed to create cache directory:', error);
            return null;
        }
    }

    /**
     * Generate a cache key for a lesson URL
     * @param {string} url - Lesson URL
     * @returns {string} Cache key
     */
    getCacheKey(url) {
        return crypto.createHash('md5').update(url).digest('hex');
    }

    /**
     * Get cache file path for a lesson
     * @param {string} url - Lesson URL
     * @returns {Promise<string>} Path to cache file
     */
    async getCacheFilePath(url) {
        const cacheDir = await this.getCacheDir();
        if (!cacheDir) return null;

        const cacheKey = this.getCacheKey(url);
        return path.join(cacheDir, `${cacheKey}.json`);
    }

    /**
     * Save lesson content to cache
     * @param {string} url - Lesson URL
     * @param {string} title - Lesson title
     * @param {string} htmlContent - Raw HTML content
     * @param {string} formattedContent - Formatted HTML content
     * @returns {Promise<boolean>} Success status
     */
    async saveLessonToCache(url, title, htmlContent, formattedContent) {
        try {
            const cacheFilePath = await this.getCacheFilePath(url);
            if (!cacheFilePath) return false;

            // Ensure all content is strings
            const cacheData = {
                url: String(url || ''),
                title: String(title || ''),
                htmlContent: String(htmlContent || ''),
                formattedContent: String(formattedContent || ''),
                cachedAt: new Date().toISOString(),
                version: '1.0'
            };

            await fs.writeFile(cacheFilePath, JSON.stringify(cacheData, null, 2), 'utf8');
            return true;
        } catch (error) {
            console.error('Failed to save lesson to cache:', error);
            return false;
        }
    }

    /**
     * Load lesson content from cache
     * @param {string} url - Lesson URL
     * @param {number} maxAgeHours - Maximum age of cache in hours (default: 24)
     * @returns {Promise<Object|null>} Cached lesson data or null if not found/expired
     */
    async loadLessonFromCache(url, maxAgeHours = 24) {
        try {
            const cacheFilePath = await this.getCacheFilePath(url);
            if (!cacheFilePath) return null;

            const cacheData = JSON.parse(await fs.readFile(cacheFilePath, 'utf8'));

            // Check if cache is expired
            const cachedAt = new Date(cacheData.cachedAt);
            const now = new Date();
            const ageHours = (now - cachedAt) / (1000 * 60 * 60);

            if (ageHours > maxAgeHours) {
                // Cache is too old, remove it
                await fs.unlink(cacheFilePath).catch(() => {});
                return null;
            }

            return cacheData;
        } catch (error) {
            // Cache file doesn't exist or is corrupted
            return null;
        }
    }

    /**
     * Clear all cached lessons
     * @returns {Promise<boolean>} Success status
     */
    async clearLessonCache() {
        try {
            const cacheDir = await this.getCacheDir();
            if (!cacheDir) return false;

            const files = await fs.readdir(cacheDir);
            await Promise.all(
                files
                    .filter(file => file.endsWith('.json'))
                    .map(file => fs.unlink(path.join(cacheDir, file)))
            );

            return true;
        } catch (error) {
            console.error('Failed to clear lesson cache:', error);
            return false;
        }
    }

    /**
     * Get cache statistics
     * @returns {Promise<Object>} Cache statistics
     */
    async getCacheStats() {
        try {
            const cacheDir = await this.getCacheDir();
            if (!cacheDir) return { totalLessons: 0, totalSize: 0, oldestCache: null, newestCache: null };

            const files = await fs.readdir(cacheDir);
            const cacheFiles = files.filter(file => file.endsWith('.json'));

            let totalSize = 0;
            let oldestCache = null;
            let newestCache = null;

            for (const file of cacheFiles) {
                const filePath = path.join(cacheDir, file);
                const stats = await fs.stat(filePath);
                totalSize += stats.size;

                const cacheData = JSON.parse(await fs.readFile(filePath, 'utf8'));
                const cachedAt = new Date(cacheData.cachedAt);

                if (!oldestCache || cachedAt < oldestCache) oldestCache = cachedAt;
                if (!newestCache || cachedAt > newestCache) newestCache = cachedAt;
            }

            return {
                totalLessons: cacheFiles.length,
                totalSize: totalSize,
                oldestCache: oldestCache ? oldestCache.toISOString() : null,
                newestCache: newestCache ? newestCache.toISOString() : null
            };
        } catch (error) {
            console.error('Failed to get cache stats:', error);
            return { totalLessons: 0, totalSize: 0, oldestCache: null, newestCache: null };
        }
    }

    /**
     * Fetch lesson content from Git book
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
                    'User-Agent': 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36',
                    'Accept': 'text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.7',
                    'Accept-Language': 'en-US,en;q=0.9',
                    'Accept-Encoding': 'gzip, deflate, br',
                    'DNT': '1',
                    'Connection': 'keep-alive',
                    'Upgrade-Insecure-Requests': '1',
                    'Sec-Fetch-Dest': 'document',
                    'Sec-Fetch-Mode': 'navigate',
                    'Sec-Fetch-Site': 'none',
                    'Sec-Fetch-User': '?1',
                    'Cache-Control': 'max-age=0'
                }
            };

            const req = client.request(options, (res) => {
                let data = '';

                // Handle gzip compression
                if (res.headers['content-encoding'] === 'gzip') {
                    const zlib = require('zlib');
                    const gunzip = zlib.createGunzip();
                    res.pipe(gunzip);

                    gunzip.on('data', (chunk) => {
                        data += chunk;
                    });

                    gunzip.on('end', () => {
                        if (res.statusCode === 200) {
                            resolve(data);
                        } else {
                            reject(new Error(`Failed to fetch lesson: ${res.statusCode}`));
                        }
                    });
                } else {
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
                }
            });

            req.on('error', (err) => {
                reject(err);
            });

            req.setTimeout(15000, () => {
                req.destroy();
                reject(new Error('Request timeout - Git book may be temporarily unavailable'));
            });

            req.end();
        });
    }

    /**
     * Fetch lesson content with retry logic
     * @param {string} lessonUrl - The URL of the lesson
     * @param {number} maxRetries - Maximum number of retry attempts
     * @returns {Promise<string>} HTML content of the lesson
     */
    async fetchLessonContentWithRetry(lessonUrl, maxRetries = 3) {
        let lastError;

        for (let attempt = 1; attempt <= maxRetries; attempt++) {
            try {
                return await this.fetchLessonContent(lessonUrl);
            } catch (error) {
                lastError = error;

                // If it's a 403, don't retry as it's likely a permanent block
                if (error.message.includes('403')) {
                    throw new Error('Access blocked by Git book. This may be due to rate limiting or bot detection. Please try again later or visit the website directly.');
                }

                // If it's the last attempt, throw the error
                if (attempt === maxRetries) {
                    break;
                }

                // Wait before retrying (exponential backoff)
                const delay = Math.min(1000 * Math.pow(2, attempt - 1), 5000);
                await new Promise(resolve => setTimeout(resolve, delay));
            }
        }

        throw lastError;
    }

    /**
     * Extract the main content from Git book lesson HTML
     * @param {string} html - Raw HTML content
     * @param {string} lessonTitle - Title of the lesson
     * @param {Object|null} nextLesson - Next lesson object or null
     * @param {Object} webview - VS Code webview object for URI conversion
     * @returns {string} Cleaned HTML content
     */
    async extractLessonContent(html, lessonTitle, nextLesson, webview) {
        try {
            // Extract lesson content using Git book's structure
            let content = '';

            // First, try to extract only the main content div
            const mainDivRegex = /<div[^>]*id="main"[^>]*>([\s\S]*)<\/div>/i;
            let match = mainDivRegex.exec(html);
            if (match) {
                content = match[1];
            } else {
                // Fallback: Try to find the main content div - Git book uses different structure than Odin
                const mainContentRegex = /<div[^>]*class="[^"]*chapter[^"]*"[^>]*>([\s\S]*?)<\/div>\s*<div[^>]*class="[^"]*footer[^"]*"/i;
                match = mainContentRegex.exec(html);
                if (match) {
                    content = match[1];
                } else {
                    // Fallback to broader content extraction
                    const broadRegex = /<main[^>]*>(.*?)<\/main>/gis;
                    const broadMatch = broadRegex.exec(html);
                    if (broadMatch) {
                        content = broadMatch[1];
                    } else {
                        // Try to find content within article or section tags
                        const articleRegex = /<article[^>]*>(.*?)<\/article>/gis;
                        const articleMatch = articleRegex.exec(html);
                        if (articleMatch) {
                            content = articleMatch[1];
                        }
                    }
                }
            }

            // If no content found, try a different approach
            if (!content) {
                // Look for content within specific Git book classes
                const contentRegex = /<div[^>]*class="[^"]*sect1[^"]*"[^>]*>(.*?)<\/div>/gis;
                let contentMatch;
                while ((contentMatch = contentRegex.exec(html)) !== null) {
                    content += contentMatch[1];
                }
            }

            // If still no content, try to extract from body
            if (!content) {
                const bodyRegex = /<body[^>]*>(.*?)<\/body>/gis;
                const bodyMatch = bodyRegex.exec(html);
                if (bodyMatch) {
                    content = bodyMatch[1];
                }
            }

            if (!content) {
                // Fallback to generic content
                return this.getGenericLessonContent(lessonTitle);
            }

            // Process links and images to work within VS Code webview
            content = await this.processContentForWebview(content, webview);

            // If we have substantial content, format it
            if (content.length > 200) {
                const result = this.formatLessonContent(content, lessonTitle, nextLesson, webview);
                // Ensure we always return a string
                return typeof result === 'string' ? result : this.getGenericLessonContent(lessonTitle);
            } else {
                // Fallback to generic content
                return this.getGenericLessonContent(lessonTitle);
            }

        } catch (error) {
            console.error('Error extracting lesson content:', error);
            return this.getGenericLessonContent(lessonTitle);
        }
    }

    /**
     * Format extracted lesson content into a clean HTML page
     * @param {string} content - Extracted HTML content
     * @param {string} lessonTitle - Title of the lesson
     * @param {Object|null} nextLesson - Next lesson object or null
     * @param {Object} webview - VS Code webview object for URI conversion
     * @returns {string} Formatted HTML page
     */
    formatLessonContent(content, lessonTitle, nextLesson, webview) {
        // Ensure content is a string
        if (typeof content !== 'string') {
            console.error('formatLessonContent received non-string content:', typeof content, content);
            content = String(content || '');
        }

        // Prepare URIs for highlight.js assets
        let highlightCssUri = 'https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.8.0/styles/github-dark.min.css';
        let highlightJsUri = 'https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.8.0/highlight.min.js';
        if (webview && this.vscode && this.vscode.Uri) {
            try {
                // Use absolute paths (relative to this module) so resolveResourceUri can create correct webview URIs
                const cssAbs = path.join(__dirname, '..', '..', 'resources', 'highlightjs', 'github-dark.min.css');
                const jsAbs = path.join(__dirname, '..', '..', 'resources', 'highlightjs', 'highlight.min.js');
                highlightCssUri = webview.asWebviewUri(this.vscode.Uri.file(cssAbs));
                highlightJsUri = webview.asWebviewUri(this.vscode.Uri.file(jsAbs));
            } catch (e) {
                // fallback to CDN
            }
        }

        // Build HTML using string concatenation to avoid JSX linting issues
        let html = '<!DOCTYPE html>';
        html += '<html lang="en">';
        html += '<head>';
        html += '<meta charset="UTF-8">';
        html += '<meta name="viewport" content="width=device-width, initial-scale=1.0">';
        html += '<title>' + lessonTitle + ' - Git Book</title>';
        html += '<link rel="stylesheet" href="' + highlightCssUri + '">';
        html += '<style>';
        html += ':root{--bg-color:#f8f9fa;--text-color:#333;--card-bg:white;--card-shadow:rgba(0,0,0,0.1);--border-color:#dee2e6;--accent-bg:#f8f9fa;--accent-border:#6c757d;--link-color:#0366d6;--button-bg:#f6f8fa;--button-hover:#f3f4f6;--header-bg:linear-gradient(135deg,#f05133,#f05133)}';
        html += '@media(prefers-color-scheme:dark){:root{--bg-color:#0d1117;--text-color:#c9d1d9;--card-bg:#161b22;--card-shadow:rgba(0,0,0,0.3);--border-color:#30363d;--accent-bg:#21262d;--accent-border:#8b949e;--link-color:#58a6ff;--button-bg:#21262d;--button-hover:#30363d;--header-bg:linear-gradient(135deg,#f05133,#f05133)}}';

        // Base styles
        html += 'body{font-family:-apple-system,BlinkMacSystemFont,"Segoe UI",Roboto,Helvetica,Arial,sans-serif;line-height:1.6;color:var(--text-color);background-color:var(--bg-color);margin:0;padding:0;font-size:16px}';

        // Header styles
        html += '.lesson-header{background:var(--header-bg);color:white;padding:20px 15px;border-radius:0;margin-bottom:20px;text-align:center}';
        html += '.lesson-title{font-size:2.2em;margin:0;font-weight:300}';
        html += '.lesson-subtitle{font-size:1.3em;opacity:0.9;margin-top:10px}';

        // Content container styles
        html += '.page-container{max-width:1200px;margin:0 auto;padding:0 15px}';
        html += '.lesson-content{max-width:900px;margin:0 auto;padding:10px 0}';
        html += '.content-section{background:var(--card-bg);padding:20px;border-radius:10px;margin-bottom:10px;box-shadow:0 2px 10px var(--card-shadow);border:1px solid var(--border-color)}';

        // Typography styles
        html += '.lesson-content h1{color:var(--text-color);margin-top:40px;margin-bottom:20px;font-weight:700;font-size:2.2em;border-bottom:2px solid var(--accent-border);padding-bottom:10px}';
        html += '.lesson-content h2{color:var(--text-color);margin-top:35px;margin-bottom:18px;font-weight:600;font-size:1.9em;border-bottom:1px solid var(--accent-border);padding-bottom:8px}';
        html += '.lesson-content h3{color:var(--text-color);margin-top:30px;margin-bottom:15px;font-weight:600;font-size:1.6em}';
        html += '.lesson-content h4{color:var(--text-color);margin-top:25px;margin-bottom:12px;font-weight:600;font-size:1.4em}';
        html += '.lesson-content h5{color:var(--text-color);margin-top:20px;margin-bottom:10px;font-weight:600;font-size:1.2em}';
        html += '.lesson-content h6{color:var(--text-color);margin-top:18px;margin-bottom:8px;font-weight:600;font-size:1.1em}';
        html += '.lesson-content p{margin-bottom:15px;color:var(--text-color);font-size:1em;line-height:1.7}';
        html += '.lesson-content ul,.lesson-content ol{margin-bottom:15px;padding-left:30px}';
        html += '.lesson-content li{margin-bottom:5px;color:var(--text-color);font-size:1em;line-height:1.6}';
        html += '.lesson-content blockquote{border-left:4px solid var(--accent-border);padding-left:20px;margin:20px 0;font-style:italic;background:var(--accent-bg);padding:15px 20px;border-radius:5px;font-size:1em}';
        html += '.lesson-content pre{background:var(--accent-bg);border:1px solid var(--border-color);border-radius:6px;padding:15px;margin:15px 0;overflow-x:auto;font-family:"SFMono-Regular","Monaco","Inconsolata","Roboto Mono",monospace;font-size:14px;color:var(--text-color)}';
        html += '.lesson-content code{background:var(--accent-bg);padding:2px 6px;border-radius:3px;font-family:"SFMono-Regular","Monaco","Inconsolata","Roboto Mono",monospace;font-size:0.9em;color:var(--text-color)}';
        html += '.lesson-content a{color:var(--link-color);text-decoration:none}';
        html += '.lesson-content a:hover{text-decoration:underline}';

        // Button styles
        html += '.back-button{display:inline-block;background:var(--button-bg);color:var(--text-color);padding:12px 24px;text-decoration:none;border-radius:5px;margin-top:10px;font-weight:500;border:1px solid var(--border-color);cursor:pointer;font-size:16px}';
        html += '.back-button:hover{background:var(--button-hover);color:var(--text-color)}';
        html += '.next-button{display:inline-block;background:#f05133;color:white;padding:12px 24px;text-decoration:none;border-radius:5px;margin-top:10px;font-weight:500;border:none;cursor:pointer;font-size:16px}';
        html += '.next-button:hover{background:#d73a49;color:white}';

        // Responsive design
        html += '@media(max-width:768px){';
        html += '.lesson-header{padding:15px 10px}';
        html += '.lesson-title{font-size:1.8em}';
        html += '.content-section{padding:15px}';
        html += '.lesson-content{max-width:none;padding:10px 0}';
        html += '.page-container{padding:0 10px}';
        html += '}';
        html += '</style>';
        html += '<script src="' + highlightJsUri + '"></script>';
        html += '</head>';
        html += '<body>';
        html += '<div class="lesson-header">';
        html += '<h1 class="lesson-title">' + lessonTitle + '</h1>';
        html += '<div class="lesson-subtitle">Pro Git Book</div>';
        html += '</div>';
        html += '<div class="page-container">';
        html += '<div class="lesson-content">';
        html += '<div class="content-section">';
        html += content;
        html += '</div>';
        html += '<div style="text-align:center;margin-top:10px">';
        if (nextLesson) {
            html += '<button class="next-button" onclick="nextLesson()">Next Lesson: ' + nextLesson.title + ' →</button>';
            html += '<br><br>';
        }
        html += '<button class="back-button" onclick="goBack()">← Back to Curriculum</button>';
        html += '</div>';
        html += '</div>';
        html += '</div>';
        html += '<script>';
        html += 'function goBack(){if(typeof acquireVsCodeApi!=="undefined"){const vscode=acquireVsCodeApi();vscode.postMessage({command:"goBack"})}}';
        html += 'function nextLesson(){if(typeof acquireVsCodeApi!=="undefined"){const vscode=acquireVsCodeApi();vscode.postMessage({command:"nextLesson"})}}';
        html += 'function openLesson(lessonId){if(typeof acquireVsCodeApi!=="undefined"){const vscode=acquireVsCodeApi();vscode.postMessage({command:"openLesson", lessonId: lessonId})}}';
        html += '// Invoke highlight.js when the lesson DOM is ready';
        html += 'document.addEventListener("DOMContentLoaded", () => {';
        html += 'try {';
        html += 'if (window.hljs && typeof hljs.highlightAll === "function") {';
        html += 'hljs.highlightAll();';
        html += '}';
        html += '} catch (e) {';
        html += '// ignore';
        html += '}';
        html += '});';
        html += '</script>';
        html += '</body>';
        html += '</html>';

        return html;
    }

    /**
     * Get generic lesson content when extraction fails
     * @param {string} lessonTitle - Title of the lesson
     * @returns {string} Generic HTML content
     */
    getGenericLessonContent(lessonTitle) {
        const html = '<div style="text-align: center; padding: 50px;">' +
            '<h2>' + lessonTitle + '</h2>' +
            '<p>This lesson content could not be loaded at this time.</p>' +
            '<p>Please check your internet connection and try again.</p>' +
            '<p>You can also visit <a href="https://git-scm.com/book/en/v2" target="_blank">the Git book website</a> directly.</p>' +
        '</div>';
        return html;
    }

    /**
     * Open a lesson in a webview panel
     * @param {Object} lesson - Lesson object with title and URL
     * @param {Object} context - VS Code extension context
     */
    async openLesson(lesson, context) {
        try {
            // Track lesson view for progress
            if (this.progressTracker && lesson.id) {
                try {
                    await this.progressTracker.markLessonComplete('git', lesson.id);
                } catch (progressError) {
                    console.log('Progress tracking failed:', progressError.message);
                    // Don't fail the lesson opening if progress tracking fails
                }
            }

            // Find the next lesson in the curriculum
            const nextLesson = await this.findNextLesson(lesson.id);

            // Create webview panel
            const panel = this.vscode.window.createWebviewPanel(
                'gitBookLesson',
                `${lesson.title} - Git Book`,
                this.vscode.ViewColumn.One,
                {
                    enableScripts: true,
                    retainContextWhenHidden: true,
                    localResourceRoots: [
                        this.vscode.Uri.file(path.join(__dirname, '..', 'resources')),
                        // Allow access to cached images
                        ...(await this.getImageCacheUri() ? [await this.getImageCacheUri()] : [])
                    ]
                }
            );

            // Show loading message
            panel.webview.html = this.getLoadingHtml(lesson.title);

            try {
                let formattedContent;
                let htmlContent;

                // Try to load from cache first
                const cachedLesson = await this.loadLessonFromCache(lesson.url);

                if (cachedLesson) {
                    // Use cached content
                    formattedContent = cachedLesson.formattedContent;
                    htmlContent = cachedLesson.htmlContent;

                    // Ensure formattedContent is a string
                    if (typeof formattedContent !== 'string') {
                        console.error('Cached formattedContent is not a string:', typeof formattedContent, formattedContent);
                        formattedContent = String(formattedContent || '');
                    }

                    // Update loading message to show it's from cache
                    panel.webview.html = this.getLoadingHtml(`${lesson.title} (Offline)`, true);
                    await new Promise(resolve => setTimeout(resolve, 500)); // Brief pause to show offline message
                } else {
                    // Fetch lesson content with retry logic
                    htmlContent = await this.fetchLessonContentWithRetry(lesson.url);

                    // Extract and format content
                    formattedContent = await this.extractLessonContent(htmlContent, lesson.title, nextLesson, panel.webview);

                    // Save to cache for future use
                    await this.saveLessonToCache(lesson.url, lesson.title, htmlContent, formattedContent);
                }

                // Set the content
                panel.webview.html = formattedContent;

            } catch (error) {
                // Try to load from cache as fallback
                const cachedLesson = await this.loadLessonFromCache(lesson.url, 7 * 24); // Allow up to 7 days old cache as fallback

                if (cachedLesson) {
                    // Show cached content with a warning
                    panel.webview.html = this.getOfflineFallbackHtml(cachedLesson.formattedContent, lesson.title, error.message);
                } else {
                    // Show error message
                    panel.webview.html = this.getErrorHtml(lesson.title, error.message, lesson.url);
                }
            }

            // Handle messages from webview
            panel.webview.onDidReceiveMessage(
                message => {
                    if (message.command === 'goBack') {
                        panel.dispose();
                        // Open the curriculum tree view
                        this.vscode.commands.executeCommand('tsiheader.learnGit');
                    } else if (message.command === 'nextLesson' && nextLesson) {
                        panel.dispose();
                        // Open the next lesson
                        this.openLesson(nextLesson, context);
                    } else if (message.command === 'openLesson' && message.lessonId) {
                        panel.dispose();
                        // Find and open the specified lesson
                        this.openLessonById(message.lessonId, context);
                    } else if (message.command === 'browseCurriculum') {
                        // Open the curriculum tree view
                        this.vscode.commands.executeCommand('tsiheader.learnGit');
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
     * Open a lesson by its ID
     * @param {string} lessonId - Lesson ID to open
     * @param {Object} context - VS Code extension context
     */
    async openLessonById(lessonId, context) {
        try {
            const curriculum = await this.getCurriculum();

            // Search for the lesson by ID
            for (const chapter of curriculum.chapters) {
                if (chapter.lessons) {
                    for (const lesson of chapter.lessons) {
                        if (lesson.id === lessonId) {
                            await this.openLesson(lesson, context);
                            return;
                        }
                    }
                }
            }

            // Lesson not found
            this.vscode.window.showErrorMessage(`Lesson with ID '${lessonId}' not found in curriculum.`);
        } catch (error) {
            this.vscode.window.showErrorMessage(`Failed to open lesson: ${error.message}`);
        }
    }

    /**
     * Get offline fallback HTML when network fails but cached content exists
     * @param {string} cachedContent - Cached formatted content
     * @param {string} lessonTitle - Lesson title
     * @param {string} errorMessage - Original error message
     * @returns {string} HTML content with offline warning
     */
    getOfflineFallbackHtml(cachedContent, lessonTitle, errorMessage) {
        // Ensure cachedContent is a string
        if (typeof cachedContent !== 'string') {
            console.error('getOfflineFallbackHtml received non-string cachedContent:', typeof cachedContent, cachedContent);
            cachedContent = String(cachedContent || '');
        }

        // Insert offline warning at the top of the cached content
        const offlineWarning = `
        <div style="background: #fff3cd; border: 1px solid #ffeaa7; border-radius: 5px; padding: 15px; margin-bottom: 20px; color: #856404;">
            <strong>⚠️ Offline Mode</strong><br>
            This lesson is loaded from cache because the latest version couldn't be fetched from Git book.<br>
            <small>Error: ${errorMessage}</small>
        </div>
        `;

        // Insert the warning after the header but before the content
        return cachedContent.replace(
            /(<div class="page-container">)/,
            offlineWarning + '$1'
        );
    }

    /**
     * Get loading HTML
     * @param {string} lessonTitle - Title of the lesson
     * @param {boolean} fromCache - Whether content is loaded from cache
     * @returns {string} HTML content
     */
    getLoadingHtml(lessonTitle, fromCache = false) {
        return `
        <!DOCTYPE html>
        <html>
        <head>
            <style>
                :root {
                    --loading-bg: linear-gradient(135deg, #f05133, #f05133);
                    --loading-text: white;
                }

                @media (prefers-color-scheme: dark) {
                    :root {
                        --loading-bg: linear-gradient(135deg, #f05133, #f05133);
                        --loading-text: #cccccc;
                    }
                }

                body {
                    font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
                    display: flex;
                    justify-content: center;
                    align-items: center;
                    height: 100vh;
                    margin: 0;
                    background: var(--loading-bg);
                    color: var(--loading-text);
                }
                .loading {
                    text-align: center;
                }
                .spinner {
                    border: 4px solid rgba(255,255,255,0.3);
                    border-top: 4px solid var(--loading-text);
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
                <p>${fromCache ? 'Loading from cache...' : 'Fetching content from Git book'}</p>
            </div>
        </body>
        </html>`;
    }

    /**
     * Find the next lesson in the curriculum after the given lesson ID
     * @param {string} currentLessonId - Current lesson ID
     * @returns {Promise<Object|null>} Next lesson object or null if none found
     */
    async findNextLesson(currentLessonId) {
        try {
            const fs = require('fs');
            const path = require('path');
            const curriculumPath = path.join(__dirname, 'curriculum.json');
            const curriculum = JSON.parse(fs.readFileSync(curriculumPath, 'utf8'));

            let foundCurrent = false;

            // Search through all chapters
            for (const chapter of curriculum.chapters) {
                if (chapter.lessons && chapter.lessons.length > 0) {
                    for (let i = 0; i < chapter.lessons.length; i++) {
                        const lesson = chapter.lessons[i];
                        if (foundCurrent) {
                            // Return the next lesson
                            return lesson;
                        }
                        if (lesson.id === currentLessonId) {
                            foundCurrent = true;
                        }
                    }
                }
            }

            return null; // No next lesson found
        } catch (error) {
            console.error('Error finding next lesson:', error);
            return null;
        }
    }

    async getProgressStats() {
        if (!this.progressTracker) {
            return {
                lessonsCompleted: 0,
                currentStreak: 0,
                totalStudyTime: 0,
                achievements: 0,
                lastStudyDate: 'Never',
                completed: []
            };
        }

        try {
            const progress = await this.progressTracker.getProgress('git');
            const stats = await this.progressTracker.getStats('git');

            return {
                lessonsCompleted: stats.lessonsCompleted,
                currentStreak: stats.currentStreak,
                totalStudyTime: stats.totalStudyTime,
                achievements: stats.achievements,
                lastStudyDate: stats.lastStudyDate,
                completed: progress.completed || []
            };
        } catch (error) {
            console.log('Failed to get Git progress stats:', error.message);
            return {
                lessonsCompleted: 0,
                currentStreak: 0,
                totalStudyTime: 0,
                achievements: 0,
                lastStudyDate: 'Never',
                completed: []
            };
        }
    }

    /**
     * Record study time for Git book
     * @param {number} minutes - Minutes studied
     */
    async recordStudyTime(minutes) {
        if (this.progressTracker) {
            try {
                await this.progressTracker.recordStudyTime('git', minutes);
            } catch (error) {
                console.log('Failed to record study time:', error.message);
            }
        }
    }

    getErrorHtml(lessonTitle, errorMessage, lessonUrl) {
        const isBlocked = errorMessage.includes('403') || errorMessage.includes('blocked') || errorMessage.includes('Access blocked');
        const isNotFound = errorMessage.includes('404') || errorMessage.includes('Not Found') || errorMessage.includes('ENOTFOUND');
        const displayMessage = isBlocked
            ? 'Access temporarily blocked by Git book. This is likely due to rate limiting or bot detection protection.'
            : isNotFound
            ? 'The lesson content could not be found. The book structure may have been updated.'
            : errorMessage;

        return `
        <!DOCTYPE html>
        <html>
        <head>
            <style>
                :root {
                    --error-bg: #f8f9fa;
                    --error-text: #333;
                    --error-card-bg: white;
                    --error-card-shadow: rgba(0,0,0,0.1);
                    --error-border: #dee2e6;
                    --error-accent: #f05133;
                    --error-details-bg: #f8f9fa;
                    --error-details-text: #6c757d;
                    --success-bg: #28a745;
                    --success-hover: #218838;
                }

                @media (prefers-color-scheme: dark) {
                    :root {
                        --error-bg: #0d1117;
                        --error-text: #c9d1d9;
                        --error-card-bg: #161b22;
                        --error-card-shadow: rgba(0,0,0,0.3);
                        --error-border: #30363d;
                        --error-accent: #f05133;
                        --error-details-bg: #161b22;
                        --error-details-text: #8b949e;
                        --success-bg: #238636;
                        --success-hover: #1a7f37;
                    }
                }

                body {
                    font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
                    display: flex;
                    justify-content: center;
                    align-items: center;
                    height: 100vh;
                    margin: 0;
                    background: var(--error-bg);
                    color: var(--error-text);
                }
                .error {
                    text-align: center;
                    max-width: 600px;
                    padding: 40px;
                    background: var(--error-card-bg);
                    border-radius: 10px;
                    box-shadow: 0 2px 10px var(--error-card-shadow);
                    border: 1px solid var(--error-border);
                }
                .error-icon {
                    font-size: 3em;
                    color: var(--error-accent);
                    margin-bottom: 20px;
                }
                .retry-button {
                    background: var(--error-accent);
                    color: white;
                    border: none;
                    padding: 12px 24px;
                    border-radius: 5px;
                    cursor: pointer;
                    margin: 10px;
                    font-size: 16px;
                }
                .retry-button:hover {
                    background: #d73a49;
                }
                .visit-site-button {
                    background: var(--success-bg);
                    color: white;
                    border: none;
                    padding: 12px 24px;
                    border-radius: 5px;
                    cursor: pointer;
                    margin: 10px;
                    font-size: 16px;
                    text-decoration: none;
                    display: inline-block;
                }
                .visit-site-button:hover {
                    background: var(--success-hover);
                    color: white;
                }
                .error-details {
                    background: var(--error-details-bg);
                    border: 1px solid var(--error-border);
                    border-radius: 5px;
                    padding: 15px;
                    margin: 20px 0;
                    text-align: left;
                    font-size: 14px;
                    color: var(--error-details-text);
                }

                @media (prefers-color-scheme: dark) {
                    .retry-button:hover {
                        background: #d73a49;
                    }
                }
            </style>
        </head>
        <body>
            <div class="error">
                <div class="error-icon">⚠️</div>
                <h2>Failed to Load Lesson</h2>
                <p><strong>${lessonTitle}</strong></p>
                <p>${displayMessage}</p>
                ${isBlocked ? `
                <div class="error-details">
                    <strong>What this means:</strong><br>
                    • Git book protects against automated access<br>
                    • This helps prevent server overload and ensures fair access<br>
                    • Access is usually restored after a short wait<br><br>
                    <strong>Suggestions:</strong><br>
                    • Wait 5-10 minutes before trying again<br>
                    • Visit the lesson directly on the website<br>
                    • The curriculum structure is still available for navigation
                </div>
                ` : isNotFound ? `
                <div class="error-details">
                    <strong>What this means:</strong><br>
                    • The lesson URL may have changed due to book updates<br>
                    • The lesson might have been moved or renamed<br>
                    • Git book is regularly updated<br><br>
                    <strong>Attempted URL:</strong><br>
                    <code style="word-break: break-all; background: var(--error-details-bg); padding: 5px; border-radius: 3px; font-size: 12px;">${lessonUrl}</code><br><br>
                    <strong>Suggestions:</strong><br>
                    • Browse the current curriculum to find the updated lesson<br>
                    • Visit Git book website directly<br>
                    • Check if the lesson is available under a different name
                </div>
                ` : '<p>Please check your internet connection and try again.</p>'}
                <div>
                    <button class="retry-button" onclick="retry()">Retry</button>
                    ${isNotFound ? `
                    <button class="visit-site-button" onclick="browseCurriculum()">Browse Curriculum</button>
                    <a class="visit-site-button" href="https://git-scm.com/book/en/v2" target="_blank">Visit Git Book</a>
                    ` : `
                    <a class="visit-site-button" href="https://git-scm.com/book/en/v2" target="_blank">Visit Git Book</a>
                    `}
                </div>
            </div>
            <script>
                function retry() {
                    location.reload();
                }
                function browseCurriculum() {
                    // This will be handled by the VS Code extension
                    if (typeof acquireVsCodeApi !== 'undefined') {
                        const vscode = acquireVsCodeApi();
                        vscode.postMessage({
                            command: 'browseCurriculum'
                        });
                    }
                }
            </script>
        </body>
        </html>`;
    }

    /**
     * Get the Git curriculum data
     * @returns {Promise<Object>} Curriculum object
     */
    async getCurriculum() {
        try {
            const fs = require('fs');
            const path = require('path');
            const curriculumPath = path.join(__dirname, 'curriculum.json');
            return JSON.parse(fs.readFileSync(curriculumPath, 'utf8'));
        } catch (error) {
            console.error('Error loading Git curriculum:', error);
            return { chapters: [] };
        }
    }

    /**
     * Map a Git book URL to a lesson ID
     * @param {string} url - Git book URL
     * @returns {Promise<string|null>} Lesson ID or null if not found
     */
    async mapUrlToLessonId(url) {
        try {
            const curriculum = await this.getCurriculum();

            // Normalize URL for comparison (remove protocol, trailing slashes, etc.)
            const normalizedUrl = url.replace(/^https?:\/\//, '').replace(/\/$/, '').toLowerCase();

            for (const chapter of curriculum.chapters) {
                if (chapter.lessons) {
                    for (const lesson of chapter.lessons) {
                        const lessonNormalizedUrl = lesson.url.replace(/^https?:\/\//, '').replace(/\/$/, '').toLowerCase();
                        if (normalizedUrl === lessonNormalizedUrl) {
                            return lesson.id;
                        }
                    }
                }
            }

            return null;
        } catch (error) {
            console.error('Error mapping URL to lesson ID:', error);
            return null;
        }
    }

    /**
     * Process HTML content to make it work within VS Code webview
     * Converts external Git book links to local commands and handles images
     * @param {string} content - HTML content to process
     * @returns {Promise<string>} Processed HTML content
     */
    async processContentForWebview(content, webview) {
        // Ensure content is a string
        if (typeof content !== 'string') {
            console.error('processContentForWebview received non-string content:', typeof content, content);
            content = String(content || '');
        }

        let processedContent = content;

        // Process links - convert Git book URLs to local commands
        processedContent = await this.processLinksForWebview(processedContent);

        // Process images - download and cache them for proper display
        processedContent = await this.processImagesForWebview(processedContent, webview);

        // Remove any remaining Git book navigation elements
        processedContent = this.removeGitBookNavigation(processedContent);

        // Ensure we return a string
        return typeof processedContent === 'string' ? processedContent : content;
    }

    /**
     * Process links in HTML content to work within webview
     * @param {string} content - HTML content
     * @returns {Promise<string>} Content with processed links
     */
    async processLinksForWebview(content) {
        let processedContent = content;

        // Use a more robust approach - split and process each link individually
        const linkRegex = /<a[^>]*href=["']([^"']*)["'][^>]*>(.*?)<\/a>/gi;
        let match;
        const replacements = [];

        // First pass: collect all matches with their positions
        while ((match = linkRegex.exec(content)) !== null) {
            replacements.push({
                fullMatch: match[0],
                href: match[1],
                linkText: match[2],
                index: match.index
            });
        }

        // Process each link
        for (const replacement of replacements) {
            const { fullMatch, href, linkText } = replacement;

            let newLink = fullMatch;

            // Check if this is a Git book URL
            if (href.includes('git-scm.com/book/en/v2')) {
                // Map URL to lesson ID
                const lessonId = await this.mapUrlToLessonId(href);

                if (lessonId) {
                    // Replace with local command link
                    newLink = `<a href="#" onclick="openLesson('${lessonId}')" style="color: var(--link-color); text-decoration: none;" onmouseover="this.style.textDecoration='underline'" onmouseout="this.style.textDecoration='none'">${linkText}</a>`;
                } else {
                    // Keep as external link but open in new tab
                    newLink = `<a href="${href}" target="_blank" style="color: var(--link-color); text-decoration: none;" onmouseover="this.style.textDecoration='underline'" onmouseout="this.style.textDecoration='none'">${linkText} ↗</a>`;
                }
            } else if (href.startsWith('http') || href.startsWith('//')) {
                // External link - keep but open in new tab
                newLink = `<a href="${href}" target="_blank" style="color: var(--link-color); text-decoration: none;" onmouseover="this.style.textDecoration='underline'" onmouseout="this.style.textDecoration='none'">${linkText} ↗</a>`;
            }

            // Replace in processed content
            processedContent = processedContent.replace(fullMatch, newLink);
        }

        return processedContent;
    }

    /**
     * Process images in HTML content for webview display
     * Downloads and caches images locally for proper display
     * @param {string} content - HTML content
     * @param {Object} webview - VS Code webview object for URI conversion
     * @returns {Promise<string>} Content with processed images
     */
    async processImagesForWebview(content, webview) {
        if (!webview) {
            // Fallback to placeholders if no webview provided
            return this.processImagesAsPlaceholders(content);
        }

        let processedContent = content;

        // Regular expression to match image tags
        const imageRegex = /<img[^>]*src=["']([^"']*)["'][^>]*>/gi;
        let match;
        const replacements = [];

        // First pass: collect all image matches
        while ((match = imageRegex.exec(content)) !== null) {
            replacements.push({
                fullMatch: match[0],
                src: match[1],
                index: match.index
            });
        }

        // Process each image
        for (const replacement of replacements) {
            const { fullMatch, src } = replacement;

            try {
                // Skip if src is empty or already a data URL
                if (!src || src.startsWith('data:') || src.startsWith('blob:')) {
                    continue;
                }

                // Convert relative URLs to absolute URLs
                let absoluteSrc = src;
                if (src.startsWith('//')) {
                    absoluteSrc = 'https:' + src;
                } else if (src.startsWith('/')) {
                    absoluteSrc = 'https://git-scm.com' + src;
                } else if (!src.startsWith('http')) {
                    // Relative URL - try to make it absolute
                    absoluteSrc = 'https://git-scm.com/book/en/v2/' + src;
                }

                console.log('Processing image:', absoluteSrc);

                // Download and cache the image
                const localImageUri = await this.downloadAndCacheImage(absoluteSrc, webview);

                if (localImageUri) {
                    console.log('Successfully cached image:', absoluteSrc);
                    // Extract other attributes from the original img tag
                    const altMatch = fullMatch.match(/alt=["']([^"']*)["']/i);
                    const titleMatch = fullMatch.match(/title=["']([^"']*)["']/i);
                    const widthMatch = fullMatch.match(/width=["']([^"']*)["']/i);
                    const heightMatch = fullMatch.match(/height=["']([^"']*)["']/i);
                    const classMatch = fullMatch.match(/class=["']([^"']*)["']/i);

                    // Build new img tag with local URI and preserved attributes
                    let newImgTag = `<img src="${localImageUri}"`;
                    if (altMatch) newImgTag += ` alt="${altMatch[1]}"`;
                    if (titleMatch) newImgTag += ` title="${titleMatch[1]}"`;
                    if (widthMatch) newImgTag += ` width="${widthMatch[1]}"`;
                    if (heightMatch) newImgTag += ` height="${heightMatch[1]}"`;
                    if (classMatch) newImgTag += ` class="${classMatch[1]}"`;
                    newImgTag += ` style="max-width: 100%; height: auto;" />`;

                    processedContent = processedContent.replace(fullMatch, newImgTag);
                } else {
                    console.warn('Failed to cache image:', absoluteSrc);
                    // Fallback to placeholder if download failed
                    const placeholder = this.createImagePlaceholder(fullMatch);
                    processedContent = processedContent.replace(fullMatch, placeholder);
                }
            } catch (error) {
                console.error('Error processing image:', src, error);
                // Fallback to placeholder
                const placeholder = this.createImagePlaceholder(fullMatch);
                processedContent = processedContent.replace(fullMatch, placeholder);
            }
        }

        return processedContent;
    }

    /**
     * Download and cache an image for webview display
     * @param {string} imageUrl - URL of the image to download
     * @param {Object} webview - VS Code webview object for URI conversion
     * @returns {Promise<string|null>} Webview URI for the cached image or null if failed
     */
    async downloadAndCacheImage(imageUrl, webview) {
        try {
            // Get cache directory
            const cacheDir = await this.getCacheDir();
            if (!cacheDir) return null;

            // Create images subdirectory
            const imagesDir = path.join(cacheDir, 'images');
            await fs.mkdir(imagesDir, { recursive: true });

            // Generate cache key for the image
            const imageCacheKey = crypto.createHash('md5').update(imageUrl).digest('hex');
            const imageExt = this.getImageExtensionFromUrl(imageUrl) || 'png';
            const cachedImagePath = path.join(imagesDir, `${imageCacheKey}.${imageExt}`);

            // Check if image is already cached
            try {
                await fs.access(cachedImagePath);
                // Image exists, convert to webview URI
                const imageUri = this.vscode.Uri.file(cachedImagePath);
                return webview.asWebviewUri(imageUri);
            } catch {
                // Image not cached, download it
            }

            // Download the image
            const imageBuffer = await this.downloadImage(imageUrl);
            if (!imageBuffer) return null;

            // Save to cache
            await fs.writeFile(cachedImagePath, imageBuffer);

            // Convert to webview URI
            const imageUri = this.vscode.Uri.file(cachedImagePath);
            return webview.asWebviewUri(imageUri);

        } catch (error) {
            console.error('Error downloading/caching image:', imageUrl, error);
            return null;
        }
    }

    /**
     * Download an image from a URL
     * @param {string} url - Image URL to download
     * @returns {Promise<Buffer|null>} Image buffer or null if failed
     */
    async downloadImage(url) {
        return new Promise((resolve) => {
            try {
                // Validate URL
                const urlObj = new URL(url);
                if (!['http:', 'https:'].includes(urlObj.protocol)) {
                    console.warn('Invalid image URL protocol:', url);
                    resolve(null);
                    return;
                }

                // Handle both http and https
                const protocol = url.startsWith('https://') ? https : http;

                const request = protocol.get(url, {
                    headers: {
                        'User-Agent': 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36',
                        'Accept': 'image/webp,image/apng,image/*,*/*;q=0.8',
                        'Accept-Language': 'en-US,en;q=0.9',
                        'Accept-Encoding': 'gzip, deflate, br',
                        'DNT': '1',
                        'Connection': 'keep-alive',
                        'Upgrade-Insecure-Requests': '1',
                        'Sec-Fetch-Dest': 'image',
                        'Sec-Fetch-Mode': 'no-cors',
                        'Sec-Fetch-Site': 'cross-site',
                        'Cache-Control': 'no-cache',
                        'Pragma': 'no-cache'
                    },
                    timeout: 15000, // 15 second timeout
                    followRedirect: true,
                    maxRedirects: 5
                }, (response) => {
                    // Handle redirects
                    if (response.statusCode >= 300 && response.statusCode < 400 && response.headers.location) {
                        console.log('Following redirect for image:', response.headers.location);
                        // Recursively try the redirect URL
                        this.downloadImage(response.headers.location).then(resolve).catch(() => resolve(null));
                        return;
                    }

                    if (response.statusCode !== 200) {
                        console.warn('Image download failed with status:', response.statusCode, 'for URL:', url);
                        resolve(null);
                        return;
                    }

                    // Check content type
                    const contentType = response.headers['content-type'];
                    if (contentType && !contentType.startsWith('image/')) {
                        console.warn('URL does not return image content:', contentType, 'for URL:', url);
                        resolve(null);
                        return;
                    }

                    const chunks = [];
                    response.on('data', (chunk) => chunks.push(chunk));
                    response.on('end', () => {
                        try {
                            const buffer = Buffer.concat(chunks);
                            if (buffer.length === 0) {
                                console.warn('Downloaded image is empty for URL:', url);
                                resolve(null);
                                return;
                            }
                            console.log('Successfully downloaded image:', url, `(${buffer.length} bytes)`);
                            resolve(buffer);
                        } catch (error) {
                            console.error('Error processing downloaded image:', error);
                            resolve(null);
                        }
                    });
                });

                request.on('error', (error) => {
                    console.error('Image download error for URL:', url, error.message);
                    resolve(null);
                });

                request.on('timeout', () => {
                    console.warn('Image download timeout for URL:', url);
                    request.destroy();
                    resolve(null);
                });

            } catch (error) {
                console.error('Invalid image URL:', url, error.message);
                resolve(null);
            }
        });
    }

    /**
     * Get image extension from URL
     * @param {string} url - Image URL
     * @returns {string|null} File extension or null
     */
    getImageExtensionFromUrl(url) {
        const extMatch = url.match(/\.([a-zA-Z]+)(?:\?|$)/);
        if (extMatch) {
            const ext = extMatch[1].toLowerCase();
            // Common image extensions
            if (['png', 'jpg', 'jpeg', 'gif', 'svg', 'webp'].includes(ext)) {
                return ext === 'jpeg' ? 'jpg' : ext;
            }
        }
        return null;
    }

    /**
     * Create a placeholder for an image that couldn't be loaded
     * @param {string} originalImgTag - Original img tag
     * @returns {string} Placeholder HTML
     */
    createImagePlaceholder(originalImgTag) {
        // Extract alt text if available
        const altMatch = originalImgTag.match(/alt=["']([^"']*)["']/i);
        const alt = altMatch ? altMatch[1] : 'Image';

        // Return a styled placeholder
        return `<div style="display: inline-block; padding: 15px; background: var(--accent-bg); border: 1px solid var(--border-color); border-radius: 8px; color: var(--text-color); font-size: 0.9em; margin: 8px 0; text-align: center; min-width: 120px;">
            <div style="font-size: 2em; margin-bottom: 8px;">🖼️</div>
            <div><strong>${alt}</strong></div>
            <div style="font-size: 0.8em; opacity: 0.7; margin-top: 4px;">Image unavailable</div>
        </div>`;
    }

    /**
     * Fallback method to process images as placeholders (when no webview available)
     * @param {string} content - HTML content
     * @returns {string} Content with image placeholders
     */
    processImagesAsPlaceholders(content) {
        const imageRegex = /<img[^>]*src=["']([^"']*)["'][^>]*>/gi;

        return content.replace(imageRegex, (match) => {
            return this.createImagePlaceholder(match);
        });
    }

    /**
     * Remove Git book's navigation elements that shouldn't appear in our webview
     * @param {string} content - HTML content
     * @returns {string} Content with navigation removed
     */
    removeGitBookNavigation(content) {
        let cleanedContent = content;

        // Remove common Git book navigation patterns
        const navPatterns = [
            /<nav[^>]*>.*?<\/nav>/gis,
            /<div[^>]*class="[^"]*nav[^"]*"[^>]*>.*?<\/div>/gis,
            /<div[^>]*class="[^"]*breadcrumb[^"]*"[^>]*>.*?<\/div>/gis,
            /<div[^>]*class="[^"]*pagination[^"]*"[^>]*>.*?<\/div>/gis,
            /<a[^>]*href="[^"]*prev[^"]*"[^>]*>.*?<\/a>/gis,
            /<a[^>]*href="[^"]*next[^"]*"[^>]*>.*?<\/a>/gis
        ];

        for (const pattern of navPatterns) {
            cleanedContent = cleanedContent.replace(pattern, '');
        }

        return cleanedContent;
    }

    /**
     * Get the URI for the image cache directory
     * @returns {Promise<Object|null>} VS Code URI for image cache directory or null
     */
    async getImageCacheUri() {
        try {
            const cacheDir = await this.getCacheDir();
            if (!cacheDir) return null;

            const imagesDir = path.join(cacheDir, 'images');
            return this.vscode.Uri.file(imagesDir);
        } catch (error) {
            return null;
        }
    }
}

module.exports = GitBookManager;