/**
 * The Odin Project Manager
 *
 * Handles fetching and displaying content from The Odin Project curriculum
 */

const https = require('https');
const http = require('http');

class OdinProjectManager {
    constructor(vscode, progressTracker = null) {
        this.vscode = vscode;
        this.progressTracker = progressTracker;
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
                reject(new Error('Request timeout - The Odin Project may be temporarily unavailable'));
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
                    throw new Error('Access blocked by The Odin Project. This may be due to rate limiting or bot detection. Please try again later or visit the website directly.');
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
     * Extract the main content from The Odin Project lesson HTML
     * @param {string} html - Raw HTML content
     * @param {string} lessonTitle - Title of the lesson
     * @param {Object|null} nextLesson - Next lesson object or null
     * @returns {string} Cleaned HTML content
     */
    extractLessonContent(html, lessonTitle, nextLesson) {
        try {
            // Extract only the div with class containing "lesson-content"
            const lessonContentRegex = /<div[^>]*class="[^"]*lesson-content[^"]*"[^>]*>(.*?)<\/div>/is;
            const match = html.match(lessonContentRegex);
            
            if (!match || !match[1]) {
                // Fallback to generic content
                return this.getGenericLessonContent(lessonTitle);
            }

            let content = match[1];

            // Clean up the content - remove unwanted elements
            content = content.replace(/<script[^>]*>.*?<\/script>/gis, '');
            content = content.replace(/<style[^>]*>.*?<\/style>/gis, '');
            content = content.replace(/<nav[^>]*>.*?<\/nav>/gis, '');
            content = content.replace(/<header[^>]*>.*?<\/header>/gis, '');
            content = content.replace(/<footer[^>]*>.*?<\/footer>/gis, '');
            content = content.replace(/<aside[^>]*>.*?<\/aside>/gis, '');
            content = content.replace(/<form[^>]*>.*?<\/form>/gis, '');
            content = content.replace(/<input[^>]*>/gi, '');
            content = content.replace(/<button[^>]*>.*?<\/button>/gis, '');
            content = content.replace(/<select[^>]*>.*?<\/select>/gis, '');

            // Remove specific Odin Project navigation elements
            content = content.replace(/<[^>]*>Improve on GitHub<\/[^>]*>/gi, '');
            content = content.replace(/<[^>]*>Report an issue<\/[^>]*>/gi, '');
            content = content.replace(/<[^>]*>See lesson changelog<\/[^>]*>/gi, '');
            content = content.replace(/<[^>]*>View Course<\/[^>]*>/gi, '');
            content = content.replace(/<[^>]*>Sign in to track progress<\/[^>]*>/gi, '');
            content = content.replace(/<[^>]*>Next Lesson.*?<\/[^>]*>/gi, '');
            content = content.replace(/<[^>]*>[^<]*Next Lesson[^<]*<\/[^>]*>/gi, '');

            // Remove arrow icons and symbols - comprehensive filtering
            content = content.replace(/[→←↑↓⇒⇐⇓⇑↗↘↙↖▶◀▲▼►◄▸◂▴▾▻◅]/g, ''); // Unicode arrows and triangles
            content = content.replace(/&[lr]arr;/gi, ''); // HTML arrow entities
            content = content.replace(/&[lr]Arr;/gi, ''); // HTML double arrow entities
            content = content.replace(/&uarr;/gi, ''); // Up arrow
            content = content.replace(/&darr;/gi, ''); // Down arrow
            content = content.replace(/&[lr]tri;/gi, ''); // Triangle entities
            content = content.replace(/&[lr]ang;/gi, ''); // Angle bracket entities
            content = content.replace(/&#x?[0-9a-f]+;/gi, ''); // Numeric HTML entities that might be arrows

            // Remove elements with arrow-related classes or attributes
            content = content.replace(/<[^>]*class="[^"]*arrow[^"]*"[^>]*>.*?<\/[^>]*>/gi, ''); // Arrow classes
            content = content.replace(/<[^>]*class="[^"]*fa-arrow[^"]*"[^>]*><\/[^>]*>/gi, ''); // FontAwesome arrows
            content = content.replace(/<[^>]*class="[^"]*icon-arrow[^"]*"[^>]*><\/[^>]*>/gi, ''); // Icon arrows
            content = content.replace(/<[^>]*data-icon="[^"]*arrow[^"]*"[^>]*><\/[^>]*>/gi, ''); // Data icon arrows
            content = content.replace(/<i[^>]*class="[^"]*fa[^"]*arrow[^"]*"[^>]*><\/i>/gi, ''); // FontAwesome arrow icons
            content = content.replace(/<span[^>]*class="[^"]*arrow[^"]*"[^>]*>.*?<\/span>/gi, ''); // Arrow spans
            content = content.replace(/<div[^>]*class="[^"]*arrow[^"]*"[^>]*>.*?<\/div>/gi, ''); // Arrow divs

            // Remove SVG icons that might contain arrows
            content = content.replace(/<svg[^>]*>[\s\S]*?<\/svg>/gi, ''); // All SVG elements
            content = content.replace(/<path[^>]*d="[^"]*M[^"]*"[^>]*>/gi, ''); // SVG path elements that might be arrows
            content = content.replace(/<use[^>]*href="[^"]*#arrow[^"]*"[^>]*>/gi, ''); // SVG use elements referencing arrows

            // Remove any remaining icon elements that might contain arrows
            content = content.replace(/<i[^>]*class="[^"]*icon[^"]*"[^>]*><\/i>/gi, ''); // Generic icons
            content = content.replace(/<span[^>]*class="[^"]*icon[^"]*"[^>]*>.*?<\/span>/gi, ''); // Icon spans
            content = content.replace(/<div[^>]*class="[^"]*icon[^"]*"[^>]*>.*?<\/div>/gi, ''); // Icon divs

            // Remove elements with CSS that might create arrows (pseudo-elements, backgrounds)
            content = content.replace(/<[^>]*style="[^"]*content:[^"]*arrow[^"]*"[^>]*>.*?<\/[^>]*>/gi, ''); // Inline styles with arrow content
            content = content.replace(/<[^>]*style="[^"]*background[^"]*arrow[^"]*"[^>]*>.*?<\/[^>]*>/gi, ''); // Background arrows

            // Remove any remaining elements that might be navigation arrows or next buttons
            content = content.replace(/<[^>]*>[^<]*next[^<]*<\/[^>]*>/gi, ''); // Elements containing "next"
            content = content.replace(/<[^>]*>[^<]*continue[^<]*<\/[^>]*>/gi, ''); // Elements containing "continue"
            content = content.replace(/<[^>]*class="[^"]*next[^"]*"[^>]*>.*?<\/[^>]*>/gi, ''); // Next classes
            content = content.replace(/<[^>]*class="[^"]*continue[^"]*"[^>]*>.*?<\/[^>]*>/gi, ''); // Continue classes

            // More comprehensive filtering for remaining elements
            content = content.replace(/See lesson changelog/gi, '');
            content = content.replace(/View Course/gi, '');
            content = content.replace(/changelog/gi, '');
            content = content.replace(/<[^>]*href="[^"]*changelog[^"]*"[^>]*>.*?<\/[^>]*>/gi, '');
            content = content.replace(/<[^>]*href="[^"]*course[^"]*"[^>]*>.*?<\/[^>]*>/gi, '');

            // Remove GitHub related elements (icons, links, etc.)
            content = content.replace(/<[^>]*class="[^"]*github[^"]*"[^>]*>.*?<\/[^>]*>/gi, '');
            content = content.replace(/<[^>]*href="[^"]*github[^"]*"[^>]*>.*?<\/[^>]*>/gi, '');
            content = content.replace(/<svg[^>]*>.*?<\/svg>/gis, ''); // Remove SVG icons
            content = content.replace(/<i[^>]*class="[^"]*fa[^"]*github[^"]*"[^>]*><\/i>/gi, ''); // FontAwesome GitHub icons
            content = content.replace(/<span[^>]*class="[^"]*icon[^"]*github[^"]*"[^>]*>.*?<\/span>/gi, ''); // Icon spans

            // Remove elements containing these texts (more comprehensive)
            content = content.replace(/<a[^>]*href="[^"]*github[^"]*"[^>]*>.*?<\/a>/gi, '');
            content = content.replace(/<a[^>]*href="[^"]*report[^"]*"[^>]*>.*?<\/a>/gi, '');
            content = content.replace(/<a[^>]*href="[^"]*changelog[^"]*"[^>]*>.*?<\/a>/gi, '');
            content = content.replace(/<a[^>]*href="[^"]*signin[^"]*"[^>]*>.*?<\/a>/gi, '');
            content = content.replace(/<div[^>]*class="[^"]*lesson-navigation[^"]*"[^>]*>.*?<\/div>/gis, '');
            content = content.replace(/<div[^>]*class="[^"]*course-navigation[^"]*"[^>]*>.*?<\/div>/gis, '');
            content = content.replace(/<div[^>]*class="[^"]*progress-tracker[^"]*"[^>]*>.*?<\/div>/gis, '');

            // Remove footer-like sections that might contain these links
            content = content.replace(/<div[^>]*class="[^"]*footer[^"]*"[^>]*>.*?<\/div>/gis, '');
            content = content.replace(/<section[^>]*class="[^"]*footer[^"]*"[^>]*>.*?<\/section>/gis, '');
            content = content.replace(/<footer[^>]*>.*?<\/footer>/gis, '');

            // Remove any remaining navigation or action sections
            content = content.replace(/<div[^>]*class="[^"]*actions[^"]*"[^>]*>.*?<\/div>/gis, '');
            content = content.replace(/<div[^>]*class="[^"]*nav[^"]*"[^>]*>.*?<\/div>/gis, '');
            content = content.replace(/<nav[^>]*>.*?<\/nav>/gis, '');

            // Remove any elements that might be at the end of lessons (common places for navigation arrows)
            content = content.replace(/<div[^>]*class="[^"]*lesson-end[^"]*"[^>]*>.*?<\/div>/gis, '');
            content = content.replace(/<div[^>]*class="[^"]*lesson-footer[^"]*"[^>]*>.*?<\/div>/gis, '');
            content = content.replace(/<div[^>]*class="[^"]*lesson-navigation[^"]*"[^>]*>.*?<\/div>/gis, '');
            content = content.replace(/<section[^>]*class="[^"]*lesson-end[^"]*"[^>]*>.*?<\/section>/gis, '');
            content = content.replace(/<section[^>]*class="[^"]*lesson-footer[^"]*"[^>]*>.*?<\/section>/gis, '');

            // Remove any remaining elements with IDs that might indicate navigation
            content = content.replace(/<[^>]*id="[^"]*next[^"]*"[^>]*>.*?<\/[^>]*>/gi, '');
            content = content.replace(/<[^>]*id="[^"]*arrow[^"]*"[^>]*>.*?<\/[^>]*>/gi, '');
            content = content.replace(/<[^>]*id="[^"]*navigation[^"]*"[^>]*>.*?<\/[^>]*>/gi, '');

            // Final cleanup: remove any standalone arrow symbols that might have been missed
            content = content.replace(/\s*[→←↑↓⇒⇐⇓⇑↗↘↙↖▶◀▲▼►◄▸◂▴▾▻◅]\s*/g, ' '); // Clean up any remaining arrows with surrounding whitespace

            // Extract meaningful content while preserving structure
            // Allow common content elements including links
            const allowedTags = ['h1', 'h2', 'h3', 'h4', 'h5', 'h6', 'p', 'ul', 'ol', 'li', 'blockquote', 'pre', 'code', 'strong', 'em', 'a', 'br', 'span', 'div'];

            // Use a more comprehensive approach - extract content blocks while preserving structure
            let cleanContent = '';

            // Split content into logical sections and clean each one
            const sections = content.split(/(<\/?(?:h[1-6]|p|ul|ol|blockquote|pre|div)[^>]*>)/i);

            for (let i = 0; i < sections.length; i++) {
                let section = sections[i];

                // If this is a tag, check if it's allowed
                if (section.match(/^<\/?[a-zA-Z][^>]*>$/)) {
                    const tagMatch = section.match(/^<\/?([a-zA-Z]+)/);
                    if (tagMatch) {
                        const tagName = tagMatch[1].toLowerCase();
                        if (allowedTags.includes(tagName)) {
                            cleanContent += section;
                        }
                        // Skip content inside non-allowed tags
                        if (!section.startsWith('</') && allowedTags.includes(tagName)) {
                            // Find the matching closing tag and include content
                            let depth = 1;
                            let contentSection = '';
                            for (let j = i + 1; j < sections.length && depth > 0; j++) {
                                if (sections[j].match(new RegExp(`^</${tagName}`, 'i'))) {
                                    depth--;
                                    if (depth === 0) {
                                        contentSection += sections[j];
                                        i = j; // Skip to after this closing tag
                                        break;
                                    }
                                } else if (sections[j].match(new RegExp(`^<${tagName}`, 'i'))) {
                                    depth++;
                                }
                                contentSection += sections[j];
                            }
                            cleanContent += contentSection;
                        }
                    }
                } else if (section.trim()) {
                    // This is text content - include it if it's substantial
                    if (section.trim().length > 10) {
                        cleanContent += section;
                    }
                }
            }

            // Clean up excessive whitespace and empty elements
            cleanContent = cleanContent.replace(/\n\s*\n\s*\n/g, '\n\n'); // Remove excessive newlines
            cleanContent = cleanContent.replace(/<p>\s*<\/p>/gi, ''); // Remove empty paragraphs
            cleanContent = cleanContent.replace(/<div>\s*<\/div>/gi, ''); // Remove empty divs
            cleanContent = cleanContent.replace(/<br\s*\/?>\s*<br\s*\/?>/gi, '<br>'); // Remove double line breaks
            cleanContent = cleanContent.replace(/\s*<br\s*\/?>\s*$/gi, ''); // Remove trailing line breaks
            cleanContent = cleanContent.replace(/^\s*<br\s*\/?>\s*/gi, ''); // Remove leading line breaks

            // Remove any remaining empty containers that might be left after filtering
            cleanContent = cleanContent.replace(/<div[^>]*>\s*<div[^>]*>\s*<\/div>\s*<\/div>/gi, ''); // Nested empty divs
            cleanContent = cleanContent.replace(/<section[^>]*>\s*<section[^>]*>\s*<\/section>\s*<\/section>/gi, ''); // Nested empty sections

            // More aggressive cleanup for empty elements and whitespace
            cleanContent = cleanContent.replace(/<div[^>]*>\s*<\/div>/gi, ''); // Remove all empty divs
            cleanContent = cleanContent.replace(/<p[^>]*>\s*<\/p>/gi, ''); // Remove all empty paragraphs
            cleanContent = cleanContent.replace(/<span[^>]*>\s*<\/span>/gi, ''); // Remove empty spans
            cleanContent = cleanContent.replace(/<section[^>]*>\s*<\/section>/gi, ''); // Remove empty sections
            cleanContent = cleanContent.replace(/<article[^>]*>\s*<\/article>/gi, ''); // Remove empty articles

            // Remove elements that only contain whitespace or line breaks
            cleanContent = cleanContent.replace(/<div[^>]*>[\s\n\r]*<\/div>/gi, ''); // Divs with only whitespace
            cleanContent = cleanContent.replace(/<p[^>]*>[\s\n\r]*<\/p>/gi, ''); // Paragraphs with only whitespace
            cleanContent = cleanContent.replace(/<span[^>]*>[\s\n\r]*<\/span>/gi, ''); // Spans with only whitespace

            // Clean up multiple consecutive empty lines
            cleanContent = cleanContent.replace(/\n\s*\n\s*\n/g, '\n\n'); // Reduce to max 2 consecutive newlines
            cleanContent = cleanContent.replace(/^\s*\n+/g, ''); // Remove leading newlines
            cleanContent = cleanContent.replace(/\n+\s*$/g, ''); // Remove trailing newlines

            // Remove any remaining navigation-related empty containers
            cleanContent = cleanContent.replace(/<div[^>]*class="[^"]*nav[^"]*"[^>]*>[\s\n\r]*<\/div>/gi, ''); // Empty nav divs
            cleanContent = cleanContent.replace(/<div[^>]*class="[^"]*action[^"]*"[^>]*>[\s\n\r]*<\/div>/gi, ''); // Empty action divs
            cleanContent = cleanContent.replace(/<div[^>]*class="[^"]*lesson-end[^"]*"[^>]*>[\s\n\r]*<\/div>/gi, ''); // Empty lesson-end divs
            cleanContent = cleanContent.replace(/<div[^>]*class="[^"]*lesson-footer[^"]*"[^>]*>[\s\n\r]*<\/div>/gi, ''); // Empty lesson-footer divs

            // If we have substantial content, format it
            if (cleanContent.length > 200) {
                return this.formatLessonContent(cleanContent, lessonTitle, nextLesson);
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
     * @returns {string} Formatted HTML page
     */
    formatLessonContent(content, lessonTitle, nextLesson) {
        // Build HTML using string concatenation to avoid JSX linting issues
        let html = '<!DOCTYPE html>';
        html += '<html lang="en">';
        html += '<head>';
        html += '<meta charset="UTF-8">';
        html += '<meta name="viewport" content="width=device-width, initial-scale=1.0">';
        html += '<title>' + lessonTitle + ' - The Odin Project</title>';
        html += '<style>';
        html += ':root{--bg-color:#f8f9fa;--text-color:#333;--card-bg:white;--card-shadow:rgba(0,0,0,0.1);--border-color:#dee2e6;--accent-bg:#e9ecef;--accent-border:#007bff;--link-color:#007bff;--button-bg:#6c757d;--button-hover:#5a6268;--header-bg:linear-gradient(135deg,#007bff,#0056b3)}';
        html += '@media(prefers-color-scheme:dark){:root{--bg-color:#1e1e1e;--text-color:#cccccc;--card-bg:#2d2d30;--card-shadow:rgba(0,0,0,0.3);--border-color:#3e3e42;--accent-bg:#3c3c3c;--accent-border:#4fc1ff;--link-color:#4fc1ff;--button-bg:#5f5f5f;--button-hover:#6f6f6f;--header-bg:linear-gradient(135deg,#4fc1ff,#1e90ff)}}';

        // Base styles
        html += 'body{font-family:-apple-system,BlinkMacSystemFont,"Segoe UI",Roboto,sans-serif;line-height:1.6;color:var(--text-color);background-color:var(--bg-color);margin:0;padding:0;font-size:18px}';

        // Header styles
        html += '.lesson-header{background:var(--header-bg);color:white;padding:20px 15px;border-radius:0;margin-bottom:20px;text-align:center}';
        html += '.lesson-title{font-size:2.8em;margin:0;font-weight:300}';
        html += '.lesson-subtitle{font-size:1.3em;opacity:0.9;margin-top:10px}';

        // Content container styles
        html += '.page-container{max-width:1200px;margin:0 auto;padding:0 15px}';
        html += '.lesson-content{max-width:800px;margin:0 auto;padding:10px 0}';
        html += '.content-section{background:var(--card-bg);padding:20px;border-radius:10px;margin-bottom:10px;box-shadow:0 2px 10px var(--card-shadow);border:1px solid var(--border-color)}';

        // Typography styles
        html += '.lesson-content h1,.lesson-content h2,.lesson-content h3,.lesson-content h4,.lesson-content h5,.lesson-content h6{color:var(--text-color);margin-top:30px;margin-bottom:15px;font-weight:600;font-size:2.5em}';
        html += '.lesson-content p{margin-bottom:15px;color:var(--text-color);font-size:0.95em}';
        html += '.lesson-content ul,.lesson-content ol{margin-bottom:15px;padding-left:30px}';
        html += '.lesson-content li{margin-bottom:5px;color:var(--text-color);font-size:1.1em}';
        html += '.lesson-content blockquote{border-left:4px solid var(--accent-border);padding-left:20px;margin:20px 0;font-style:italic;background:var(--accent-bg);padding:15px 20px;border-radius:5px;font-size:1.1em}';
        html += '.lesson-content pre{background:var(--accent-bg);border:1px solid var(--border-color);border-radius:4px;padding:15px;margin:15px 0;overflow-x:auto;font-family:"Monaco","Menlo","Ubuntu Mono",monospace;font-size:16px;color:var(--text-color)}';
        html += '.lesson-content code{background:var(--accent-bg);padding:2px 6px;border-radius:3px;font-family:"Monaco","Menlo","Ubuntu Mono",monospace;font-size:1em;color:var(--text-color)}';
        html += '.lesson-content a{color:var(--link-color);text-decoration:none}';
        html += '.lesson-content a:hover{text-decoration:underline}';

        // Button styles
        html += '.back-button{display:inline-block;background:var(--button-bg);color:white;padding:12px 24px;text-decoration:none;border-radius:5px;margin-top:10px;font-weight:500;border:none;cursor:pointer;font-size:16px}';
        html += '.back-button:hover{background:var(--button-hover);color:white}';
        html += '.next-button{display:inline-block;background:var(--accent-border);color:white;padding:12px 24px;text-decoration:none;border-radius:5px;margin-top:10px;font-weight:500;border:none;cursor:pointer;font-size:16px}';
        html += '.next-button:hover{background:#0056b3;color:white}';

        // Responsive design
        html += '@media(max-width:768px){';
        html += '.lesson-header{padding:15px 10px}';
        html += '.lesson-title{font-size:2em}';
        html += '.content-section{padding:15px}';
        html += '.lesson-content{max-width:none;padding:10px 0}';
        html += '.page-container{padding:0 10px}';
        html += '}';
        html += '</style>';
        html += '</head>';
        html += '<body>';
        html += '<div class="lesson-header">';
        html += '<h1 class="lesson-title">' + lessonTitle + '</h1>';
        html += '<div class="lesson-subtitle">The Odin Project</div>';
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
            '<p>You can also visit <a href="https://www.theodinproject.com" target="_blank">The Odin Project website</a> directly.</p>' +
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
                    await this.progressTracker.markLessonComplete('odin', lesson.id);
                } catch (progressError) {
                    console.log('Progress tracking failed:', progressError.message);
                    // Don't fail the lesson opening if progress tracking fails
                }
            }

            // Find the next lesson in the curriculum
            const nextLesson = await this.findNextLesson(lesson.id);

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
                // Fetch lesson content with retry logic
                const htmlContent = await this.fetchLessonContentWithRetry(lesson.url);

                // Extract and format content
                const formattedContent = this.extractLessonContent(htmlContent, lesson.title, nextLesson);

                // Set the content
                panel.webview.html = formattedContent;

            } catch (error) {
                // Show error message
                panel.webview.html = this.getErrorHtml(lesson.title, error.message, lesson.url);
            }

            // Handle messages from webview
            panel.webview.onDidReceiveMessage(
                message => {
                    if (message.command === 'goBack') {
                        panel.dispose();
                        // Open the curriculum tree view
                        this.vscode.commands.executeCommand('tsiheader.learnOdin');
                    } else if (message.command === 'nextLesson' && nextLesson) {
                        panel.dispose();
                        // Open the next lesson
                        this.openLesson(nextLesson, context);
                    } else if (message.command === 'browseCurriculum') {
                        // Open the curriculum tree view
                        this.vscode.commands.executeCommand('tsiheader.learnOdin');
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
                :root {
                    --loading-bg: linear-gradient(135deg, #dc3545, #c82333);
                    --loading-text: white;
                }

                @media (prefers-color-scheme: dark) {
                    :root {
                        --loading-bg: linear-gradient(135deg, #f44747, #d13438);
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
                <p>Fetching content from The Odin Project</p>
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
            
            // Search through all paths and courses
            for (const path of curriculum.paths) {
                for (const course of path.courses) {
                    if (course.lessons && course.lessons.length > 0) {
                        for (let i = 0; i < course.lessons.length; i++) {
                            const lesson = course.lessons[i];
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
            const progress = await this.progressTracker.getProgress('odin');
            const stats = await this.progressTracker.getStats('odin');
            
            return {
                lessonsCompleted: stats.lessonsCompleted,
                currentStreak: stats.currentStreak,
                totalStudyTime: stats.totalStudyTime,
                achievements: stats.achievements,
                lastStudyDate: stats.lastStudyDate,
                completed: progress.completed || []
            };
        } catch (error) {
            console.log('Failed to get Odin progress stats:', error.message);
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
     * Record study time for Odin Project
     * @param {number} minutes - Minutes studied
     */
    async recordStudyTime(minutes) {
        if (this.progressTracker) {
            try {
                await this.progressTracker.recordStudyTime('odin', minutes);
            } catch (error) {
                console.log('Failed to record study time:', error.message);
            }
        }
    }
    getErrorHtml(lessonTitle, errorMessage, lessonUrl) {
        const isBlocked = errorMessage.includes('403') || errorMessage.includes('blocked') || errorMessage.includes('Access blocked');
        const isNotFound = errorMessage.includes('404') || errorMessage.includes('Not Found') || errorMessage.includes('ENOTFOUND');
        const displayMessage = isBlocked
            ? 'Access temporarily blocked by The Odin Project. This is likely due to rate limiting or bot detection protection.'
            : isNotFound
            ? 'The lesson content could not be found. The curriculum may have been updated and this lesson might have moved or been renamed.'
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
                    --error-accent: #dc3545;
                    --error-details-bg: #f8f9fa;
                    --error-details-text: #6c757d;
                    --success-bg: #28a745;
                    --success-hover: #218838;
                }

                @media (prefers-color-scheme: dark) {
                    :root {
                        --error-bg: #1e1e1e;
                        --error-text: #cccccc;
                        --error-card-bg: #2d2d30;
                        --error-card-shadow: rgba(0,0,0,0.3);
                        --error-border: #3e3e42;
                        --error-accent: #f44747;
                        --error-details-bg: #2d2d30;
                        --error-details-text: #cccccc;
                        --success-bg: #4ec9b0;
                        --success-hover: #3bb085;
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
                    background: #c82333;
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
                        background: #d13438;
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
                    • The Odin Project protects against automated access<br>
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
                    • The lesson URL may have changed due to curriculum updates<br>
                    • The lesson might have been moved or renamed<br>
                    • The Odin Project regularly updates their content<br><br>
                    <strong>Attempted URL:</strong><br>
                    <code style="word-break: break-all; background: var(--error-details-bg); padding: 5px; border-radius: 3px; font-size: 12px;">${lessonUrl}</code><br><br>
                    <strong>Suggestions:</strong><br>
                    • Browse the current curriculum to find the updated lesson<br>
                    • Visit The Odin Project website directly<br>
                    • Check if the lesson is available under a different name
                </div>
                ` : '<p>Please check your internet connection and try again.</p>'}
                <div>
                    <button class="retry-button" onclick="retry()">Retry</button>
                    ${isNotFound ? `
                    <button class="visit-site-button" onclick="browseCurriculum()">Browse Curriculum</button>
                    <a class="visit-site-button" href="https://www.theodinproject.com" target="_blank" style="margin-left: 10px;">Visit The Odin Project</a>
                    ` : `
                    <a class="visit-site-button" href="https://www.theodinproject.com" target="_blank">Visit The Odin Project</a>
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
}

module.exports = OdinProjectManager;