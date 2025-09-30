const { generateTSIHeaderContent } = require('../headerUtils');

async function createHtmlFiles(vscode, projectName, projectUri) {
    await createIndexHtml(vscode, projectName, projectUri);
    await createStylesCss(vscode, projectName, projectUri);
    await createScriptJs(vscode, projectName, projectUri);
    await createGitIgnore(vscode, projectName, projectUri);
    await createPackageJson(vscode, projectName, projectUri);
    await createReadme(vscode, projectName, projectUri);
    await createWebpackConfig(vscode, projectName, projectUri);
    await createEslintConfig(vscode, projectName, projectUri);
    await createPrettierConfig(vscode, projectName, projectUri);
}

async function createIndexHtml(vscode, projectName, projectUri) {
    const fileUri = vscode.Uri.joinPath(projectUri, 'index.html');
    const headerContent = await generateTSIHeaderContent('index.html', vscode);

    const content = `${headerContent}
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <meta name="description" content="${projectName} - A modern HTML/CSS/JavaScript project">
    <meta name="author" content="TSI Student">
    <title>${projectName}</title>
    <link rel="stylesheet" href="css/styles.css">
    <link rel="icon" href="data:image/svg+xml,<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 100 100'><text y='.9em' font-size='90'>🌐</text></svg>">
</head>
<body>
    <div id="app">
        <header class="site-header">
            <nav class="navbar">
                <div class="nav-container">
                    <h1 class="nav-logo">${projectName}</h1>
                    <ul class="nav-menu">
                        <li class="nav-item"><a href="#home" class="nav-link">Home</a></li>
                        <li class="nav-item"><a href="#about" class="nav-link">About</a></li>
                        <li class="nav-item"><a href="#contact" class="nav-link">Contact</a></li>
                    </ul>
                    <div class="hamburger">
                        <span class="bar"></span>
                        <span class="bar"></span>
                        <span class="bar"></span>
                    </div>
                </div>
            </nav>
        </header>

        <main>
            <section id="home" class="hero-section">
                <div class="hero-container">
                    <h2>Welcome to ${projectName}</h2>
                    <p class="hero-description">This is a modern, responsive HTML/CSS/JavaScript project created with TSI Header extension.</p>
                    <div class="hero-buttons">
                        <button class="btn btn-primary" onclick="showAlert()">Get Started</button>
                        <button class="btn btn-secondary" onclick="toggleTheme()">Toggle Theme</button>
                    </div>
                </div>
            </section>

            <section id="about" class="about-section">
                <div class="container">
                    <h2>About This Project</h2>
                    <div class="about-grid">
                        <div class="about-card">
                            <h3>🚀 Modern Development</h3>
                            <p>Built with modern web technologies and best practices.</p>
                        </div>
                        <div class="about-card">
                            <h3>📱 Responsive Design</h3>
                            <p>Optimized for all devices and screen sizes.</p>
                        </div>
                        <div class="about-card">
                            <h3>⚡ Fast & Efficient</h3>
                            <p>Performance-optimized with modern build tools.</p>
                        </div>
                    </div>
                </div>
            </section>

            <section id="contact" class="contact-section">
                <div class="container">
                    <h2>Contact Us</h2>
                    <div class="contact-form">
                        <div class="form-group">
                            <label for="name">Name:</label>
                            <input type="text" id="name" name="name" required>
                        </div>
                        <div class="form-group">
                            <label for="email">Email:</label>
                            <input type="email" id="email" name="email" required>
                        </div>
                        <div class="form-group">
                            <label for="message">Message:</label>
                            <textarea id="message" name="message" rows="5" required></textarea>
                        </div>
                        <button type="submit" class="btn btn-primary" onclick="handleSubmit(event)">Send Message</button>
                    </div>
                </div>
            </section>
        </main>

        <footer class="site-footer">
            <div class="container">
                <p>&copy; 2025 ${projectName} - Created with TSI Header Extension</p>
                <p>Transport and Telecommunication Institute - Riga, Latvia</p>
            </div>
        </footer>
    </div>
    <script src="js/script.js"></script>
</body>
</html>`;

    const encoder = new TextEncoder();
    await vscode.workspace.fs.writeFile(fileUri, encoder.encode(content));
}

async function createStylesCss(vscode, projectName, projectUri) {
    const fileUri = vscode.Uri.joinPath(projectUri, 'css', 'styles.css');
    const headerContent = await generateTSIHeaderContent('styles.css', vscode);

    const content = `${headerContent}
/* ${projectName} Styles - Modern Responsive Design */

:root {
    /* Color Palette */
    --primary-color: #007acc;
    --primary-hover: #005999;
    --secondary-color: #6c757d;
    --secondary-hover: #545b62;
    --accent-color: #28a745;
    --danger-color: #dc3545;

    /* Neutral Colors */
    --text-color: #333333;
    --text-light: #666666;
    --text-muted: #999999;
    --background-color: #ffffff;
    --background-light: #f8f9fa;
    --background-dark: #343a40;
    --border-color: #e9ecef;

    /* Spacing */
    --spacing-xs: 0.25rem;
    --spacing-sm: 0.5rem;
    --spacing-md: 1rem;
    --spacing-lg: 1.5rem;
    --spacing-xl: 2rem;
    --spacing-xxl: 3rem;

    /* Typography */
    --font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
    --font-size-xs: 0.75rem;
    --font-size-sm: 0.875rem;
    --font-size-base: 1rem;
    --font-size-lg: 1.125rem;
    --font-size-xl: 1.25rem;
    --font-size-2xl: 1.5rem;
    --font-size-3xl: 1.875rem;
    --font-size-4xl: 2.25rem;

    /* Layout */
    --max-width: 1200px;
    --border-radius: 0.375rem;
    --box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);
    --box-shadow-lg: 0 4px 6px rgba(0, 0, 0, 0.1);

    /* Transitions */
    --transition-fast: 0.15s ease-in-out;
    --transition-normal: 0.3s ease-in-out;

    /* Dark theme variables */
    --dark-primary: #0d7bdc;
    --dark-background: #1a1a1a;
    --dark-text: #ffffff;
    --dark-text-light: #cccccc;
}

/* Global Reset and Base Styles */
* {
    margin: 0;
    padding: 0;
    box-sizing: border-box;
}

html {
    scroll-behavior: smooth;
}

body {
    font-family: var(--font-family);
    line-height: 1.6;
    color: var(--text-color);
    background-color: var(--background-color);
    font-size: var(--font-size-base);
}

/* App container */
#app {
    min-height: 100vh;
    display: flex;
    flex-direction: column;
}

/* Utility Classes */
.container {
    max-width: var(--max-width);
    margin: 0 auto;
    padding: 0 var(--spacing-md);
}

.btn {
    display: inline-block;
    padding: var(--spacing-sm) var(--spacing-lg);
    border: none;
    border-radius: var(--border-radius);
    cursor: pointer;
    font-size: var(--font-size-base);
    font-weight: 500;
    text-decoration: none;
    transition: all var(--transition-normal);
    text-align: center;
}

.btn-primary {
    background-color: var(--primary-color);
    color: white;
}

.btn-primary:hover {
    background-color: var(--primary-hover);
    transform: translateY(-1px);
    box-shadow: var(--box-shadow-lg);
}

.btn-secondary {
    background-color: var(--secondary-color);
    color: white;
}

.btn-secondary:hover {
    background-color: var(--secondary-hover);
}

/* Navigation */
.navbar {
    background-color: var(--background-color);
    box-shadow: var(--box-shadow);
    position: fixed;
    top: 0;
    width: 100%;
    z-index: 1000;
}

.nav-container {
    max-width: var(--max-width);
    margin: 0 auto;
    padding: 0 var(--spacing-md);
    display: flex;
    justify-content: space-between;
    align-items: center;
    height: 70px;
}

.nav-logo {
    font-size: var(--font-size-xl);
    font-weight: bold;
    color: var(--primary-color);
}

.nav-menu {
    display: flex;
    list-style: none;
    gap: var(--spacing-lg);
}

.nav-link {
    text-decoration: none;
    color: var(--text-color);
    font-weight: 500;
    transition: color var(--transition-fast);
}

.nav-link:hover {
    color: var(--primary-color);
}

.hamburger {
    display: none;
    flex-direction: column;
    cursor: pointer;
}

.bar {
    width: 25px;
    height: 3px;
    background-color: var(--text-color);
    margin: 3px 0;
    transition: var(--transition-fast);
}

/* Hero Section */
.hero-section {
    padding: var(--spacing-xxl) var(--spacing-md);
    background: linear-gradient(135deg, var(--primary-color) 0%, var(--primary-hover) 100%);
    color: white;
    text-align: center;
    min-height: 60vh;
    display: flex;
    align-items: center;
}

.hero-container {
    max-width: 800px;
    margin: 0 auto;
}

.hero-section h2 {
    font-size: var(--font-size-4xl);
    margin-bottom: var(--spacing-lg);
    font-weight: 700;
}

.hero-description {
    font-size: var(--font-size-lg);
    margin-bottom: var(--spacing-xl);
    opacity: 0.9;
}

.hero-buttons {
    display: flex;
    gap: var(--spacing-md);
    justify-content: center;
    flex-wrap: wrap;
}

/* About Section */
.about-section {
    padding: var(--spacing-xxl) var(--spacing-md);
    background-color: var(--background-light);
}

.about-section h2 {
    text-align: center;
    font-size: var(--font-size-3xl);
    margin-bottom: var(--spacing-xl);
    color: var(--text-color);
}

.about-grid {
    display: grid;
    grid-template-columns: repeat(auto-fit, minmax(300px, 1fr));
    gap: var(--spacing-lg);
}

.about-card {
    background: white;
    padding: var(--spacing-xl);
    border-radius: var(--border-radius);
    box-shadow: var(--box-shadow);
    text-align: center;
    transition: transform var(--transition-normal);
}

.about-card:hover {
    transform: translateY(-5px);
    box-shadow: var(--box-shadow-lg);
}

.about-card h3 {
    font-size: var(--font-size-xl);
    margin-bottom: var(--spacing-md);
    color: var(--primary-color);
}

/* Contact Section */
.contact-section {
    padding: var(--spacing-xxl) var(--spacing-md);
}

.contact-section h2 {
    text-align: center;
    font-size: var(--font-size-3xl);
    margin-bottom: var(--spacing-xl);
}

.contact-form {
    max-width: 600px;
    margin: 0 auto;
}

.form-group {
    margin-bottom: var(--spacing-lg);
}

.form-group label {
    display: block;
    margin-bottom: var(--spacing-xs);
    font-weight: 500;
}

.form-group input,
.form-group textarea {
    width: 100%;
    padding: var(--spacing-md);
    border: 1px solid var(--border-color);
    border-radius: var(--border-radius);
    font-size: var(--font-size-base);
    font-family: inherit;
    transition: border-color var(--transition-fast);
}

.form-group input:focus,
.form-group textarea:focus {
    outline: none;
    border-color: var(--primary-color);
    box-shadow: 0 0 0 3px rgba(0, 122, 204, 0.1);
}

/* Footer */
.site-footer {
    background-color: var(--background-dark);
    color: white;
    padding: var(--spacing-xl) var(--spacing-md);
    text-align: center;
}

.site-footer p {
    margin: var(--spacing-xs) 0;
    opacity: 0.8;
}

/* Responsive Design */
@media (max-width: 768px) {
    .nav-menu {
        display: none;
    }

    .hamburger {
        display: flex;
    }

    .hero-section h2 {
        font-size: var(--font-size-3xl);
    }

    .hero-buttons {
        flex-direction: column;
        align-items: center;
    }

    .about-grid {
        grid-template-columns: 1fr;
    }

    .nav-container {
        padding: 0 var(--spacing-sm);
    }
}

@media (max-width: 480px) {
    .container {
        padding: 0 var(--spacing-sm);
    }

    .hero-section {
        padding: var(--spacing-xl) var(--spacing-sm);
    }

    .btn {
        width: 100%;
        max-width: 300px;
    }
}

/* Dark Theme */
body.dark-theme {
    --background-color: var(--dark-background);
    --text-color: var(--dark-text);
    --text-light: var(--dark-text-light);
    --background-light: #2a2a2a;
    --border-color: #404040;
}

body.dark-theme .navbar {
    background-color: var(--dark-background);
}

body.dark-theme .about-card {
    background-color: var(--background-light);
}

/* Animations */
@keyframes fadeIn {
    from {
        opacity: 0;
        transform: translateY(20px);
    }
    to {
        opacity: 1;
        transform: translateY(0);
    }
}

.fade-in {
    animation: fadeIn 0.6s ease-out;
}

/* Print Styles */
@media print {
    .navbar,
    .hero-buttons,
    .contact-form {
        display: none !important;
    }

    body {
        font-size: 12pt;
        color: black;
        background: white;
    }
}
`;

    const encoder = new TextEncoder();
    await vscode.workspace.fs.writeFile(fileUri, encoder.encode(content));
}

async function createScriptJs(vscode, projectName, projectUri) {
    const fileUri = vscode.Uri.joinPath(projectUri, 'js', 'script.js');
    const headerContent = await generateTSIHeaderContent('script.js', vscode);

    const content = `${headerContent}
/**
 * ${projectName} - Main JavaScript File
 *
 * This file contains the main JavaScript functionality for the ${projectName} project.
 * Features: Theme switching, form handling, responsive navigation, and modern ES6+ patterns.
 */

// Application state
const AppState = {
    currentTheme: 'light',
    isMenuOpen: false,
    formData: {}
};

/**
 * Initialize the application when DOM is loaded
 */
document.addEventListener('DOMContentLoaded', function() {
    console.log(\`${projectName} loaded successfully!\`);

    // Initialize all application components
    initializeApp();
});

/**
 * Main application initialization
 */
function initializeApp() {
    // Setup event listeners
    setupEventListeners();

    // Initialize components
    initializeComponents();

    // Load saved preferences
    loadUserPreferences();

    // Initialize theme
    initializeTheme();

    // Add loading animation
    addLoadingAnimation();

    console.log('Application initialized successfully');
}

/**
 * Setup all event listeners
 */
function setupEventListeners() {
    // Theme toggle button
    const themeButton = document.querySelector('.btn-secondary');
    if (themeButton && themeButton.textContent.includes('Theme')) {
        themeButton.addEventListener('click', toggleTheme);
    }

    // Alert button
    const alertButton = document.querySelector('.btn-primary');
    if (alertButton && alertButton.textContent.includes('Started')) {
        alertButton.addEventListener('click', showAlert);
    }

    // Contact form
    const contactForm = document.querySelector('.contact-form');
    if (contactForm) {
        contactForm.addEventListener('submit', handleFormSubmit);
    }

    // Navigation links
    const navLinks = document.querySelectorAll('.nav-link');
    navLinks.forEach(link => {
        link.addEventListener('click', handleNavClick);
    });

    // Hamburger menu
    const hamburger = document.querySelector('.hamburger');
    if (hamburger) {
        hamburger.addEventListener('click', toggleMobileMenu);
    }

    // Window resize
    window.addEventListener('resize', handleResize);

    // Scroll events
    window.addEventListener('scroll', handleScroll);
}

/**
 * Initialize UI components
 */
function initializeComponents() {
    // Add fade-in animations to sections
    const sections = document.querySelectorAll('section');
    sections.forEach((section, index) => {
        section.style.opacity = '0';
        section.style.transform = 'translateY(20px)';
        section.style.transition = 'opacity 0.6s ease-out, transform 0.6s ease-out';
        section.style.transitionDelay = index * 0.1 + "s";

        // Trigger animation
        setTimeout(() => {
            section.style.opacity = '1';
            section.style.transform = 'translateY(0)';
        }, 100);
    });

    // Initialize form validation
    initializeFormValidation();
}

/**
 * Initialize form validation
 */
function initializeFormValidation() {
    const inputs = document.querySelectorAll('input, textarea');
    inputs.forEach(input => {
        input.addEventListener('blur', validateField);
        input.addEventListener('input', clearFieldError);
    });
}

/**
 * Validate individual form field
 */
function validateField(event) {
    const field = event.target;
    const value = field.value.trim();
    const fieldName = field.name;

    // Remove existing error message
    clearFieldError(event);

    let isValid = true;
    let errorMessage = '';

    switch (fieldName) {
        case 'name':
            if (!value) {
                isValid = false;
                errorMessage = 'Name is required';
            } else if (value.length < 2) {
                isValid = false;
                errorMessage = 'Name must be at least 2 characters';
            }
            break;

        case 'email':
            const emailRegex = /^[^\s@]+@[^\s@]+\.[^\s@]+$/;
            if (!value) {
                isValid = false;
                errorMessage = 'Email is required';
            } else if (!emailRegex.test(value)) {
                isValid = false;
                errorMessage = 'Please enter a valid email address';
            }
            break;

        case 'message':
            if (!value) {
                isValid = false;
                errorMessage = 'Message is required';
            } else if (value.length < 10) {
                isValid = false;
                errorMessage = 'Message must be at least 10 characters';
            }
            break;
    }

    if (!isValid) {
        showFieldError(field, errorMessage);
    }

    return isValid;
}

/**
 * Show field error message
 */
function showFieldError(field, message) {
    const formGroup = field.closest('.form-group');
    const errorElement = document.createElement('div');
    errorElement.className = 'field-error';
    errorElement.textContent = message;
    errorElement.style.color = 'var(--danger-color)';
    errorElement.style.fontSize = 'var(--font-size-sm)';
    errorElement.style.marginTop = 'var(--spacing-xs)';

    formGroup.appendChild(errorElement);
    field.style.borderColor = 'var(--danger-color)';
}

/**
 * Clear field error message
 */
function clearFieldError(event) {
    const field = event.target;
    const formGroup = field.closest('.form-group');
    const errorElement = formGroup.querySelector('.field-error');

    if (errorElement) {
        errorElement.remove();
    }

    field.style.borderColor = 'var(--border-color)';
}

/**
 * Handle form submission
 */
function handleFormSubmit(event) {
    event.preventDefault();

    const form = event.target;
    const formData = new FormData(form);
    const data = Object.fromEntries(formData.entries());

    // Validate all fields
    const inputs = form.querySelectorAll('input, textarea');
    let isFormValid = true;

    inputs.forEach(input => {
        if (!validateField({ target: input })) {
            isFormValid = false;
        }
    });

    if (!isFormValid) {
        showAlert('Please correct the errors in the form', 'error');
        return;
    }

    // Store form data
    AppState.formData = data;

    // Show success message
    showAlert('Thank you for your message! We will get back to you soon.', 'success');

    // Reset form
    form.reset();

    // Log form submission (in real app, this would be sent to server)
    console.log('Form submitted:', data);
}

/**
 * Handle navigation clicks
 */
function handleNavClick(event) {
    event.preventDefault();
    const targetId = event.target.getAttribute('href').substring(1);
    const targetElement = document.getElementById(targetId);

    if (targetElement) {
        const offsetTop = targetElement.offsetTop - 70; // Account for fixed navbar
        window.scrollTo({
            top: offsetTop,
            behavior: 'smooth'
        });
    }

    // Close mobile menu if open
    if (AppState.isMenuOpen) {
        toggleMobileMenu();
    }
}

/**
 * Toggle mobile menu
 */
function toggleMobileMenu() {
    const navMenu = document.querySelector('.nav-menu');
    const hamburger = document.querySelector('.hamburger');

    if (navMenu) {
        AppState.isMenuOpen = !AppState.isMenuOpen;
        navMenu.style.display = AppState.isMenuOpen ? 'flex' : 'none';

        // Animate hamburger
        if (hamburger) {
            hamburger.classList.toggle('active');
        }
    }
}

/**
 * Handle window resize
 */
function handleResize() {
    // Close mobile menu on desktop
    if (window.innerWidth > 768 && AppState.isMenuOpen) {
        toggleMobileMenu();
    }
}

/**
 * Handle scroll events
 */
function handleScroll() {
    const navbar = document.querySelector('.navbar');
    if (navbar) {
        if (window.scrollY > 100) {
            navbar.style.backgroundColor = 'rgba(255, 255, 255, 0.95)';
            navbar.style.backdropFilter = 'blur(10px)';
        } else {
            navbar.style.backgroundColor = 'var(--background-color)';
            navbar.style.backdropFilter = 'none';
        }
    }
}

/**
 * Toggle between light and dark themes
 */
function toggleTheme() {
    const body = document.body;
    const isDark = body.classList.contains('dark-theme');

    if (isDark) {
        body.classList.remove('dark-theme');
        AppState.currentTheme = 'light';
    } else {
        body.classList.add('dark-theme');
        AppState.currentTheme = 'dark';
    }

    // Save theme preference
    saveUserPreferences();

    // Show theme change feedback
    const themeName = AppState.currentTheme === 'dark' ? 'Dark' : 'Light';
    showAlert(themeName + ' theme activated', 'info');
}

/**
 * Initialize theme based on saved preference or system preference
 */
function initializeTheme() {
    const savedTheme = localStorage.getItem('theme');
    const prefersDark = window.matchMedia('(prefers-color-scheme: dark)').matches;

    if (savedTheme) {
        AppState.currentTheme = savedTheme;
    } else if (prefersDark) {
        AppState.currentTheme = 'dark';
    }

    if (AppState.currentTheme === 'dark') {
        document.body.classList.add('dark-theme');
    }
}

/**
 * Save user preferences to localStorage
 */
function saveUserPreferences() {
    try {
        localStorage.setItem('theme', AppState.currentTheme);
    } catch (error) {
        console.warn('Could not save user preferences:', error);
    }
}

/**
 * Load user preferences from localStorage
 */
function loadUserPreferences() {
    try {
        const savedTheme = localStorage.getItem('theme');
        if (savedTheme) {
            AppState.currentTheme = savedTheme;
        }
    } catch (error) {
        console.warn('Could not load user preferences:', error);
    }
}

/**
 * Show alert/notification
 */
function showAlert(message, type = 'info') {
    // Remove existing alerts
    const existingAlerts = document.querySelectorAll('.alert');
    existingAlerts.forEach(alert => alert.remove());

    // Create new alert
    const alert = document.createElement('div');
    alert.className = 'alert alert-' + type;
    alert.textContent = message;

    // Style the alert
    Object.assign(alert.style, {
        position: 'fixed',
        top: '20px',
        right: '20px',
        padding: '1rem 1.5rem',
        borderRadius: 'var(--border-radius)',
        color: 'white',
        fontWeight: '500',
        zIndex: '10000',
        maxWidth: '400px',
        boxShadow: 'var(--box-shadow-lg)',
        opacity: '0',
        transform: 'translateX(100%)',
        transition: 'all 0.3s ease-out'
    });

    // Set background color based on type
    const colors = {
        success: '#28a745',
        error: '#dc3545',
        warning: '#ffc107',
        info: '#007acc'
    };
    alert.style.backgroundColor = colors[type] || colors.info;

    // Add to page
    document.body.appendChild(alert);

    // Animate in
    setTimeout(() => {
        alert.style.opacity = '1';
        alert.style.transform = 'translateX(0)';
    }, 10);

    // Auto remove after 5 seconds
    setTimeout(() => {
        alert.style.opacity = '0';
        alert.style.transform = 'translateX(100%)';
        setTimeout(() => {
            if (alert.parentNode) {
                alert.parentNode.removeChild(alert);
            }
        }, 300);
    }, 5000);
}

/**
 * Add loading animation
 */
function addLoadingAnimation() {
    const app = document.getElementById('app');
    if (app) {
        app.style.opacity = '0';
        app.style.transition = 'opacity 0.5s ease-in-out';

        setTimeout(() => {
            app.style.opacity = '1';
        }, 100);
    }
}

/**
 * Utility functions for external use
 */
const Utils = {
    /**
     * Debounce function calls
     */
    debounce: function(func, wait) {
        let timeout;
        return function executedFunction(...args) {
            const later = () => {
                clearTimeout(timeout);
                func(...args);
            };
            clearTimeout(timeout);
            timeout = setTimeout(later, wait);
        };
    },

    /**
     * Throttle function calls
     */
    throttle: function(func, limit) {
        let inThrottle;
        return function() {
            const args = arguments;
            const context = this;
            if (!inThrottle) {
                func.apply(context, args);
                inThrottle = true;
                setTimeout(() => inThrottle = false, limit);
            }
        };
    },

    /**
     * Check if element is in viewport
     */
    isInViewport: function(element) {
        const rect = element.getBoundingClientRect();
        return (
            rect.top >= 0 &&
            rect.left >= 0 &&
            rect.bottom <= (window.innerHeight || document.documentElement.clientHeight) &&
            rect.right <= (window.innerWidth || document.documentElement.clientWidth)
        );
    }
};

// Export functions for potential module usage or testing
if (typeof module !== 'undefined' && module.exports) {
    module.exports = {
        initializeApp,
        setupEventListeners,
        initializeComponents,
        toggleTheme,
        showAlert,
        handleFormSubmit,
        validateField,
        Utils,
        AppState
    };
}

// Legacy function for backward compatibility
function showAlert(message) {
    showAlert(message, 'info');
}

function handleSubmit(event) {
    handleFormSubmit(event);
}
`;

    const encoder = new TextEncoder();
    await vscode.workspace.fs.writeFile(fileUri, encoder.encode(content));
}

async function createGitIgnore(vscode, projectName, projectUri) {
    const fileUri = vscode.Uri.joinPath(projectUri, '.gitignore');
    const content = `# Dependencies
node_modules/
npm-debug.log*
yarn-debug.log*
yarn-error.log*

# Production builds
dist/
build/

# Environment variables
.env
.env.local
.env.development.local
.env.test.local
.env.production.local

# IDE and editor files
.vscode/
.idea/
*.swp
*.swo
*~

# OS generated files
.DS_Store
.DS_Store?
._*
.Spotlight-V100
.Trashes
ehthumbs.db
Thumbs.db

# Logs
logs
*.log

# Runtime data
pids
*.pid
*.seed
*.pid.lock

# Coverage directory used by tools like istanbul
coverage/
*.lcov

# nyc test coverage
.nyc_output

# Dependency directories
jspm_packages/

# Optional npm cache directory
.npm

# Optional REPL history
.node_repl_history

# Output of 'npm pack'
*.tgz

# Yarn Integrity file
.yarn-integrity

# parcel-bundler cache (https://parceljs.org/)
.cache
.parcel-cache

# next.js build output
.next

# nuxt.js build output
.nuxt

# vuepress build output
.vuepress/dist

# Serverless directories
.serverless

# FuseBox cache
.fusebox/

# DynamoDB Local files
.dynamodb/

# TernJS port file
.tern-port

# Stores VSCode versions used for testing VSCode extensions
.vscode-test

# Temporary folders
tmp/
temp/
`;

    const encoder = new TextEncoder();
    await vscode.workspace.fs.writeFile(fileUri, encoder.encode(content));
}

async function createPackageJson(vscode, projectName, projectUri) {
    const fileUri = vscode.Uri.joinPath(projectUri, 'package.json');

    const packageName = projectName.toLowerCase().replace(/[^a-z0-9-]/g, '-');
    const content = `{
  "name": "${packageName}",
  "version": "1.0.0",
  "description": "${projectName} - A modern HTML/CSS/JavaScript project created with TSI Header extension",
  "main": "js/script.js",
  "scripts": {
    "start": "live-server --port=3000 --open=./",
    "dev": "webpack serve --mode development --open",
    "build": "webpack --mode production",
    "lint": "eslint js/script.js",
    "lint:fix": "eslint js/script.js --fix",
    "format": "prettier --write js/script.js css/styles.css",
    "test": "echo \\"No tests specified\\" && exit 0",
    "clean": "rm -rf dist node_modules/.cache"
  },
  "keywords": [
    "html",
    "css",
    "javascript",
    "webpack",
    "live-server",
    "tsi",
    "education"
  ],
  "author": "TSI Student",
  "license": "MIT",
  "devDependencies": {
    "@babel/core": "^7.20.0",
    "@babel/preset-env": "^7.20.0",
    "babel-loader": "^9.1.0",
    "css-loader": "^6.7.0",
    "eslint": "^8.30.0",
    "html-webpack-plugin": "^5.5.0",
    "live-server": "^1.2.2",
    "prettier": "^2.8.0",
    "style-loader": "^3.3.0",
    "webpack": "^5.75.0",
    "webpack-cli": "^5.0.0",
    "webpack-dev-server": "^4.11.0"
  },
  "repository": {
    "type": "git",
    "url": "https://github.com/tsi-student/${packageName}.git"
  },
  "bugs": {
    "url": "https://github.com/tsi-student/${packageName}/issues"
  },
  "homepage": "https://github.com/tsi-student/${packageName}#readme"
}
`;

    const encoder = new TextEncoder();
    await vscode.workspace.fs.writeFile(fileUri, encoder.encode(content));
}

async function createReadme(vscode, projectName, projectUri) {
    const fileUri = vscode.Uri.joinPath(projectUri, 'README.md');
    const headerContent = await generateTSIHeaderContent('README.md', vscode);

    const content = `${headerContent}
# ${projectName}

**Transport and Telecommunication Institute - Modern Web Development Project**

![TSI Logo](https://tsi.lv/themes/custom/tsi/logo.svg)

## 📋 Project Information

- **Technology Stack**: HTML5, CSS3, ES6+ JavaScript
- **Build Tools**: Webpack, Babel, ESLint, Prettier
- **Author**: TSI Student
- **Institution**: Transport and Telecommunication Institute (TSI)
- **Website**: [https://tsi.lv](https://tsi.lv)

## 🚀 Usage

### Prerequisites

Make sure you have the following installed:
- **Node.js**: \`node --version\` (recommended: Node.js 16.0+)
- **npm**: \`npm --version\` (usually included with Node.js)
- **Git**: \`git --version\` (for version control)

### Installation

1. Clone or download this project
2. Navigate to the project directory
3. Install dependencies:

\`\`\`bash
npm install
\`\`\`

### Development

Start the development server with live reloading:

\`\`\`bash
npm start
\`\`\`

This will open your browser to \`http://localhost:3000\` and automatically reload when you make changes.

### Build for Production

\`\`\`bash
npm run build
\`\`\`

The built files will be in the \`dist/\` directory.

## 📁 Project Structure

\`\`\`
${projectName}/
├── index.html                 # Main HTML page with TSI header
├── css/
│   └── styles.css            # Modern CSS with responsive design
├── js/
│   └── script.js             # Main JavaScript with ES6+ features
├── package.json              # Node.js dependencies and scripts
├── webpack.config.js         # Webpack build configuration
├── .eslintrc.js             # ESLint configuration
├── .prettierrc              # Prettier code formatting
├── .gitignore               # Git ignore patterns
├── README.md                 # This file
└── dist/                     # Production build output (generated)
\`\`\`

## 🛠️ Development Workflow

### Code Quality

- **Linting**: \`npm run lint\` - Check code quality with ESLint
- **Auto-fix**: \`npm run lint:fix\` - Automatically fix linting issues
- **Formatting**: \`npm run format\` - Format code with Prettier

### Available Scripts

- \`npm start\` - Start development server
- \`npm run dev\` - Start webpack dev server
- \`npm run build\` - Build for production
- \`npm run lint\` - Run ESLint
- \`npm run lint:fix\` - Fix ESLint issues
- \`npm run format\` - Format code
- \`npm run clean\` - Clean build artifacts

## 🎨 Features

### Modern Web Technologies
- ✅ **HTML5**: Semantic markup with accessibility features
- ✅ **CSS3**: Modern CSS with CSS variables, flexbox, and grid
- ✅ **ES6+ JavaScript**: Modern JavaScript with async/await, modules, and classes
- ✅ **Responsive Design**: Mobile-first approach with media queries

### Development Tools
- ✅ **Webpack**: Module bundling and asset optimization
- ✅ **Babel**: JavaScript transpilation for browser compatibility
- ✅ **ESLint**: Code quality and consistency
- ✅ **Prettier**: Automatic code formatting
- ✅ **Live Server**: Development server with hot reloading

### User Experience
- ✅ **Theme Switching**: Light/dark theme toggle with localStorage persistence
- ✅ **Form Validation**: Client-side form validation with error messages
- ✅ **Responsive Navigation**: Mobile-friendly hamburger menu
- ✅ **Smooth Animations**: CSS transitions and JavaScript animations
- ✅ **Accessibility**: ARIA labels and keyboard navigation

## 🎓 TSI Academic Requirements

This project includes all required elements for TSI web development courses:

- ✅ **Professional Headers**: All source files include TSI institutional headers
- ✅ **Modern Technologies**: HTML5, CSS3, ES6+ JavaScript
- ✅ **Build System**: Complete webpack configuration with development and production builds
- ✅ **Code Quality**: ESLint and Prettier configuration
- ✅ **Documentation**: Comprehensive README and code comments
- ✅ **Version Control**: Git-ready with appropriate .gitignore

## 📚 Learning Resources

### Web Development Fundamentals
- [MDN Web Docs](https://developer.mozilla.org/) - Comprehensive web development reference
- [HTML5 Specification](https://html.spec.whatwg.org/) - Official HTML5 standard
- [CSS Specification](https://www.w3.org/TR/CSS/) - Official CSS standards
- [ECMAScript Specification](https://tc39.es/ecma262/) - JavaScript language specification

### JavaScript
- [JavaScript.info](https://javascript.info/) - Modern JavaScript tutorial
- [Eloquent JavaScript](https://eloquentjavascript.net/) - Free JavaScript book
- [You Don't Know JS](https://github.com/getify/You-Dont-Know-JS) - Deep JavaScript knowledge

### CSS
- [CSS-Tricks](https://css-tricks.com/) - CSS tutorials and techniques
- [A Complete Guide to Flexbox](https://css-tricks.com/snippets/css/a-guide-to-flexbox/)
- [A Complete Guide to Grid](https://css-tricks.com/snippets/css/complete-guide-grid/)

### Tools and Frameworks
- [Webpack Documentation](https://webpack.js.org/concepts/)
- [Babel Documentation](https://babeljs.io/docs/en/)
- [ESLint Documentation](https://eslint.org/docs/user-guide/)
- [Prettier Documentation](https://prettier.io/docs/en/)

### TSI Resources
- [TSI Official Website](https://tsi.lv)
- [Web Development Courses](https://tsi.lv/study)
- [Programming Study Programs](https://tsi.lv/study/undergraduate)

## 🤝 Contributing

If this is a collaborative project, please:

1. Fork the repository
2. Create a feature branch (\`git checkout -b feature/amazing-feature\`)
3. Commit your changes with TSI headers (\`git commit -m 'Add amazing feature'\`)
4. Push to the branch (\`git push origin feature/amazing-feature\`)
5. Open a Pull Request

## 📄 License

This project is created for educational purposes at Transport and Telecommunication Institute (TSI). Please respect TSI's academic integrity policies.

## 📞 Contact

- **Author**: TSI Student
- **Institution**: Transport and Telecommunication Institute
- **Address**: Lomonosova 1, Riga, LV-1019, Latvia
- **Website**: [https://tsi.lv](https://tsi.lv)

---

*Generated by [TSI Header Extension](https://marketplace.visualstudio.com/items?itemName=st93642.tsi-header) for Visual Studio Code*

**🎓 Excellence in Technical Education - Transport and Telecommunication Institute**
`;

    const encoder = new TextEncoder();
    await vscode.workspace.fs.writeFile(fileUri, encoder.encode(content));
}

async function createWebpackConfig(vscode, projectName, projectUri) {
    const fileUri = vscode.Uri.joinPath(projectUri, 'webpack.config.js');
    const headerContent = await generateTSIHeaderContent('webpack.config.js', vscode);

    const content = `${headerContent}
const path = require('path');
const HtmlWebpackPlugin = require('html-webpack-plugin');

module.exports = {
  // Entry point - main JavaScript file
  entry: './js/script.js',

  // Mode configuration
  mode: 'development',

  // Output configuration
  output: {
    filename: 'bundle.[contenthash].js',
    path: path.resolve(__dirname, 'dist'),
    clean: true, // Clean dist folder before each build
    publicPath: '/'
  },

  // Module rules for different file types
  module: {
    rules: [
      {
        test: /\\.js$/i,
        exclude: /node_modules/,
        use: {
          loader: 'babel-loader',
          options: {
            presets: ['@babel/preset-env']
          }
        }
      },
      {
        test: /\\.css$/i,
        use: ['style-loader', 'css-loader']
      },
      {
        test: /\\.(png|svg|jpg|jpeg|gif)$/i,
        type: 'asset/resource',
        generator: {
          filename: 'images/[name].[hash][ext]'
        }
      },
      {
        test: /\\.(woff|woff2|eot|ttf|otf)$/i,
        type: 'asset/resource',
        generator: {
          filename: 'fonts/[name].[hash][ext]'
        }
      }
    ]
  },

  // Plugins
  plugins: [
    new HtmlWebpackPlugin({
      template: './index.html',
      filename: 'index.html',
      inject: 'body',
      minify: {
        collapseWhitespace: true,
        removeComments: true,
        removeRedundantAttributes: true,
        removeScriptTypeAttributes: true,
        removeStyleLinkTypeAttributes: true,
        useShortDoctype: true
      }
    })
  ],

  // Development server configuration
  devServer: {
    static: {
      directory: path.join(__dirname, 'dist'),
    },
    compress: true,
    port: 8080,
    open: true,
    hot: true,
    historyApiFallback: true
  },

  // Source maps for debugging
  devtool: 'source-map',

  // Optimization settings
  optimization: {
    splitChunks: {
      chunks: 'all',
      cacheGroups: {
        vendor: {
          test: /[\\\\/]node_modules[\\\\/]/,
          name: 'vendors',
          chunks: 'all'
        }
      }
    }
  },

  // Resolve configuration
  resolve: {
    extensions: ['.js', '.json'],
    alias: {
      '@': path.resolve(__dirname, 'js'),
      '~': path.resolve(__dirname, 'css')
    }
  },

  // Performance hints
  performance: {
    hints: 'warning',
    maxAssetSize: 512000, // 500 KB
    maxEntrypointSize: 512000 // 500 KB
  }
};
`;

    const encoder = new TextEncoder();
    await vscode.workspace.fs.writeFile(fileUri, encoder.encode(content));
}

async function createEslintConfig(vscode, projectName, projectUri) {
    const fileUri = vscode.Uri.joinPath(projectUri, '.eslintrc.js');
    const headerContent = await generateTSIHeaderContent('.eslintrc.js', vscode);

    const content = `${headerContent}
module.exports = {
  // Environment settings
  env: {
    browser: true,
    es2021: true,
    node: true
  },

  // Extend existing configurations
  extends: [
    'eslint:recommended'
  ],

  // Parser options
  parserOptions: {
    ecmaVersion: 'latest',
    sourceType: 'module'
  },

  // Custom rules
  rules: {
    // Possible Errors
    'no-console': 'warn',
    'no-debugger': 'error',
    'no-duplicate-imports': 'error',
    'no-template-curly-in-string': 'error',
    'no-unreachable': 'error',
    'no-unsafe-negation': 'error',

    // Best Practices
    'array-callback-return': 'error',
    'block-scoped-var': 'error',
    'complexity': ['warn', 10],
    'consistent-return': 'error',
    'curly': ['error', 'all'],
    'default-case': 'error',
    'dot-notation': 'error',
    'eqeqeq': ['error', 'always'],
    'guard-for-in': 'error',
    'no-alert': 'warn',
    'no-caller': 'error',
    'no-else-return': 'error',
    'no-empty-function': 'warn',
    'no-eval': 'error',
    'no-extend-native': 'error',
    'no-extra-bind': 'error',
    'no-floating-decimal': 'error',
    'no-global-assign': 'error',
    'no-implicit-coercion': 'error',
    'no-implicit-globals': 'error',
    'no-implied-eval': 'error',
    'no-invalid-this': 'error',
    'no-iterator': 'error',
    'no-labels': 'error',
    'no-lone-blocks': 'error',
    'no-loop-func': 'error',
    'no-multi-spaces': 'error',
    'no-multi-str': 'error',
    'no-new': 'error',
    'no-new-func': 'error',
    'no-new-wrappers': 'error',
    'no-octal': 'error',
    'no-octal-escape': 'error',
    'no-param-reassign': 'error',
    'no-proto': 'error',
    'no-return-assign': 'error',
    'no-return-await': 'error',
    'no-script-url': 'error',
    'no-self-compare': 'error',
    'no-sequences': 'error',
    'no-throw-literal': 'error',
    'no-unmodified-loop-condition': 'error',
    'no-unused-expressions': 'error',
    'no-useless-call': 'error',
    'no-useless-concat': 'error',
    'no-useless-return': 'error',
    'no-void': 'error',
    'no-with': 'error',
    'prefer-promise-reject-errors': 'error',
    'radix': 'error',
    'require-await': 'error',
    'vars-on-top': 'error',
    'wrap-iife': ['error', 'any'],
    'yoda': 'error',

    // Variables
    'no-delete-var': 'error',
    'no-shadow': 'error',
    'no-shadow-restricted-names': 'error',
    'no-undef': 'error',
    'no-undefined': 'error',
    'no-unused-vars': ['error', { argsIgnorePattern: '^_' }],
    'no-use-before-define': ['error', { functions: false }],

    // Stylistic Issues
    'array-bracket-newline': ['error', 'consistent'],
    'array-bracket-spacing': ['error', 'never'],
    'array-element-newline': ['error', 'consistent'],
    'block-spacing': ['error', 'always'],
    'brace-style': ['error', '1tbs', { allowSingleLine: true }],
    'camelcase': ['error', { properties: 'never' }],
    'comma-dangle': ['error', 'never'],
    'comma-spacing': ['error', { before: false, after: true }],
    'comma-style': ['error', 'last'],
    'computed-property-spacing': ['error', 'never'],
    'consistent-this': ['error', 'that'],
    'eol-last': ['error', 'always'],
    'func-call-spacing': ['error', 'never'],
    'func-name-matching': 'error',
    'func-names': ['error', 'as-needed'],
    'func-style': ['error', 'declaration', { allowArrowFunctions: true }],
    'function-call-argument-newline': ['error', 'consistent'],
    'function-paren-newline': ['error', 'consistent'],
    'implicit-arrow-linebreak': ['error', 'beside'],
    'indent': ['error', 4, { SwitchCase: 1 }],
    'jsx-quotes': ['error', 'prefer-double'],
    'key-spacing': ['error', { beforeColon: false, afterColon: true }],
    'keyword-spacing': ['error', { before: true, after: true }],
    'linebreak-style': ['error', 'unix'],
    'lines-around-comment': ['error', { beforeBlockComment: true, afterBlockComment: false }],
    'lines-between-class-members': ['error', 'always'],
    'max-depth': ['error', 4],
    'max-len': ['error', { code: 120, ignoreUrls: true, ignoreStrings: true }],
    'max-lines': ['warn', 300],
    'max-lines-per-function': ['warn', 50],
    'max-nested-callbacks': ['error', 3],
    'max-params': ['warn', 4],
    'multiline-comment-style': ['error', 'starred-block'],
    'new-cap': ['error', { newIsCap: true, capIsNew: false }],
    'new-parens': 'error',
    'newline-per-chained-call': ['error', { ignoreChainWithDepth: 2 }],
    'no-array-constructor': 'error',
    'no-bitwise': 'error',
    'no-continue': 'error',
    'no-inline-comments': 'off',
    'no-lonely-if': 'error',
    'no-mixed-operators': 'error',
    'no-mixed-spaces-and-tabs': 'error',
    'no-multi-assign': 'error',
    'no-multiple-empty-lines': ['error', { max: 2, maxBOF: 0, maxEOF: 1 }],
    'no-negated-condition': 'error',
    'no-nested-ternary': 'error',
    'no-new-object': 'error',
    'no-plusplus': 'error',
    'no-restricted-syntax': [
      'error',
      'ForInStatement',
      'LabeledStatement',
      'WithStatement'
    ],
    'no-tabs': 'error',
    'no-trailing-spaces': 'error',
    'no-underscore-dangle': 'error',
    'no-unneeded-ternary': 'error',
    'no-whitespace-before-property': 'error',
    'nonblock-statement-body-position': ['error', 'beside'],
    'object-curly-newline': ['error', { consistent: true }],
    'object-curly-spacing': ['error', 'always'],
    'object-property-newline': ['error', { allowAllPropertiesOnSameLine: true }],
    'one-var': ['error', 'never'],
    'one-var-declaration-per-line': ['error', 'always'],
    'operator-assignment': ['error', 'always'],
    'operator-linebreak': ['error', 'before'],
    'padded-blocks': ['error', 'never'],
    'padding-line-between-statements': [
      'error',
      { blankLine: 'always', prev: '*', next: 'return' },
      { blankLine: 'always', prev: ['const', 'let', 'var'], next: '*' },
      { blankLine: 'any', prev: ['const', 'let', 'var'], next: ['const', 'let', 'var'] }
    ],
    'prefer-arrow-callback': 'error',
    'prefer-const': 'error',
    'prefer-destructuring': ['error', 'always'],
    'prefer-named-capture-group': 'error',
    'prefer-numeric-literals': 'error',
    'prefer-object-spread': 'error',
    'prefer-rest-params': 'error',
    'prefer-spread': 'error',
    'prefer-template': 'error',
    'quote-props': ['error', 'as-needed'],
    'quotes': ['error', 'single', { avoidEscape: true }],
    'require-yield': 'error',
    'rest-spread-spacing': ['error', 'never'],
    'semi': ['error', 'always'],
    'semi-spacing': ['error', { before: false, after: true }],
    'semi-style': ['error', 'last'],
    'sort-keys': 'off',
    'sort-vars': 'error',
    'space-before-blocks': ['error', 'always'],
    'space-before-function-paren': ['error', 'never'],
    'space-in-parens': ['error', 'never'],
    'space-infix-ops': 'error',
    'space-unary-ops': 'error',
    'spaced-comment': ['error', 'always'],
    'switch-colon-spacing': ['error', { after: true, before: false }],
    'template-tag-spacing': ['error', 'never'],
    'unicode-bom': ['error', 'never'],
    'wrap-regex': 'off'
  },

  // Global variables
  globals: {
    console: 'readonly',
    process: 'readonly',
    Buffer: 'readonly',
    __dirname: 'readonly',
    __filename: 'readonly',
    global: 'readonly',
    window: 'readonly',
    document: 'readonly',
    navigator: 'readonly',
    localStorage: 'readonly',
    sessionStorage: 'readonly',
    fetch: 'readonly',
    setTimeout: 'readonly',
    clearTimeout: 'readonly',
    setInterval: 'readonly',
    clearInterval: 'readonly',
    requestAnimationFrame: 'readonly',
    cancelAnimationFrame: 'readonly'
  }
};
`;

    const encoder = new TextEncoder();
    await vscode.workspace.fs.writeFile(fileUri, encoder.encode(content));
}

async function createPrettierConfig(vscode, projectName, projectUri) {
    const fileUri = vscode.Uri.joinPath(projectUri, '.prettierrc');
    const content = `{
  "semi": true,
  "trailingComma": "none",
  "singleQuote": true,
  "printWidth": 120,
  "tabWidth": 4,
  "useTabs": false,
  "bracketSpacing": true,
  "bracketSameLine": false,
  "arrowParens": "avoid",
  "endOfLine": "lf",
  "quoteProps": "as-needed",
  "jsxSingleQuote": false,
  "embeddedLanguageFormatting": "auto"
}
`;

    const encoder = new TextEncoder();
    await vscode.workspace.fs.writeFile(fileUri, encoder.encode(content));
}

module.exports = {
    createHtmlFiles,
    createIndexHtml,
    createStylesCss,
    createScriptJs,
    createGitIgnore,
    createPackageJson,
    createReadme,
    createWebpackConfig,
    createEslintConfig,
    createPrettierConfig
};