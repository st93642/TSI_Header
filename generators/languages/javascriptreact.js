/**
 * JavaScript React Code Base Generator
 * Generates boilerplate React components in JavaScript
 */

function generateJavaScriptReactCodeBase() {
    return `\nimport React from 'react';\nimport ReactDOM from 'react-dom/client';\n\n/**\n * Main React component\n */\nfunction App() {\n  return (\n    <div className="app">\n      <header className="app-header">\n        <h1>Hello, World!</h1>\n        <p>This is a basic React application.</p>\n      </header>\n      <main>\n        <section>\n          <h2>Welcome to TSI</h2>\n          <p>Transport and Telecommunication Institute</p>\n          <p>Building the future of technology education.</p>\n        </section>\n      </main>\n    </div>\n  );\n}\n\n/**\n * Render the application\n */\nconst root = ReactDOM.createRoot(document.getElementById('root'));\nroot.render(<App />);\n\nexport default App;\n`;
}

module.exports = {
    generateJavaScriptReactCodeBase
};