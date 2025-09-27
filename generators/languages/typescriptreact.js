/**
 * TypeScript React Code Base Generator
 * Generates boilerplate TypeScript React components
 */

function generateTypeScriptReactCodeBase() {
    return `\nimport React from 'react';\nimport ReactDOM from 'react-dom/client';\n\n// Interface definitions\ninterface AppProps {}\n\ninterface AppState {\n  message: string;\n  count: number;\n}\n\n/**\n * Main React component\n */\nfunction App(): JSX.Element {\n  const [state, setState] = React.useState<AppState>({\n    message: 'Hello, World!',\n    count: 0\n  });\n\n  const handleIncrement = (): void => {\n    setState(prev => ({ ...prev, count: prev.count + 1 }));\n  };\n\n  const handleMessageChange = (newMessage: string): void => {\n    setState(prev => ({ ...prev, message: newMessage }));\n  };\n\n  return (\n    <div className="app">\n      <header className="app-header">\n        <h1>{state.message}</h1>\n        <p>This is a basic TypeScript React application.</p>\n        <p>Count: {state.count}</p>\n      </header>\n      <main>\n        <section>\n          <h2>Welcome to TSI</h2>\n          <p>Transport and Telecommunication Institute</p>\n          <button onClick={handleIncrement}>\n            Increment Counter\n          </button>\n          <button onClick={() => handleMessageChange('Hello, TSI Student!')}>\n            Change Message\n          </button>\n        </section>\n      </main>\n    </div>\n  );\n}\n\n/**\n * Render the application\n */\nconst root = ReactDOM.createRoot(\n  document.getElementById('root') as HTMLElement\n);\nroot.render(<App />);\n\nexport default App;\n`;
}

module.exports = {
    generateTypeScriptReactCodeBase
};