#!/usr/bin/env ruby

# Compile script for TSI Header Ruby extension
# This script prepares the Ruby code for VS Code integration

require 'json'
require 'fileutils'

puts "Compiling TSI Header Ruby extension..."

# Create output directory
output_dir = File.join(__dir__, '..', 'out')
FileUtils.mkdir_p(output_dir)

# Create a JavaScript wrapper that calls the Ruby extension
js_wrapper = <<~JS
  const vscode = require('vscode');
  const { execSync } = require('child_process');
  const path = require('path');

  function activate(context) {
      console.log('TSI Header extension is now active!');

      // Register insert header command
      const insertHeaderCommand = vscode.commands.registerCommand('tsiheader.insertHeader', async () => {
          const editor = vscode.window.activeTextEditor;
          if (!editor) {
              vscode.window.showErrorMessage('No active editor found');
              return;
          }

          try {
              const document = editor.document;
              const languageId = document.languageId;
              const fileName = document.fileName;
              
              // Get configuration
              const config = vscode.workspace.getConfiguration('tsiheader');
              const username = config.get('username');
              const email = config.get('email');
              
              // Validate configuration for insert command
              if (!username || !email) {
                  vscode.window.showErrorMessage('Please configure your username and email in VS Code settings (tsiheader.username and tsiheader.email) or ensure git config is set');
                  return;
              }
              
              // Get Ruby CLI path
              const extensionPath = context.extensionPath;
              const cliPath = path.join(extensionPath, 'lib', 'tsi_header_cli.rb');
              
              // Set environment variables for configuration
              const env = {
                  ...process.env,
                  TSI_USERNAME: username,
                  TSI_EMAIL: email
              };
              
              // Execute Ruby CLI
              const command = `ruby "\${cliPath}" insert "\${languageId}" "\${fileName}"`;
              console.log('Executing command:', command);
              const result = execSync(command, { encoding: 'utf8', cwd: extensionPath, env: env });
              console.log('CLI result:', result);
              const response = JSON.parse(result);
              
              if (response.success) {
                  // Insert header at the beginning of the document
                  const firstLine = document.lineAt(0);
                  const insertPosition = new vscode.Position(0, 0);
                  
                  await editor.edit(editBuilder => {
                      editBuilder.insert(insertPosition, response.header + '\\n');
                  });
                  
                  vscode.window.showInformationMessage('TSI Header inserted successfully!');
              } else {
                  vscode.window.showErrorMessage(`Failed to insert header: \${response.message}`);
              }
          } catch (error) {
              vscode.window.showErrorMessage(`Error: \${error.message}`);
          }
      });

      // Register update header command
      const updateHeaderCommand = vscode.commands.registerCommand('tsiheader.updateHeader', async () => {
          const editor = vscode.window.activeTextEditor;
          if (!editor) {
              vscode.window.showErrorMessage('No active editor found');
              return;
          }

          try {
              const document = editor.document;
              const languageId = document.languageId;
              const fileName = document.fileName;
              
              // Get configuration
              const config = vscode.workspace.getConfiguration('tsiheader');
              const username = config.get('username');
              const email = config.get('email');
              
              // Validate configuration for update command
              if (!username || !email) {
                  vscode.window.showErrorMessage('Please configure your username and email in VS Code settings (tsiheader.username and tsiheader.email) or ensure git config is set');
                  return;
              }
              
              // Get Ruby CLI path
              const extensionPath = context.extensionPath;
              const cliPath = path.join(extensionPath, 'lib', 'tsi_header_cli.rb');
              
              // Set environment variables for configuration
              const env = {
                  ...process.env,
                  TSI_USERNAME: username,
                  TSI_EMAIL: email
              };
              
              // Execute Ruby CLI for update
              const command = `ruby "\${cliPath}" update "\${languageId}" "\${fileName}"`;
              console.log('Executing update command:', command);
              const result = execSync(command, { encoding: 'utf8', cwd: extensionPath, env: env });
              console.log('Update CLI result:', result);
              const response = JSON.parse(result);
              
              if (response.success) {
                  vscode.window.showInformationMessage('TSI Header updated successfully!');
              } else {
                  vscode.window.showErrorMessage(`Failed to update header: \${response.message}`);
              }
          } catch (error) {
              vscode.window.showErrorMessage(`Error: \${error.message}`);
          }
      });

      context.subscriptions.push(insertHeaderCommand);
      context.subscriptions.push(updateHeaderCommand);
  }

  function deactivate() {}

  module.exports = {
      activate,
      deactivate
  };
JS

# Copy our current, working extension.js and CLI files instead of generating static ones
puts "Copying current working files..."

# Copy the current extension.js that has our fixes
FileUtils.cp(File.join(__dir__, '..', 'core', 'src', 'extension.js'), File.join(output_dir, 'extension.js'))

# The CLI file is already in the right place, no need to overwrite it
puts "✓ Copied JavaScript extension from src/extension.js"
puts "✓ Using current Ruby CLI interface at core/lib/tsi_header_cli.rb"
puts "✓ Compilation complete!"