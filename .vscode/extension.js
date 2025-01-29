const vscode = require('vscode');
const { spawn } = require('child_process');

function activate(context) {
  const formatter = {
    async provideDocumentFormattingEdits(document) {
      const config = vscode.workspace.getConfiguration('pgformat');
      const command = config.get('path', 'pgformat');

      return new Promise((resolve, reject) => {
        const input = document.getText();
        const proc = spawn(command, [], { stdio: ['pipe', 'pipe', 'pipe'] });

        let formatted = '';
        let errorOutput = '';

        proc.stdout.on('data', (data) => (formatted += data.toString()));
        proc.stderr.on('data', (data) => (errorOutput += data.toString()));

        proc.on('error', (err) => {
          vscode.window.showErrorMessage(`Formatter error: ${err.message}`);
          reject();
        });

        proc.on('close', (code) => {
          if (code !== 0) {
            vscode.window.showErrorMessage(`Formatter failed (${code}): ${errorOutput}`);
            return reject();
          }

          if (formatted === input) return resolve([]);

          const fullRange = new vscode.Range(
            document.positionAt(0),
            document.positionAt(input.length)
          );

          resolve([vscode.TextEdit.replace(fullRange, formatted)]);
        });

        proc.stdin.write(input);
        proc.stdin.end();
      });
    }
  };

  context.subscriptions.push(
    vscode.languages.registerDocumentFormattingEditProvider(
      { language: 'sql' }, // Match the language ID here
      formatter
    )
  );
}

exports.activate = activate;

function deactivate() { }

module.exports = { activate, deactivate };
