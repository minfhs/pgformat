# Install formatter for VSCode

```sh
cd .vscode
npm install -g vsce
vsce package
code --install-extension pgformat-0.0.1.vsix
```

You may need to configure your workspace as trusted before it works.
Now, open settings as json (C-S-p "user json"):

```json
{
  "security.workspace.trust.untrustedFiles": "open",
  "[sql]": {
    "editor.defaultFormatter": "minfhs.pgformat",
    "editor.formatOnSave": true,
    "editor.formatOnPaste": true
  }
}
```
