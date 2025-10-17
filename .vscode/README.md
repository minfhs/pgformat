# pgformat VS Code Extension

A VS Code extension that integrates the pgformat PostgreSQL formatter into your development workflow.

## Prerequisites

First, ensure you have pgformat installed on your system:

```bash
# Install pgformat (from the main project directory)
just up
just install  # installs to /usr/local/bin/pgformat

# Or verify it's available in your PATH
which pgformat
pgformat --version  # should show 2.0 or later
```

**Note**: This extension requires pgformat version 2.0 or later for full configuration file support.

## Installation

### Option 1: Build and Install Extension

```bash
# Navigate to the .vscode directory
cd .vscode

# Install vsce (VS Code Extension Manager) globally
npm install -g vsce

# Package the extension
vsce package

# Install the generated extension
code --install-extension pgformat-0.0.1.vsix
```

### Option 2: Direct Installation (if published)

Search for "pgformat" in the VS Code Extensions marketplace.

## Configuration

### Workspace Trust

You may need to configure your workspace as trusted before the formatter works:

1. Open Command Palette (`Ctrl+Shift+P` / `Cmd+Shift+P`)
2. Search for "Workspace: Manage Workspace Trust"
3. Set your workspace as trusted

### VS Code Settings

Configure pgformat as your SQL formatter by opening your settings as JSON (`Ctrl+Shift+P` > "Preferences: Open Settings (JSON)"):

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

### Extension Settings

You can customize the pgformat executable path in VS Code settings:

```json
{
  "pgformat.path": "/usr/local/bin/pgformat"
}
```

If you installed pgformat to a different location, update this path accordingly.

## Usage

Once configured, pgformat will automatically format your SQL files:

- **Format on Save**: Files are automatically formatted when saved
- **Format on Paste**: Pasted SQL code is automatically formatted
- **Manual Format**: Use `Shift+Alt+F` (Windows/Linux) or `Shift+Option+F` (Mac)
- **Format Selection**: Select SQL code and use the format command

## Configuration File Support

The extension respects `.pgformat` configuration files in your project root:

```ini
# .pgformat
indent-size = 2
max-line-length = 80
newline-after-semicolon = true
space-after-comma = true
```

## Troubleshooting

### Extension Not Working

1. **Check pgformat installation**:

   ```bash
   which pgformat
   pgformat --version
   ```

2. **Verify extension settings**: Ensure the `pgformat.path` setting points to the correct executable

3. **Check workspace trust**: Make sure your workspace is trusted

4. **Restart VS Code**: After installing the extension or changing settings

### PATH Issues (Fish Shell Users)

If you're using fish shell and VS Code can't find pgformat:

1. Check your PATH in fish:

   ```fish
   echo $PATH
   ```

2. Ensure `/usr/local/bin` is in your PATH:

   ```fish
   set -U fish_user_paths /usr/local/bin $fish_user_paths
   ```

3. Restart VS Code after updating your PATH

### Permission Issues

If you encounter permission errors during installation:

1. Make sure pgformat is executable:

   ```fish
   chmod +x /usr/local/bin/pgformat
   ```

2. Verify you have read access to the binary:
   ```fish
   ls -la /usr/local/bin/pgformat
   ```

### Custom Installation Path

If pgformat is installed to a non-standard location:

1. Find your pgformat installation:

   ```bash
   which pgformat
   ```

2. Update the VS Code setting:
   ```json
   {
     "pgformat.path": "/path/to/your/pgformat"
   }
   ```

## Development

To modify or debug the extension:

1. Open the `.vscode` directory in VS Code
2. Press `F5` to launch a new Extension Development Host window
3. Test your changes in the new window

## Features

- ✅ Format SQL files on save
- ✅ Format SQL code on paste
- ✅ Manual formatting commands
- ✅ Configurable formatter path
- ✅ Respects `.pgformat` project configuration
- ✅ Works with all SQL file types

## Supported SQL Features

pgformat handles:

- SELECT, INSERT, UPDATE, DELETE statements
- CREATE TABLE, VIEW, FUNCTION statements
- Complex nested queries and subqueries
- PostgreSQL-specific syntax (functions, procedures, etc.)
- Comments and string literals
- Custom indentation and line length preferences

## Quick Start

1. **Install pgformat**: Follow the installation steps above
2. **Install the extension**: Use `vsce package` and `code --install-extension`
3. **Configure VS Code**: Add the JSON settings for SQL formatting
4. **Test it**: Open a `.sql` file, paste some messy SQL, and save - it should auto-format!

### Example Formatting

**Before:**

```sql
SELECT id,name,email FROM users WHERE active=true;
```

**After:**

```sql
SELECT
  id
  , name
  , email
FROM users
WHERE
  active = true;
```

## Keyboard Shortcuts

- **Format Document**: `Shift+Alt+F` (Windows/Linux) or `Shift+Option+F` (Mac)
- **Format Selection**: Select text and use the same shortcut
- **Command Palette**: `Ctrl+Shift+P` > "Format Document"

## Known Limitations

- Only works with `.sql` files (by design)
- Requires pgformat to be in PATH or explicitly configured
- Large files (>1MB) may take a few seconds to format

## Version Compatibility

- **Extension Version**: 0.0.1
- **Required pgformat**: 2.0 or later
- **VS Code**: 1.50.0 or later

## Related Links

- [pgformat Repository](https://github.com/minfhs/pgformat)
- [Main README](../README.md) - Complete pgformat documentation
- [VS Code Extension Development](https://code.visualstudio.com/api)

## Contributing

Found a bug or want to improve the extension?

1. Check the [main repository](https://github.com/minfhs/pgformat) for issues
2. The extension code is in the `.vscode` directory
3. Test changes by pressing `F5` in VS Code with the extension directory open

## License

This extension follows the same license as the main pgformat project.
