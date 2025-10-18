# pgformat - PostgreSQL Formatter

An opinionated PostgreSQL code formatter built in OCaml with configurable formatting options.

## Features

✨ **Smart SQL Formatting**

- Consistent indentation and line breaks
- Proper comma placement and spacing
- Function and procedure formatting
- Complex query structure normalization

🔧 **Configurable Options**

- Customizable indentation size
- Adjustable line length limits
- Boolean formatting preferences
- Project-level configuration files

🚀 **Multiple Interfaces**

- Command-line tool with rich options
- VS Code extension support
- Editor integrations (Helix, etc.)
- Programmatic FormatterBuilder API

## Installation

### Quick Install

```bash
just up
just install # installs to /usr/local/bin
```

### From Source

```bash
dune build
dune install
```

## Usage

### Basic Command Line

```bash
# Format a file
pgformat myfile.sql

# Format from stdin
echo "SELECT a,b FROM users;" | pgformat

# Format with custom indentation
pgformat myfile.sql -indent-size 2

# Format with custom line length
pgformat myfile.sql -max-line-length 80
```

### Configuration File

Create a `.pgformat` file in your project root for consistent formatting:

```ini
# .pgformat configuration file
indent-size = 2
max-line-length = 80
newline-after-semicolon = true
space-after-comma = true
```

Command-line options override config file settings.

### Help

```bash
pgformat --help
```

## Editor Integration

### VS Code

Install the pgformat extension following this [guide](./.vscode/README.md).

### Helix

Add to your `languages.toml`:

```toml
[[language]]
name = "sql"
auto-format = true
formatter = { command = "pgformat" }
```

Check with `hx --health sql`.

### Other Editors

pgformat works with any editor that supports external formatters. Configure your editor to run `pgformat` on SQL files.

## Configuration Options

| Option                    | Type    | Default | Description                            |
| ------------------------- | ------- | ------- | -------------------------------------- |
| `indent-size`             | integer | 4       | Number of spaces per indentation level |
| `max-line-length`         | integer | 120     | Maximum line length before wrapping    |
| `newline-after-semicolon` | boolean | true    | Add newline after semicolons           |
| `space-after-comma`       | boolean | true    | Add space after commas                 |

### Configuration Precedence

1. Command-line arguments (highest priority)
2. `.pgformat` file in current directory
3. Default values (fallback)

## Sample Output

### Before

```sql
SELECT     , reviews.room_id , reviews.user_id , reviews.state AS "state: ReviewState"
    , applicants.state AS "applicant_state: ApplicantState"
FROM applicant_reviews AS reviews LEFT JOIN (
    SELECT id , fhs_id , state FROM applicants ) AS applicants ON applicants.id
    = reviews.applicant_id
WHERE
    reviews.room_id = ( SELECT id FROM rooms
        WHERE abbr = $1 AND y = x )
    AND applicants.state IS NOT NULL AND applicants.fhs_id IS NOT NULL;
```

### After

```sql
SELECT
    reviews.id AS id
    , applicants.fhs_id AS fhs_id
    , applicants.id AS applicant_id
    , reviews.room_id
    , reviews.user_id
    , reviews.state AS "state: ReviewState"
    , applicants.state AS "applicant_state: ApplicantState"
FROM applicant_reviews AS reviews
LEFT JOIN (
    SELECT
        id
        , fhs_id
        , state
    FROM applicants
) AS applicants ON applicants.id = reviews.applicant_id
WHERE
    reviews.room_id = (
        SELECT
            id
        FROM rooms
        WHERE
            abbr = $1
            AND y = x
    )
    AND applicants.state IS NOT NULL
    AND applicants.fhs_id IS NOT NULL;
```

## Development

### Running Tests

```bash
# Run all tests
dune runtest

# Run tests in watch mode
dune runtest -w

# Run specific test
dune runtest test/
```

### Testing the Formatter

```bash
# Test on fixture files
dune exec bin/main.exe fixture/complex.sql

# Test with different configurations
echo "SELECT a,b FROM users;" | dune exec bin/main.exe -- -indent-size 2
```

### Building

```bash
# Build library and executables
dune build

# Build in watch mode
dune build -w
```

## Architecture

pgformat uses a modular architecture with:

- **Lexer**: Tokenizes SQL input
- **Parser**: Processes tokens with formatting rules
- **Formatters**: Specialized modules for different token types
  - Comment Formatter
  - Keyword Formatter
  - Literal Formatter
  - Punctuation Formatter
- **Builder Pattern**: Configurable formatter construction
- **Output Interface**: Pluggable output destinations

### FormatterBuilder API

For programmatic use:

```ocaml
open Pgformat

let config = { Config.default_config with indent_size = 2 } in
let builder = Builder.FormatterBuilder.create ()
  |> Builder.FormatterBuilder.with_indent_size config.indent_size
  |> Builder.FormatterBuilder.with_max_line_length config.max_line_length in
Builder.FormatterBuilder.format_with_config builder lexbuf
```

## Examples

Check the [fixture](./fixture) directory for formatted examples demonstrating various SQL constructs:

- `complex.sql` - Complex queries with functions and nested structures
- `create.sql` - Table creation statements
- `select.sql` - Various SELECT query patterns
- `select_simple.sql` - Basic SELECT statements

## Contributing

1. Fork the repository
2. Create a feature branch
3. Make your changes with tests
4. Ensure all tests pass: `dune runtest`
5. Submit a pull request

## License

This project is licensed under the terms specified in the repository.
