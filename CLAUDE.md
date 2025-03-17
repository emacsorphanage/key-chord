# Key-Chord Coding Style Guidelines

## General formatting
- Lines should stay below 80 characters like the old code and readme
- Make sure not to add trailing whitespace
- Add space between top-level forms (defun, defvar, defcustom, etc.)

## Documentation
- First line of docstrings should be a sentence ending with a period
- Format docstrings to wrap at 72 characters for readability

## Commit Messages
- Use the format `type: Message` where type is lowercase (feat, fix, docs, chore, etc.)
- Capitalize the first word after the colon in the message
- Keep messages concise but descriptive

## Build commands
- `make package` - Create a package
- `make lint` - Run linting
- `emacs -Q -batch -L . -l key-chord.el -f key-chord-mode` - Simple test of loading

## Versioning 
- Use semantic versioning: MAJOR.MINOR.PATCH
- Current version: 0.8.2