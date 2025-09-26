# org-pagemaker

A lightweight Emacs minor mode to help author Org-mode documents for the A101 pagemaker (Typst) pipeline.

It provides:
- Build commands for Typst (`.typ`) and PDF via the `pagemaker` CLI
- Validation and watch workflows with clickable error locations
- An IR viewer to inspect the intermediate JSON
- Font helpers (list, install, validate, search, analyze, specimen)
- Interactive template inserters for documents, pages, and elements

Repository: https://github.com/sanderboer/org-pagemaker


## Requirements
- Emacs >= 27.1
- Org >= 9.3
- Pagemaker CLI available:
  - Either the `pagemaker` executable on your `PATH`, or the Python module `pagemaker.cli` available in your project.
- Typst installed and on `PATH` for PDF builds
- Python 3 (only needed when invoking `python -m pagemaker.cli`)

Note: When the `pagemaker` binary is not found (or you disable it), the mode runs `python -m pagemaker.cli` and automatically augments `PYTHONPATH` with the project’s `src/` folder (project root is detected by `pyproject.toml` or `.git`).


## Installation

### straight.el (recommended)
Basic recipe:

```elisp
(straight-use-package
 '(org-pagemaker :type git
                 :host github
                 :repo "sanderboer/org-pagemaker"))
```

Explicit branch and file layout (robust for the `lisp/` layout):

```elisp
(straight-use-package
 '(org-pagemaker :type git
                 :host github
                 :repo "sanderboer/org-pagemaker"
                 :branch "main"
                 :files ("lisp/*.el")))
```

With use-package:

```elisp
(use-package org-pagemaker
  :straight (org-pagemaker :type git
                           :host github
                           :repo "sanderboer/org-pagemaker"
                           :branch "main"
                           :files ("lisp/*.el"))
  :hook (org-mode . org-pagemaker-mode))
```

### Manual
- Clone the repo and add the `lisp/` directory to your `load-path`:

```elisp
(add-to-list 'load-path "/path/to/org-pagemaker/lisp/")
(require 'org-pagemaker)
```


## Quick Start
1. Open an `.org` file in your project.
2. Enable the mode: `M-x org-pagemaker-mode` (or add to your `org-mode-hook`).
3. Build PDF with `C-c p p`. The output goes into `export/` by default.
4. Use `C-c p o` to open the last PDF for the current Org file.

The mode displays build/validate/watch buffers in `compilation-mode` by default; Typst-style error messages become clickable.


## Keybindings
- Builds
  - `C-c p b`: Build Typst (`org-pagemaker-build`)
  - `C-c p p`: Build PDF (`org-pagemaker-pdf`)
  - `C-c p v`: Validate assets (`org-pagemaker-validate`)
  - `C-c p w`: Watch (Typst or PDF with `C-u`) (`org-pagemaker-watch`)
  - `C-c p s`: Stop watch (`org-pagemaker-stop-watch`)
  - `C-c p o`: Open last PDF (`org-pagemaker-open-last-pdf`)
  - `C-c p i`: IR viewer (`org-pagemaker-ir`)

- Templates
  - `C-c p t d`: Insert document header (`org-pagemaker-insert-document-template`)
  - `C-c p t p`: Insert page (`org-pagemaker-insert-page`)
  - `C-c p t e`: Insert element (`org-pagemaker-insert-element`)

- Fonts
  - `C-c p f l`: List installed fonts (`org-pagemaker-fonts-list`) — `C-u` for details
  - `C-c p f i`: Install font (`org-pagemaker-fonts-install`) — prompt for family, variants
  - `C-c p f v`: Validate font availability (`org-pagemaker-fonts-validate`) — `C-u` for details
  - `C-c p f s`: Search Google Fonts (`org-pagemaker-fonts-search`) — `C-u` for details
  - `C-c p f a`: Analyze current document’s fonts (`org-pagemaker-fonts-analyze`) — `C-u` for details
  - `C-c p f p`: Generate specimen (`org-pagemaker-fonts-specimen`) — `C-u` for PDF, `C-u C-u` to choose type


## Commands and Prefix Arguments
- Most build commands accept a prefix (`C-u`) to prompt for additional options (e.g., export dir, filenames).
- `org-pagemaker-watch` with `C-u` also builds PDFs on changes.
- `org-pagemaker-fonts-specimen` with `C-u C-u` prompts for specimen type (`showcase`, `comparison`, `simple`).


## Customization
Customize via `M-x customize-group RET org-pagemaker RET`:
- `org-pagemaker-export-dir` (default: `export`): Output location for builds
- `org-pagemaker-typst-output` (default: `deck.typ`): Typst filename
- `org-pagemaker-pdf-output` (default: nil): Optional explicit PDF output name
- `org-pagemaker-prefer-cli-binary` (default: t): Prefer `pagemaker` executable
- `org-pagemaker-python-executable` (default: `python3`): Used when falling back to module
- `org-pagemaker-sanitize-pdfs` (default: nil): Pass `--sanitize-pdfs` to PDF/watch
- `org-pagemaker-validate-fonts` (default: nil): Pass `--validate-fonts` to build/pdf
- `org-pagemaker-strict-fonts` (default: nil): Pass `--strict-fonts` to build/pdf
- `org-pagemaker-use-compilation-mode` (default: t): Enable clickable errors


## Error Navigation
When `org-pagemaker-use-compilation-mode` is enabled, build/validate/watch buffers use `compilation-mode` and add Typst-style error patterns:
- Clicking on `--> file.typ:line:col` jumps to the location.
- Standard `next-error` / `previous-error` works (`M-g n` / `M-g p`).


## Project Root Detection
The mode derives the project root by looking for `pyproject.toml` or `.git` (fallback to the current file’s directory). This is used to:
- Set the working directory for CLI commands
- Infer `PYTHONPATH` to include `src/` when using `python -m pagemaker.cli`


## Backward Compatibility
The legacy `pagemaker-el` package name is still available as a shim that forwards all commands and variables to `org-pagemaker`. Move to `org-pagemaker-*` names when convenient.


## Troubleshooting
- Ensure Typst and `pagemaker` are available on your `PATH` if you want to use the binary. Otherwise, the mode falls back to `python -m pagemaker.cli`.
- If `watch` says a process is already running, use `C-c p s` to stop it.
- Fonts commands rely on the CLI; network is required for searching and installing from Google Fonts.


## Contributing
Issues and PRs welcome at https://github.com/sanderboer/org-pagemaker.
