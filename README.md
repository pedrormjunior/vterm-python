# vterm-python

Collection of functions to interact with a Python interpreter in vterm in Emacs.

It is a tiny Emacs minor mode to send Python code (regions, lines, buffers, narrowed regions) to a `vterm`-based Python REPL, preserving your intended indentation and workflow.

---

## Features

- **Open or switch** to a dedicated `*vterm-python*` buffer, reusing the last one or creating numbered new ones via prefix (`C-u`).
- **Send**:
  - **Region** (`C-c C-r`)
  - **Current line** (`C-c C-l`)
  - **Entire buffer** (`C-c C-b`)
  - **Narrowed region** (`C-c C-c`)
- **Correct block handling**: replaces newlines with `Ctrl+J` to treat multi-line input as a single block.
- **Customizable**: honors `python-shell-interpreter` and `python-shell-interpreter-args`.
- **Lightweight**: zero external dependencies beyond `vterm`.

---

## Installation

### 1. Clone or download

```bash
git clone https://github.com/yourusername/vterm-python.git ~/.emacs.d/site-lisp/vterm-python
```

### 2. Add to your `load-path`

```elisp
(add-to-list 'load-path "~/.emacs.d/site-lisp/vterm-python")
(require 'vterm-python)
```

### 3. Enable in `python-mode`

Using vanilla Emacs:

```elisp
(add-hook 'python-mode-hook #'vterm-python-mode)
```

Or with `use-package`:

```elisp
(use-package vterm-python
  :load-path "~/.emacs.d/site-lisp/vterm-python"
  :hook (python-mode . vterm-python-mode))
```

---

## Keybindings

Once `vterm-python-mode` is active (lighter: `VtPy`), these bindings are available:

| Key       | Command                        | Description                                   |
|-----------|--------------------------------|-----------------------------------------------|
| `C-c C-o` | `vterm-python-open`            | Open/switch to `*vterm-python*` (prefix → new)|
| `C-c C-b` | `vterm-python-send-buffer`     | Send the **entire buffer** as one code block  |
| `C-c C-c` | `vterm-python-send-narrowed`   | Send the **narrowed region**                  |
| `C-c C-l` | `vterm-python-send-line`       | Send the **current line** and move point down |
| `C-c C-r` | `vterm-python-send-region`     | Send the **selected region**                  |

---

## Usage Example

1. Open a `*vterm-python*` terminal from a `vterm-python-mode`-activated Python file:
   `C-c C-o`
   This opens a `*vterm-python*` buffer and starts the interpreter according to `python-shell-interpreter` and `python-shell-interpreter-args`.

2. Write this code in your buffer:

```python
def greet(name):
    print(f"Hello, {name}")

greet("Carla")
```

3. Send the function definition (region):

   - Select lines 1–3
   - `C-c C-r`

4. Send the call:

   - Place point on line 5
   - `C-c C-l`

5. Observe the output in the vterm buffer.

Also, the entire buffer can be sent with `C-c C-b` and the entire narrowed/visible region can be sent with `C-c C-c`.

---

## Minor Mode Code

Here is the minor mode that wires everything up:

```elisp
(define-minor-mode vterm-python-mode
  "Minor mode for sending code to `*vterm-python*'."
  :lighter " VtPy"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-o") #'vterm-python-open)
            (define-key map (kbd "C-c C-b") #'vterm-python-send-buffer)
            (define-key map (kbd "C-c C-c") #'vterm-python-send-narrowed)
            (define-key map (kbd "C-c C-l") #'vterm-python-send-line)
            (define-key map (kbd "C-c C-r") #'vterm-python-send-region)
            map))
```

---

## Configuration

By default, the mode reads:

- `python-shell-interpreter` (e.g. `"python3"`)
- `python-shell-interpreter-args` (e.g. `"-i"`)

You can override these:

```elisp
(setq python-shell-interpreter "python3.13"
      python-shell-interpreter-args "-i")
```

---

## How It Works

1. **Buffer naming**
   `vterm-python-generate-buffer-name` (called by `vterm-python-open` [`C-c C-o`]) finds or generates `*vterm-python*`, `*vterm-python*<2>`, etc. 

2. **Opening**
   `vterm-python-open` creates or reuses a vterm buffer and runs the interpreter.
   That is, when called with a prefix argument, it forcefully generates a new `vterm` buffer, otherwise it simply opens a pre-existing (last visited) buffer.

3. **String processing**
   `vterm-python-process-string`: Replaces sent `\n` with `C-q C-j` so the REPL treats multi-line input as one block.

4. **Sending**
   `vterm-python-send-string`: handles empty-line logic, optional extra return, and keeps state via `vterm-python-last-was-empty`.

5. **Convenience commands**
   `…-send-region`, `…-send-line`, `…-send-buffer`, `…-send-narrowed` wrap all of the above.

---

## Contributing

1. Fork the repo.
2. Clone it.
3. Create a branch: `git checkout -b feature/foo`.
4. Commit your changes.
5. Push: `git push origin feature/foo`.
6. Open a Pull Request.

---

## License

This project is licensed under the **GNU General Public License v3.0**. See the LICENSE file for details.
