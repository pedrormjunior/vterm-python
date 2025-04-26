;;; vterm-python.el --- Send code to Python vterm easily -*- lexical-binding: t; -*-

;; Author: Pedro Ribeiro Mendes Júnior <pedrormjunior@gmail.com>
;; Maintainer: Pedro Ribeiro Mendes Júnior <pedrormjunior@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "26.1") (vterm "0.0"))
;; Homepage: https://github.com/pedrormjunior/vterm-python
;; Keywords: terminals, python, repl, convenience
;; License: GPL-3+

;;; Commentary:

;; This minor mode allows sending Python code (regions, lines,
;; buffers, narrowed regions) to a vterm buffer running a Python
;; interpreter, simulating an IPython-style REPL.

;;; Code:

(defvar vterm-python-last-was-empty
  "Mark if last string (line) sent was empty.
In case it was and the current sent string is empty, send a return to
the terminal anyways." nil)


(defun vterm-python-generate-buffer-name (&optional new)
  "Get a buffer name for `*vterm-python*'.
If NEW is non-nil, generate a new one, otherwise, try to reuse an
already used one."
  (let ((base-name "*vterm-python*")
        (existing-buffers
         (seq-filter (lambda (buf)
                       (string-match-p
                        "^\\*vterm-python\\*\\(<[0-9]+>\\)?$"
                        (buffer-name buf)))
                     (buffer-list))))
    (if new
        (generate-new-buffer-name base-name)
      (if existing-buffers
          (buffer-name (car existing-buffers))
        base-name))))

(defun vterm-python-process-string (str)
  "Common processing of strings STR for `*vterm-python*'."
  (replace-regexp-in-string
   "\n" "\C-q\C-j"
   (replace-regexp-in-string "\n\\'" "" str)))

(defun vterm-python-send-string (buf-name str &optional extra-return)
  "Send string STR to `*vterm-python*' buffer.
BUF-NAME is the name of the buffer in which to send the string STR."
  (if (get-buffer buf-name)
      (progn
        (with-current-buffer buf-name
          (vterm-send-string str)
          (if (or (not (string= str ""))
                  vterm-python-last-was-empty)
              (progn
                (vterm-send-return)
                (when extra-return
                  (vterm-send-return))
                (vterm-send-key "\\" nil t)
                (setq vterm-python-last-was-empty nil))
            (vterm-send-key "q" nil nil t)
            (vterm-send-key "j" nil nil t)
            (setq vterm-python-last-was-empty t)))
        (deactivate-mark))
    (error "No vterm-python buffer found")))


;;;###autoload
(defun vterm-python-open (prefix)
  "Open or switch to the most recent Python vterm.
With PREFIX argument, always open a new vterm buffer.  Without it, reuse
the most recently used `*vterm-python*' buffer.  Start the interpreter
only if the buffer is newly created."
  (interactive "P")
  (let* ((interpreter (or python-shell-interpreter "python3"))
         (args (or python-shell-interpreter-args ""))
         (cmd (string-trim (concat interpreter " " args "; exit")))
         (buf-name (vterm-python-generate-buffer-name prefix))
         (new-buffer? (not (get-buffer buf-name)))
         (window-config (current-window-configuration)))
    (if new-buffer?
        (progn
          (vterm buf-name)
          (vterm-clear)
          (vterm-send-string cmd)
          (vterm-send-return)
          (set-window-configuration window-config)))
    (switch-to-buffer-other-window buf-name)))

;;;###autoload
(defun vterm-python-send-region nil
  "Send the selected region to `*vterm-python*' buffer."
  (interactive)
  (if (use-region-p)
      (let* ((region (buffer-substring-no-properties
                      (region-beginning) (region-end)))
             (processed-str (vterm-python-process-string region))
             (buf-name (vterm-python-generate-buffer-name)))
        (vterm-python-send-string buf-name processed-str))
    (error "No region selected")))

;;;###autoload
(defun vterm-python-send-line nil
  "Send the current line to `*vterm-python*' buffer."
  (interactive)
  (let* ((line (thing-at-point 'line t))
         (processed-str (vterm-python-process-string line))
         (buf-name (vterm-python-generate-buffer-name)))
    (vterm-python-send-string buf-name processed-str)
    (forward-line)))

;;;###autoload
(defun vterm-python-send-region-or-line nil
  "Send the selected region or line to `*vterm-python*' buffer.
When there is no selected region, the current line is sent instead."
  (interactive)
  (if (use-region-p)
      (vterm-python-send-region)
    (vterm-python-send-line)))

;;;###autoload
(defun vterm-python-send-buffer nil
  "Send the entire buffer to `*vterm-python*' buffer."
  (interactive)
  (let* ((buffer-content (buffer-string))
         (processed-str (vterm-python-process-string buffer-content))
         (buf-name (vterm-python-generate-buffer-name)))
    (vterm-python-send-string buf-name processed-str t)))

;;;###autoload
(defun vterm-python-send-narrowed nil
  "Send the narrowed region to `*vterm-python*' buffer."
  (interactive)
  (let* ((narrowed-region (buffer-substring-no-properties
                           (point-min) (point-max)))
         (processed-str (vterm-python-process-string narrowed-region))
         (buf-name (vterm-python-generate-buffer-name)))
    (vterm-python-send-string buf-name processed-str t)))


;;;###autoload
(define-minor-mode vterm-python-mode
  "Minor mode for sending code to `*vterm-python*' buffer."
  :lighter " VtPy"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-b") #'vterm-python-send-buffer)
            (define-key map (kbd "C-c C-c") #'vterm-python-send-region-or-line)
            (define-key map (kbd "C-c C-l") #'vterm-python-send-line)
            (define-key map (kbd "C-c C-n") #'vterm-python-send-narrowed)
            (define-key map (kbd "C-c C-o") #'vterm-python-open)
            (define-key map (kbd "C-c C-r") #'vterm-python-send-region)
            map))

(provide 'vterm-python)

;;; vterm-python.el ends here
