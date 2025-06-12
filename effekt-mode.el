;;; effekt-mode.el --- Major mode for the Effekt programming language  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Vojtěch Štěpančík

;; Author: Vojtěch Štěpančík
;; Keywords: languages

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(defgroup effekt-mode nil
  "Customization group for `effekt-mode'."
  :group 'languages)

(defcustom effekt-path "effekt"
  "The compiler executable path."
  :group 'effekt-mode
  :type 'string)

(defconst effekt--syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?/ ". 12" st)
    (modify-syntax-entry ?\n ">" st)
    (modify-syntax-entry ?\( "()" st)
    (modify-syntax-entry ?\) ")(" st)
    (modify-syntax-entry ?\{ "(}" st)
    (modify-syntax-entry ?\} "){" st)
    (modify-syntax-entry ?\[ "(]" st)
    (modify-syntax-entry ?\] ")[" st)
    (modify-syntax-entry ?_ "_" st)
    st))

;;;###autoload
(defun effekt-run-file (&optional buffer)
  "Run the program in BUFFER, or current buffer if nil."
  (interactive)
  (let* ((buffer (or buffer (current-buffer)))
         (file (buffer-file-name buffer)))
    (if (not file)
        (message "Buffer %s does not correspond to a file!" buffer)
      (when (buffer-modified-p buffer)
        (save-buffer buffer))
      (pop-to-buffer
       (make-comint (concat "effekt: " (buffer-name buffer))
                    effekt-path nil
                    file)))))

(defvar effekt-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'effekt-run-file)
    map))

;;;###autoload
(define-derived-mode effekt-mode prog-mode "effekt"
  "Major mode for editing Effekt source code."
  (set-syntax-table effekt--syntax-table)
  (setq comment-start "//"
        comment-start-skip "//+ *"
        tab-width 2))

;;;###autoload
(define-minor-mode literate-effekt-mode
  "Minor mode for editing Markdown files with embedded Effekt snippets.")

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.effekt\\'" . effekt-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.effekt\\.md\\'" . literate-effekt-mode))

(provide 'effekt-mode)
;;; effekt-mode.el ends here
