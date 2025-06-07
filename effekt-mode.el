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

(defconst effekt--syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?/ "<" st)
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
