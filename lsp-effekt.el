;;; lsp-effekt.el --- LSP client for the Effekt language server  -*- lexical-binding: t; -*-

;; Package-Requires ((emacs "30.1") (lsp-mode "3.0"))

;; Copyright (C) 2025  Vojtěch Štěpančík

;; Author: Vojtěch Štěpančík <adalbert@latte>
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

;; Bindings to the Effekt language server, attempting to reproduce the
;; VSCode extension's behavior.

;;; Code:

(require 'lsp-mode)
(require 'ht)

(defgroup lsp-effekt nil
  "Customization group for `lsp-effekt'."
  :group 'lsp-mode)

(defcustom lsp-effekt-server-path "effekt"
  "The language server executable path.
Non-path values are looked up in $PATH. Usually this variable points to
the `effekt' executable."
  :group 'lsp-effekt
  :type 'string)

(defcustom lsp-effekt-server-args '("--server")
  "Arguments to the language server executable."
  :group 'lsp-effekt
  :type '(repeat string))

(defun lsp-effekt--server-command ()
  "The full server command, concatenating the executable path with the arguments."
  (cons lsp-effekt-server-path lsp-effekt-server-args))

;; TODO:
;; effekt.lib
;; effekt.debug
;; effekt.inlayHints.captures
;; effekt.inlayHints.returnTypes
;; effekt.showHoles

(lsp-defcustom lsp-effekt-backend "js"
  "Effekt backend."
  :group 'lsp-effekt
  :type '(choice (const :tag "JavaScript" "js")
                 (const :tag "LLVM" "llvm")
                 (const :tag "Chez Scheme (monadic)" "chez-monadic")
                 (const :tag "Chez Scheme (call/cc)" "chez-callcc")
                 (const :tag "Chez Scheme (lifted)" "chez-lift")
                 (const :tag "MLton" "ml"))
  :lsp-path "effekt.backend")

(lsp-defcustom lsp-effekt-show-explanations t
  "Display additional information when hovering over identifier."
  :group 'lsp-effekt
  :type 'boolean
  :lsp-path "effekt.showExplanations")

(lsp-defcustom lsp-effekt-show-ir "source"
  "Display intermediate representation."
  :group 'lsp-effekt
  :type '(choice
          (const :tag "Disable showing intermediate representation." "none")
          (const :tag "Show source tree after parsing." "source")
          (const "core")
          (const "machine")
          (const "target"))
  :lsp-path "effekt.showIR")

(lsp-defcustom lsp-effekt-show-tree nil
  "Show tree of IR instead of rendered version."
  :group 'lsp-effekt
  :type 'boolean
  :lsp-path "effekt.showTree")

(cl-defmethod lsp-clients-extract-signature-on-hover (_contents (_server-id (eql effekt)))
  "Extract signature from Effekt's hover information CONTENTS."
  "No way you get a type")

(defun lsp-effekt--show-ir-buffer (filename contents)
  "Render CONTENTS in a new buffer derived from FILENAME."
  (let ((buf (get-buffer-create (concat "*effekt-ir:" filename "*")))
        (inhibit-read-only t))
    (with-current-buffer buf
      (special-mode)
      (erase-buffer)
      (insert contents)
      (pop-to-buffer buf))))

(defconst lsp-effekt--notification-handlers
  '(("$/effekt/publishIR" .
     (lambda (_w params)
       (lsp-effekt--show-ir-buffer (ht-get params "filename")
                                   (ht-get params "content"))))))

(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-language-id-configuration '(effekt-mode . "effekt"))
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection #'lsp-effekt--server-command)
    :server-id 'effekt
    :language-id "effekt"
    :major-modes '(effekt-mode)
    :notification-handlers (ht<-alist lsp-effekt--notification-handlers)
    :synchronize-sections '("effekt")
    ;; Send `{}' instead of `nil', otherwise the server crashes
    :initialization-options
    (lambda ()
      (let ((json-object-type 'hash-table))
        (ht-get (lsp-configuration-section "effekt")
                "effekt"
                (ht-create)))))))

;; (defun lsp-effekt-run-file ()
;;     )

(provide 'lsp-effekt)
;;; lsp-effekt.el ends here
