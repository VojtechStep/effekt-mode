;;; lsp-effekt-holes.el --- Handling of Effekt holes  -*- lexical-binding: t; -*-

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

;; Visualization of typed holes.

;;; Code:

(require 'magit-section)
(require 'lsp-mode)

(defclass lsp-effekt-holes-section (magit-section)
  ((type :initform 'lsp-effekt-holes)))

(defclass lsp-effekt-hole-section (magit-section)
  ((type :initform 'lsp-effekt-hole)
   (keymap :initform 'lsp-effekt--hole-map)))

(defclass lsp-effekt-hole-terms (magit-section)
  ((type :initform 'lsp-effekt-hole-terms)))

(defclass lsp-effekt-hole-term (magit-section)
  ((type :initform 'lsp-effekt-hole-term)))

(defclass lsp-effekt-hole-types (magit-section)
  ((type :initform 'lsp-effekt-hole-types)))

(defclass lsp-effekt-hole-type (magit-section)
  ((type :initform 'lsp-effekt-hole-type)))

(defun lsp-effekt--goto-hole (path start &optional noselect)
  "Go to a buffer visiting PATH at position START.

If NOSELECT is non-nil, don't switch to the target buffer."
  (let* ((old-frame (selected-frame))
         (target-buffer (find-file-noselect path))
         (display-actions
          '((display-buffer-reuse-window display-buffer-pop-up-frame)
            nil))
         (window (display-buffer target-buffer display-actions)))
    (when window
      (with-current-buffer target-buffer
        (set-window-point window (lsp--position-to-point start)))
      (when (not noselect)
        (let ((frame (window-frame window)))
          (unless (eq frame old-frame)
            (select-frame-set-input-focus frame))
          (select-window window))))))

(defun lsp-effekt--goto-current-hole (&optional pos noselect)
  "Wrapper around `lsp-effekt--goto-hole'.

Extracts the location data from section at POS or point. NOSELECT is
passed through."
  (let* ((section (magit-section-at pos))
         (value (oref section value))
         (path (car value))
         (start (cdr value)))
    (lsp-effekt--goto-hole path start noselect)))

(defun lsp-effekt--goto-hole-section (&optional pos)
  "Go to the hole associated to the section at POS.

To be bound in a keymap."
  (interactive)
  (lsp-effekt--goto-current-hole pos nil))

(defun lsp-effekt--goto-hole-section-noselect (&optional pos)
  "Go to the hole associated to the section at POS.
Don't select the target buffer.

To be bound in a keymap."
  (interactive)
  (lsp-effekt--goto-current-hole pos 'noselect))

(defvar lsp-effekt--hole-map
  (let ((map (make-sparse-keymap)))
    ;; (set-keymap-parent map magit-section-mode-map)
    (define-key map (kbd "<return>") #'lsp-effekt--goto-hole-section)
    (define-key map (kbd "S-<return>") #'lsp-effekt--goto-hole-section-noselect)
    map))

;; More interesting stuff about local types could be shown after
;; https://github.com/effekt-lang/effekt/pull/1029 is merged

;;;###autoload
(defun lsp-effekt--register-holes (_w params)
  "Entrypoint handling PARAMS."
  (let* ((uri (lsp-get params :uri))
         (holes (lsp-get params :holes))
         (path (lsp--uri-to-path uri))
         (buffer (get-buffer-create "*effekt holes*"))
         prev-point)
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (setq prev-point (point))
        (erase-buffer)
        (effekt-hole-mode)
        (magit-insert-section (lsp-effekt-holes-section)
          (magit-insert-heading
            (format "Holes in %s" path))
          (insert "\n")
          (seq-doseq (hole holes)
            (let* ((id (lsp-get hole :id))
                   (range (lsp-get hole :range))
                   (start (lsp-get range :start))
                   (innerT (or (lsp-get hole :innerType) "N/A"))
                   (outerT (or (lsp-get hole :expectedType) "N/A"))
                   ;; (imported-types (lsp-get hole :importedTypes))
                   ;; (imported-terms (lsp-get hole :importedTerms))
                   (terms (lsp-get hole :terms))
                   (types (lsp-get hole :types)))
              (magit-insert-section (lsp-effekt-hole-section (cons path start))
                (magit-insert-heading
                  (format "%s (%s:%s:%s)"
                          id
                          (file-name-nondirectory path)
                          (lsp-get start :line)
                          (lsp-get start :character)))
                (insert (format "Inner type: %s\n" innerT))
                (insert (format "Expected type: %s\n" outerT))
                (magit-insert-section (lsp-effekt-hole-terms nil t)
                  (magit-insert-heading t "Local terms")
                  (seq-doseq (term terms)
                    (magit-insert-section (lsp-effekt-hole-term)
                      (insert
                       (format "%s : %s\n"
                               (lsp-get term :name)
                               (lsp-get term :type))))))
                (magit-insert-section (lsp-effekt-hole-types nil t)
                  (magit-insert-heading t "Local types")
                  (seq-doseq (type types)
                    (magit-insert-section (lsp-effekt-hole-type)
                      (insert (lsp-get type :name) "\n")))))
              (insert "\n"))))
        (goto-char prev-point)))
    (let ((window (display-buffer
                   buffer
                   '((display-buffer-reuse-window display-buffer-pop-up-frame)
                     ((inhibit-switch-frame . t))))))
      (when window
        (set-window-point window prev-point)))))

(define-derived-mode effekt-hole-mode magit-section-mode "effect-hole"
  "Major mode for viewing information about typed Effekt holes.")

(provide 'lsp-effekt-holes)
;;; lsp-effekt-holes.el ends here
