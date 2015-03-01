;;; imenu-list.el --- Show imenu entries in a seperate buffer

;; Copyright (C) 2015 Bar Magal

;; Author: Bar Magal (2015)
;; Version: 0.1
;; Homepage: https://github.com/bmag/imenu-list
;; Package-Requires: ((cl-lib "0.5"))

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Shows a list of imenu entries for the current buffer, in another
;; buffer with the name "*Ilist*".
;;
;; Activation and deactivation:
;; M-x imenu-list-minor-mode
;;
;; Key shortcuts from "*Ilist*" buffer:
;; <enter>: Go to current definition
;; <space>: display current definition

;;; Code:

(require 'imenu)
(require 'cl-lib)

(defvar imenu-list-buffer-name "*Ilist*"
  "Name of the buffer that is used to display imenu entries.")

(defvar imenu-list--imenu-entries nil
  "A copy of the imenu entries of the buffer we want to display in the
imenu-list buffer.")

(defvar imenu-list--line-entries nil
  "List of imenu entries displayed in the imenu-list buffer.
The first item in this list corresponds to the first line in the
imenu-list buffer, the second item matches the second line, and so on.")

(defvar imenu-list--displayed-buffer nil
  "The buffer who owns the saved imenu entries.")


;;; collect entries

(defun imenu-list-rescan-imenu ()
  "Force imenu to rescan the current buffer."
  (setq imenu--index-alist nil)
  (imenu--make-index-alist))

(defun imenu-list-collect-entries ()
  "Collect all `imenu' entries of the current buffer."
  (imenu-list-rescan-imenu)
  (setq imenu-list--imenu-entries imenu--index-alist)
  (setq imenu-list--displayed-buffer (current-buffer)))


;;; print entries

(defun imenu-list--depth-string (depth)
  "Return a prefix string representing an entry's DEPTH."
  (let ((indents (cl-loop for i from 1 to depth collect "-")))
    (format "%s%s"
            (mapconcat #'identity indents "")
            (if indents " " ""))))

(defun imenu-list--insert-entry (entry depth)
  "Insert a line to represent an entry in `imenu-list--imenu-entries'.
Don't forget to insert a newline at the end.
ENTRY is the entry to represent, and DEPTH is its nesting level (0 means
toplevel)."
  (if (imenu--subalist-p entry)
      (insert (format "%s{ %s }\n" (imenu-list--depth-string depth) (car entry)))
    (insert (format "%s%s\n" (imenu-list--depth-string depth) (car entry)))))

(defun imenu-list--insert-entries-internal (index-alist depth)
  "Insert all imenu entries in INDEX-ALIST into the current buffer.
DEPTH is the depth of the code block were the entries are written.
Each entry is inserted in its own line.
Each entry is appended to `imenu-list--line-entries' as well."
  (dolist (entry index-alist)
    (setq imenu-list--line-entries (append imenu-list--line-entries (list entry)))
    (imenu-list--insert-entry entry depth)
    (when (imenu--subalist-p entry)
      (imenu-list--insert-entries-internal (cdr entry) (1+ depth)))))

(defun imenu-list-insert-entries ()
  "Insert all imenu entries into the current buffer.
The entries are taken from `imenu-list--imenu-entries'.
Each entry is inserted in its own line.
Each entry is appended to `imenu-list--line-entries' as well
 (`imenu-list--line-entries' is cleared in the beginning of this
function)."
  (read-only-mode -1)
  (erase-buffer)
  (setq imenu-list--line-entries nil)
  (imenu-list--insert-entries-internal imenu-list--imenu-entries 0)
  (read-only-mode 1))


;;; goto entries

(defun imenu-list--find-entry ()
  "Find in `imenu-list--line-entries' the entry in the current line."
  (nth (1- (line-number-at-pos)) imenu-list--line-entries))

(defun imenu-list-goto-entry ()
  "Switch to the original buffer and display the entry under point."
  (interactive)
  (let ((entry (imenu-list--find-entry)))
    (pop-to-buffer imenu-list--displayed-buffer)
    (imenu entry)
    (imenu-list--show-current-entry)))

(defun imenu-list-display-entry ()
  "Display in original buffer the entry under point."
  (interactive)
  (let ((entry (imenu-list--find-entry)))
    (save-selected-window
      (pop-to-buffer imenu-list--displayed-buffer)
      (imenu entry)
      (imenu-list--show-current-entry))))

(defalias 'imenu-list-<=
  (if (ignore-errors (<= 1 2 3))
      #'<=
    #'(lambda (x y z)
        "Return t if X <= Y and Y <= Z."
        (and (<= x y) (<= y z)))))

(defun imenu-list--current-entry ()
  "Find entry in `imenu-list--line-entries' matching current position."
  (let ((pos (point-marker))
        (offset (point-min-marker))
        match-entry)
    (dolist (entry imenu-list--line-entries match-entry)
      (when (and (not (imenu--subalist-p entry))
                 (imenu-list-<= offset (cdr entry) pos))
        (setq offset (cdr entry))
        (setq match-entry entry)))))

(defun imenu-list--show-current-entry ()
  "Move the imenu-list buffer's point to the current position's entry."
  (when (get-buffer-window (imenu-list-get-buffer-create))
    (let ((line-number (cl-position (imenu-list--current-entry)
                                    imenu-list--line-entries
                                    :test 'equal)))
      (with-selected-window (get-buffer-window (imenu-list-get-buffer-create))
        (goto-char (point-min))
        (forward-line line-number)
        (hl-line-mode 1)))))

;;; define major mode

(defun imenu-list-get-buffer-create ()
  "Return the imenu-list buffer.
If it doesn't exist, create it."
  (or (get-buffer imenu-list-buffer-name)
      (let ((buffer (get-buffer-create imenu-list-buffer-name)))
        (with-current-buffer buffer
          (imenu-list-major-mode)
          buffer))))

(defun imenu-list-update ()
  "Update the imenu-list buffer.
If the imenu-list buffer doesn't exist, create it."
  (let ((old-entries imenu-list--imenu-entries))
    (imenu-list-collect-entries)
    (unless (equal old-entries imenu-list--imenu-entries)
      (with-current-buffer (imenu-list-get-buffer-create)
        (imenu-list-insert-entries)))
    (imenu-list--show-current-entry)))

(defun imenu-list-show ()
  "Show the imenu-list buffer.
If the imenu-list buffer doesn't exist, create it."
  (interactive)
  (pop-to-buffer imenu-list-buffer-name))

(defun imenu-list-show-noselect ()
  "Show the imenu-list buffer, but don't select it.
If the imenu-list buffer doesn't exist, create it."
  (interactive)
  (display-buffer imenu-list-buffer-name))

;;;###autoload
(defun imenu-list-noselect ()
  "Update and show the imenu-list buffer, but don't select it.
If the imenu-list buffer doesn't exist, create it."
  (interactive)
  (imenu-list-update)
  (imenu-list-show-noselect))

;;;###autoload
(defun imenu-list ()
  "Update and show the imenu-list buffer.
If the imenu-list buffer doesn't exist, create it."
  (interactive)
  (imenu-list-update)
  (imenu-list-show))

(defvar imenu-list-major-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'imenu-list-goto-entry)
    (define-key map (kbd "SPC") #'imenu-list-display-entry)
    map))

(define-derived-mode imenu-list-major-mode special-mode "Ilist"
  "Major mode for showing the `imenu' entries of a buffer (an Ilist).
\\{imenu-list-mode-map}"
  (read-only-mode 1))


;;; define minor mode

(defvar imenu-list--timer nil)

(defun imenu-list-update-safe ()
  "Call `imenu-list-update', return nil if an error occurs."
  (ignore-errors (imenu-list-update)))

;;;###autoload
(define-minor-mode imenu-list-minor-mode
  nil :global t
  (if imenu-list-minor-mode
      (progn
        (imenu-list-get-buffer-create)
        (ignore-errors (imenu-list-noselect))
        (setq imenu-list--timer
              (run-with-idle-timer 1 t #'imenu-list-update-safe)))
    (cancel-timer imenu-list--timer)))

(provide 'imenu-list)

;;; imenu-list.el ends here
