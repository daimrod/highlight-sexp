;; highlight-region.el
;; Copyright (C) 2011 Grégoire Jadi

;; Author: Grégoire Jadi <gregoire.jadi@gmail.com>

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(defvar hl-region-overlay
  nil
  "The current overlay.")
(make-local-variable 'hl-region-overlay)

(define-minor-mode highlight-region-mode
    "Minor mode to highlight the current zone according to its
    context.

ie: sexp, comment, string."
  nil
  " hl-r"
  nil
  (if highlight-region-mode
      (progn
        (hl-region-create-overlay)
        (add-hook 'post-command-hook 'hl-region-highlight nil t))
      (hl-region-delete-overlay)
      (kill-local-variable 'hl-region-overlay)
      (remove-hook 'post-command-hook 'hl-region-highlight t)))

(defun hl-region-create-overlay ()
  nil)

(defun hl-region-highlight ()
  nil)

(defun hl-region-delete-overlay ()
  nil)

(provide 'highlight-region)
