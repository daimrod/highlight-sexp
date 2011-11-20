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

(eval-when-compile (require 'cl))

(defgroup highlight-region nil
  "Highlight region"
  :group 'faces
  :group 'matching)

(defvar hl-region-overlay
  nil
  "The current overlay.")
(make-local-variable 'hl-region-overlay)

(defcustom hl-region-background-color
  "#4b3b4b"
  "*The color used for the background of the highlighted region."
  :type 'color
  :group 'highlight-region)

(defcustom hl-region-foreground-color
  nil
  "*The color used for the foreground of the highlighted region"
  :type 'color
  :group 'highlight-region)

(make-face 'hl-region-face)
(defcustom hl-region-face
  nil
  "*The face used for the highlighted region."
  :group 'highlight-region)

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

(defun hl-region-delete-overlay ()
  (if hl-region-overlay
      (delete-overlay hl-region-overlay))
  (setf hl-region-overlay nil))

(defun hl-region-highlight ()
  (let ((text-property (get-text-property (point) 'face)))
    (cond ((not (or (eq text-property 'font-lock-string-face)
                    (eq text-property 'font-lock-comment-face)))
           (let* ((sppss (syntax-ppss))
                  (start (elt sppss 1))
                  (inside-a-string? (elt sppss 3))
                  (inside-a-comment? (elt sppss 4))
                  end)
             (cond ((not (or (not start)
                             inside-a-string?
                             inside-a-comment?))
                    (setf end (scan-sexps start 1))
                    (when end
                      (move-overlay hl-region-overlay start end)))
                   (t (move-overlay hl-region-overlay 0 0)))))
          (t (move-overlay hl-region-overlay 0 0)))))

(defun hl-region-create-overlay ()
  (let (attribute)
    (setf attribute (face-attr-construct 'hl-region-face))
    (if hl-region-foreground-color
        (setf attribute (plist-put attribute :foreground hl-region-foreground-color)))
    (if hl-region-background-color
        (setf attribute (plist-put attribute :background hl-region-background-color)))
    (setf hl-region-overlay (make-overlay 0 0))
    (overlay-put hl-region-overlay 'face attribute)))

(provide 'highlight-region)
