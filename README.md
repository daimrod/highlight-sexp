Highlight s-exp
===============

Highlight-sexp-mode is a minor mode for GNU/Emacs that highlights the
s-exp at the current position.

This this heavily inspired by highlight-parentheses-mode which
highlights all parentheses but not the s-exp.

Installation and configuration
==============================

    (load "path-to-highlight.el")
    (require 'highlight-sexp)
    (add-hook 'lisp-mode-hook 'highlight-sexp-mode)
    (add-hook 'emacs-lisp-mode-hook 'highlight-sexp-mode)

Customization
=============

The following variables are available for customization:

- hl-sexp-background-color
- hl-sexp-foreground-color
- hl-sexp-face
