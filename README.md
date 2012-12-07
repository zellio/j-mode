# J Mode

Provides font-lock, REPL integration ( via comint ) and a basic help
documentation for the [J programming language](http://www.jsoftware.com).

## Installation

Currently the method of installation is entirely manually. Fetch the source via
git or direct download, place in your load path and load / require normally.

```lisp
;; Add this to your emacs config
(add-to-list 'load-path "/path/to/j-mode/")
(autoload 'j-mode "j-mode.el" "Major mode for editing J files" t)

;; Add for detection of j source files if the auto-load fails
(add-to-list 'auto-mode-alist '("\\.ij[rstp]$" . j-mode)))
```

## J Font Lock

`j-mode` font-lock provides four new faces for management of the coloring
various parts of speech. Those faces are `j-verb-face` `j-adverb-face`
`j-conjunction-face` `j-other-face`. They can be modified like any of the
standard built in faces to help meet your need.

```lisp
(custom-set-face
 '(j-verb-face ((t (:foreground "Red"))))
 '(j-adverb-face ((t (:foreground "Green"))))
 '(j-conjunction-face ((t (:foreground "Blue"))))
 '(j-other-face ((t (:foreground "Black")))))
```

## J Console

Interaction to the j REPL is provided via the comint module. The `j-console`
function starts the REPL session in a new buffer.

The module provides the following key bindings for convenience

* <kbd>C-c !</kbd> Runs the `j-console` function
* <kbd>C-c C-l</kbd> Executes the current line
* <kbd>C-c C-r</kbd> Executes the current region
* <kbd>C-c C-c</kbd> Executes the current buffer

## J Help

`j-help` provides access to the
[J software vocabulary](http://www.jsoftware.com/help/dictionary/vocabul.htm)
via two functions `j-help-lookup-symbol` and
`j-help-lookup-symbol-at-point`. `j-help-look-symbol` takes one string argument
( generally via the mini-buffer ) which it then looks up.
`j-help-lookup-symbol-at-point` attempts to determine which symbol is under your
cursor and then passes that to `j-help-lookup-symbol`.

The module provides the following key bindings for convenience

* <kbd>C-c h</kbd> runs `j-help-lookup-symbol`
* <kbd>C-c C-h</kbd> `j-help-lookup-symbol-at-point`

### License

Copyright (C) 2012 Zachary Elliott

Distributed under the GNU General Public License; see <kbd>C-h t</kbd> in emacs
to view.
