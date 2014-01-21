# J Mode

Provides font-lock, REPL integration ( via comint ) and a basic help
documentation for the [J programming language](http://www.jsoftware.com).

## Installation

`j-mode` has been added to the el-get package managment system and can now
be installed via the `el-get-install` function.

To install the project manually fetch the source via git or direct download,
place in your load path and load / require normally.

```lisp
;; Add this to your emacs config
(add-to-list 'load-path "/path/to/j-mode/")
(autoload 'j-mode "j-mode.el" "Major mode for editing J files" t)

;; Add for detection of j source files if the auto-load fails
(add-to-list 'auto-mode-alist '("\\.ij[rstp]$" . j-mode))
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

NB. Java on many Linux systems provides an executable which is sadly named
`jconsole`. This means that there is a good chance `j-mode` will attempt to
start the Java console up instead of the J console when beginning a new REPL
session. The easiest fix for this, as I doubt that we can convince the Java
packagers to rename their executable, is to set the `j-console-cmd` variable
provided by `j-console.el`. This can be done either directly or via the
`custom-set-variables` block.


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

Copyright (C) 2012-2014 Zachary Elliott

Distributed under the GNU General Public License; see <kbd>C-h t</kbd> in emacs
to view.
