
;;; j-mode.el --- Major mode for editing J programs

;; Copyright (C) 2012 Zachary Elliott
;;
;; Authors: Zachary Elliott <ZacharyElliott1@gmail.com>
;; URL: http://github.com/zellio/j-mode
;; Version: 0.0.5
;; Keywords: J, Langauges

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Provides font-lock and basic REPL integration for the J programming language
;; (http://www.jsoftware.com)

;;; License:

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 3 of the License, or (at your option) any later
;; version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.
;;
;; You should have received a copy of the GNU General Public License along with
;; GNU Emacs; see the file COPYING.  If not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
;; USA.

;;; Code:


(require 'comint)


(defconst j-mode-version "0.0.5"
  "`j-mode' version")

(defgroup j-mode nil
  "A mode for J"
  :group 'languages
  :prefix "j-")

(defcustom j-mode-hook nil
  "Hook called by `j-mode'."
  :type 'hook
  :group 'j-)


(defgroup j-faces nil
  "Faces for j-mode font-lock"
  :group 'j-)

(defmacro build-faces ( &rest faces )
  `(eval-when-compile
     ,@(mapcan (lambda ( x )
                 (let* ((name (car x))
                        (body (cdr x)))
                   `((defvar ,name ',name)
                     (defface ,name ,@body))))
               faces)))

(build-faces
 (j-verb-face
  `((t (:foreground "Red")))
  "Font Lock mode face used to higlight vrebs"
  :group 'j-faces)

 (j-adverb-face
  `((t (:foreground "Green")))
  "Font Lock mode face used to higlight adverbs"
  :group 'j-faces)

 (j-conjunction-face
  `((t (:foreground "Blue")))
  "Font Lock mode face used to higlight conjunctions"
  :group 'j-faces)

 (j-other-face
  `((t (:foreground "Black")))
  "Font Lock mode face used to higlight others"
  :group 'j-faces))


(defvar j-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c !") 'j-console)
    (define-key map (kbd "C-c C-c") 'j-execute-buffer)
    (define-key map (kbd "C-c C-r") 'j-execute-region)
    (define-key map (kbd "C-c C-l") 'j-execute-line)
    map)
  "Keymap for J major mode")


(defvar j-mode-menu nil "")
(easy-menu-define j-mode-menu j-mode-map "J Mode menu"
  '("J"
    ["Start J Console" j-console t]
    ["Execute Buffer" j-execute-buffer]
    ["Execute Region" j-execute-region]
    ["Execute Line" j-execute-line]
    "---"
    ["Help on J-mode" describe-mode t]
    "---"
    ("Sub-Menu")))


(defvar j-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\{ "."  table)
    (modify-syntax-entry ?\} "."  table)
    (modify-syntax-entry ?\[ "."  table)
    (modify-syntax-entry ?\] "."  table)
    (modify-syntax-entry ?\" "."  table)
    (modify-syntax-entry ?\\ "."  table)
    (modify-syntax-entry ?\. "w"  table)
    (modify-syntax-entry ?\: "w"  table)
    (modify-syntax-entry ?\( "()"  table)
    (modify-syntax-entry ?\) ")("  table)
    (modify-syntax-entry ?\' "\"" table)
    (modify-syntax-entry ?\N "w 1" table)
    (modify-syntax-entry ?\B "w 2" table)
    (modify-syntax-entry ?\n ">" table)
    (modify-syntax-entry ?\r ">" table)
    table)
  "Syntax table for j-mode")


(defvar j-mode-constants '())

(defvar j-mode-control-structures
  '("assert."  "break."  "continue."  "while."  "whilst."  "for."  "do."  "end."
    "if."  "else."  "elseif."  "return."  "select."  "case."  "fcase."  "throw."
    "try."  "catch."  "catchd."  "catcht."  "end."
    ;; "for_[a-zA-Z]+\\."  "goto_[a-zA-Z]+\\."  "label_[a-zA-Z]+\\."
    ))

(defvar j-mode-foreign-conjunctions
  '("0!:" "1!:" "2!:" "3!:" "4!:" "5!:" "6!:" "7!:" "8!:" "9!:" "11!:" "13!:"
    "15!:" "18!:" "128!:" ))

(defvar j-mode-len-3-verbs
  '("_9:" "p.." "{::"))
(defvar j-mode-len-2-verbs
  '("x:" "u:" "s:" "r." "q:" "p:" "p." "o." "L." "j." "I." "i:" "i." "E." "e."
    "C." "A." "?." "\":" "\"." "}:" "}." "{:" "{." "[:" "/:" "#:" "#." ";:" ",:"
    ",." "|:" "|." "~:" "~." "$:" "$." "^." "%:" "%." "-:" "-." "*:" "*."  "+:"
    "+." "_:" ">:" ">." "<:" "<."))
(defvar j-mode-len-1-verbs
  '("?" "{" "]" "[" ":" "!" "#" ";" "," "|" "$" "^" "%" "-" "*" "+" ">" "<" "="))
(defvar j-mode-verbs
  (append j-mode-len-3-verbs j-mode-len-2-verbs j-mode-len-1-verbs))

(defvar j-mode-len-2-adverbs
  '("t:" "t." "M." "f." "b." "/."))
(defvar j-mode-len-1-adverbs
  '("}" "." "\\" "/" "~"))
(defvar j-mode-adverbs
  (append j-mode-len-2-adverbs j-mode-len-1-adverbs))

(defvar j-mode-len-3-others
  '("NB."))
(defvar j-mode-len-2-others
  '("=." "=:" "_." "a." "a:"))
(defvar j-mode-len-1-others
  '("_" ))
(defvar j-mode-others
  (append j-mode-len-3-others j-mode-len-2-others j-mode-len-1-others))

(defvar j-mode-len-3-conjunctions
  '("&.:"))
(defvar j-mode-len-2-conjunctions
  '("T." "S:" "L:" "H." "D:" "D." "d." "&:" "&." "@:" "@." "`:" "!:" "!." ";."
    "::" ":." ".:" ".." "^:"))
(defvar j-mode-len-1-conjunctions
  '("&" "@" "`" "\"" ":" "."))
(defvar j-mode-conjunctions
  (append j-mode-len-3-conjunctions j-mode-len-2-conjunctions j-mode-len-1-conjunctions))


(defvar j-mode-font-lock-keywords
  `(
    ("\\([_a-zA-Z0-9]+\\)\s*\\(=[.:]\\)"
     (1 font-lock-variable-name-face) (2 j-other-face))

    (,(regexp-opt j-mode-foreign-conjunctions) . font-lock-warning-face)
    (,(concat (regexp-opt j-mode-control-structures)
              "\\|\\(?:\\(?:for\\|goto\\|label\\)_[a-zA-Z]+\\.\\)")
     . font-lock-keyword-face)
    (,(regexp-opt j-mode-constants) . font-lock-constant-face)
    (,(regexp-opt j-mode-len-3-verbs) . j-verb-face)
    (,(regexp-opt j-mode-len-3-conjunctions) . j-conjunction-face)
    ;;(,(regexp-opt j-mode-len-3-others) . )
    (,(regexp-opt j-mode-len-2-verbs) . j-verb-face)
    (,(regexp-opt j-mode-len-2-adverbs) . j-adverb-face)
    (,(regexp-opt j-mode-len-2-conjunctions) . j-conjunction-face)
    ;;(,(regexp-opt j-mode-len-2-others) . )
    (,(regexp-opt j-mode-len-1-verbs) . j-verb-face)
    (,(regexp-opt j-mode-len-1-adverbs) . j-adverb-face)
    (,(regexp-opt j-mode-len-1-conjunctions) . j-conjunction-face)
    ;;(,(regexp-opt j-mode-len-1-other) . )
    ) "J Mode font lock keys words")


(defun j-font-lock-syntactic-face-function (state)
  (if (nth 3 state) font-lock-string-face ;; String Context
    (let* ((start-pos (nth 8 state)))
      (and (<= (+ start-pos 3) (point-max))
           (eq (char-after start-pos) ?N)
           (string= (buffer-substring-no-properties
                     start-pos (+ start-pos 3)) "NB.")
           font-lock-comment-face))))


;;;###autoload
(defun j-mode ()
  "Major mode for editing J code"
  (interactive)
  (kill-all-local-variables)
  (use-local-map j-mode-map)
  (setq mode-name "J"
        major-mode 'j-mode)
  (set-syntax-table j-mode-syntax-table)
  (set (make-local-variable 'comment-start)
       "NB.")
  (set (make-local-variable 'comment-start-skip)
       "\\(\\(^\\|[^\\\\\n]\\)\\(\\\\\\\\\\)*\\)NB. *")
  (set (make-local-variable 'font-lock-comment-start-skip)
       "NB. *")
  (set (make-local-variable 'font-lock-defaults)
       '(j-mode-font-lock-keywords
         nil nil nil nil
;;         (font-lock-mark-block-function . mark-defun)
         (font-lock-syntactic-face-function
          . j-font-lock-syntactic-face-function)))
  (run-mode-hooks 'j-mode-hook))


(defcustom j-cmd "jconsole"
  ""
  :type 'string
  :group 'j-)

(defcustom j-cmd-args '()
  ""
  :type 'string
  :group 'j-)

(defcustom j-cmd-conf nil
  ""
  :type 'string
  :group 'j-)

(defcustom j-cmd-buffer-name "J"
  ""
  :type 'string
  :group 'j-)

(defun j-create-interpreter ()
  (setq comint-process-echoes t)
  (apply 'make-comint j-cmd-buffer-name j-cmd j-cmd-conf j-cmd-args)
  (add-hook
   'comint-preoutput-filter-functions
   (lambda ( output )
     (if (string-match "^[ \r\n\t]+" output)
         (concat "  " (replace-match "" nil t output))
       output))))

(defun j-ensure-interpreter ()
  (or (get-process j-cmd-buffer-name)
      (progn
        (j-create-interpreter)
        (get-process j-cmd-buffer-name))))

(defmacro defun-wp ( name label args interactive &rest body )
  `(defun ,name ,args
     ,interactive
     (let* ((,@label (j-ensure-interpreter)))
       ,@body)))

(defun-wp j-execute-line ( interpreter ) ()
  (interactive)
  (let* ((line (buffer-substring-no-properties (point-at-bol)
                                               (point-at-eol))))
    (pop-to-buffer (process-buffer interpreter))
    (goto-char (point-max))
    (insert-string line)
    (comint-send-input)))

(defun-wp j-execute-region (interpreter) (start end)
  (interactive "r")
  (and (= start end) (error "Region is empty"))
  (let* ((region (buffer-substring-no-properties start end))
         (block-size (if (and (= start (point-min)) (= end (point-max)))
                         "buffer" "region"))
         (region (concat "\nNB. Sending " block-size "...\n" region)))
    (pop-to-buffer (process-buffer interpreter))
    (insert-string (concat "\n" region "\n"))
    (comint-send-input)))

(defun j-execute-buffer ()
  (interactive)
  (j-execute-region (point-min) (point-max)))

;;;###autoload
(defun j-console ()
  (interactive)
  (switch-to-buffer-other-window (process-buffer (j-ensure-interpreter))))


;;;###autoload
(progn
  (add-to-list 'auto-mode-alist '("\\.ij[rstp]$" . j-mode)))


(provide 'j-mode '(j-console))
