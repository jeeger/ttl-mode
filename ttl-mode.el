;;; ttl-mode.el --- mode for Turtle (and Notation 3)
;; ttl-mode.el is released under the terms of the two-clause BSD licence:
;;
;; Copyright 2003-2007, Hugo Haas <http://www.hugoh.net>
;; Copyright 2011-2012, Norman Gray <https://nxg.me.uk>
;; Copyright 2013, Daniel Gerber <https://danielgerber.net>
;; Copyright 2016, Peter Vasil <http://petervasil.net>
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms,
;; with or without modification,
;; are permitted provided that the following conditions are met:
;;
;; 1. Redistributions of source code must retain the above copyright
;; notice, this list of conditions and the following disclaimer.
;;
;; 2. Redistributions in binary form must reproduce the above copyright
;; notice, this list of conditions and the following disclaimer in the
;; documentation and/or other materials provided with the distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;; HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;
;;
;;; Commentary:
;;
;; See Hugo's commentary for original goals and further discussion,
;; at http://larve.net/people/hugo/2003/scratchpad/NotationThreeEmacsMode.html
;; Also draws on http://dishevelled.net/elisp/turtle-mode.el (which is for the _other_ turtle!)

;; Project hosted at <https://bitbucket.org/nxg/ttl-mode>.  See there for updates.

;; For documentation on Notation 3, see:
;; http://www.w3.org/DesignIssues/Notation3.html

;; Current features:
;; - *Turtle* grammar subset
;; - approximate syntax highlighting
;; - comment/uncomment block with M-;
;; - indentation

;; To use:
;;
;; (autoload 'ttl-mode "ttl-mode")
;; (add-hook 'ttl-mode-hook 'turn-on-font-lock)
;; (add-to-list 'auto-mode-alist '("\\.\\(n3\\|ttl\\|trig\\)\\'" . ttl-mode))

;;; Code:

(require 'cl-lib)

(defgroup ttl nil "Customization for ttl-mode" :group 'text)

(defcustom ttl-indent-level 4
  "Number of spaces for each indentation step in `ttl-mode'."
  :type 'integer)

(defcustom ttl-electric-punctuation t
  "If non-nil, `\;' or `\.' will self insert, reindent the line, and do a newline. (To insert while t, do: \\[quoted-insert] \;)."
  :type 'boolean)

(defcustom ttl-indent-on-idle-timer t
  "If non-nil, will automatically indent a line after `ttl-idle-timer-timeout'."
  :type 'boolean)

(defcustom ttl-indent-idle-timer-period 2
  "If `ttl-indent-on-idle-timer' is non-nil, indent after EMACS has been idle for this many seconds."
  :type 'integer)


(defvar ttl-indent-idle-timer nil "TTL-mode autoindent idle timer if idle auto indentation is used (`ttl-indent-on-idle-timer' is non-nil).")

;;;###autoload
(define-derived-mode ttl-mode prog-mode "N3/Turtle mode"
  "Major mode for Turtle RDF documents."

  ;; Comments syntax
  (set (make-local-variable 'comment-start) " # ")
  (modify-syntax-entry ?\n "> " ttl-mode-syntax-table)
  (modify-syntax-entry ?\' "\"" ttl-mode-syntax-table)
  ;; fontification
  (setq font-lock-defaults
        `((,(regexp-opt '("@prefix" "@base" "@keywords" "PREFIX" "BASE" "@forAll" "@forSome" "true" "false" "a") 'symbols)  ;keywords
           ("\\^\\^[^,;.]+" 0 font-lock-preprocessor-face t) ;literal types
	   ("\\?[[:word:]_]+" 0 font-lock-variable-name-face) ;Existentially quantified variables
           ("@[[:word:]_]+" . font-lock-preprocessor-face) ;languages
	   ; Does not work with iris containing multiple colons (one:two:three is apparently allowed.)
           ("\\(:?[-[:alnum:]]+\\|_\\)?:" . font-lock-type-face)       ;prefix
           (":\\([[:word:]_-]+\\)\\>" 1 font-lock-constant-face nil) ;suffix
	   ;; TODO: This incorrectly highlights resources in strings.
           ("<.*?>" 0 font-lock-function-name-face t) ;resources
           ("[,;.]" 0 font-lock-keyword-face))))
  
  ;; indentation
  (set (make-local-variable 'indent-line-function) 'ttl-indent-line)
  (set (make-local-variable 'indent-tabs-mode) nil)
  (set (make-local-variable 'syntax-propertize-function) 'ttl-propertize-comments)
  (setq show-trailing-whitespace t)
  (if (and ttl-indent-on-idle-timer (not ttl-indent-idle-timer))
      (setq ttl-indent-idle-timer (run-with-idle-timer ttl-indent-idle-timer-period t 'ttl-idle-indent))
    (when ttl-indent-idle-timer
      (setq ttl-indent-idle-timer (cancel-timer ttl-indent-idle-timer)))))

;; electric punctuation
(define-key ttl-mode-map (kbd "\,") 'ttl-electric-comma)
(define-key ttl-mode-map (kbd "\;") 'ttl-electric-semicolon)
(define-key ttl-mode-map (kbd "\,") 'ttl-electric-comma)
(define-key ttl-mode-map (kbd "\.") 'ttl-electric-dot)
(define-key ttl-mode-map [backspace] 'ttl-hungry-delete-backwards)


;; Could be replaced with a call to syntax-propertize-rules. See
;; https://emacs.stackexchange.com/questions/36909/how-can-i-make-syntax-propertize-skip-part-of-the-buffer
(defun ttl-propertize-comments (start end)
  "Set the syntax class to `comment-start` for all hashes that are prepended by a space between START and END."
  (save-excursion
    (goto-char start)
    (save-match-data
      (while (search-forward "#" end t)
	(let ((char-before (buffer-substring-no-properties
			    (max (point-min) (- (point) 2))
			    (min (point-max) (- (point) 1)))))
	  (when (or (equal char-before " ")
		    (equal char-before "\n"))
	    (put-text-property (match-beginning 0) (match-end 0) 'syntax-table '(11))))))))

(defun ttl-indent-line ()
  "Indent current line."
  (interactive)
  (save-excursion
    (indent-line-to
     (or (ttl-calculate-indentation) 0)))
  (move-to-column (max (current-indentation) (current-column))))

(defun ttl-idle-indent ()
  "Indent the current line, and check you're in an ttl-mode buffer."
  (when (eq major-mode 'ttl-mode)
      (ttl-indent-line)))

(defun ttl-calculate-indentation ()
  "Calculate the indentation for the current line."
  (save-excursion
    (backward-to-indentation 0)
    (cl-destructuring-bind
	(last-indent last-character after-prefix)
	(save-excursion (ttl-skip-uninteresting-lines) (list (current-indentation) (char-before) (ttl-in-prefix-line)))
      (let* ((syntax-info (syntax-ppss))
	     (base-indent (* ttl-indent-level (ttl-adjusted-paren-depth (nth 9 syntax-info)))))
	(cond
	 ;; in multiline string
	 ((nth 3 syntax-info) (current-indentation))
	 ;; First line in buffer.
	 ((= (line-number-at-pos (point) t) 1) 0)
	 ;; beginning of stanza
	 ((and (or (looking-at "@")         ; @prefix, @base, @keywords.
		   (looking-at "PREFIX")
		   (looking-at "BASE"))
	       (not (looking-at "\\(@forSome\\)\\|\\(@forAll\\)"))) ; @forAll and @forSome should be indented normally.
	  0)
	 ;; ((looking-at "#") base-indent)
	 ((looking-at "[])}]")		; Indent to level of matching parenthesis.
	  (save-excursion
	    (goto-char (nth 1 syntax-info))
	    (current-indentation)))
	 ((and (not (bobp))
               (or (ttl-first-line-of ?\[ last-character)
                   (ttl-first-line-of ?\( last-character)
                   (ttl-first-line-of ?\{ last-character)))
	  (+ last-indent ttl-indent-level))
	 ((eq ?. last-character) base-indent)
         (after-prefix 0)
         ((ttl-in-blank-node) base-indent)
	 (last-character (+ base-indent ttl-indent-level)))))))

(defun ttl-adjusted-paren-depth (parenpos)
  "Calculate parenthesis depth from PARENPOS, ignoring parentheses on the same line."
  ;; Just enough common lisp to be dangerous.
  (length (delete-dups (cl-loop for pos in parenpos collect (line-number-at-pos pos)))))

(defun ttl-in-prefix-line ()
  (save-excursion
    (beginning-of-line)
    (looking-at (rx (or "@" "PREFIX" "BASE")))))

(defun ttl-skip-uninteresting-lines ()
  "Skip backwards to the first non-comment content."
  (forward-line -1)
  ;; First, skip lines that consist only of comments.
  (while (and
          (not (bobp))
          ;; Skip comment lines
          (or
           (string-match (rx (and string-start (* blank) ?# (*? not-newline) line-end)) (thing-at-point 'line))
           ;; Skip empty lines
           (string-match (rx (and string-start (* blank) line-end)) (thing-at-point 'line))))
    (forward-line -1))
  ;; Then, go to last non-comment-character
  (if (search-forward " #" (point-at-eol) t)
      (backward-char 2)
    (end-of-line)))
  

(defun ttl-insulate ()
  "Return non-nil if this location should not be electrified."
  (or (not ttl-electric-punctuation)
      (let '(s (syntax-ppss))
        (or (nth 3 s)
            (nth 4 s)
            (ttl-in-resource)))))

(defun ttl-last-bracket-is (brack)
  "Is the last bracket equal to BRACK?"
  (let ((list-start (nth 1 (syntax-ppss))))
       (and list-start
	    (equal (char-after list-start) brack))))

(defun ttl-first-line-of (brack lastchar)
  "Whether we are in the first line of a node introduced by BRACK.

LASTCHAR is the last character of the preceding line."
  (and (ttl-last-bracket-is brack)
       (eq brack lastchar)))

(defun ttl-in-blank-node ()
  "Is point within a blank node, marked by [...]?"
  (ttl-last-bracket-is ?\[))
  
(defun ttl-in-graph ()
  "Is point within a graph?"
  (ttl-last-bracket-is ?\{))

(defun ttl-in-object-list ()
  "Are we in a node list, marked by (...)?"
  (ttl-last-bracket-is ?\())

(defun ttl-in-resource ()
  "Is point within a resource, marked by <...>?"
  (save-excursion
    (and (re-search-backward "[<> ]" nil t)
         (looking-at "<"))))

(defun ttl-in-comment ()
  "Are we in a comment, marked by a hash mark?"
  (nth 4 (syntax-ppss)))

(defun ttl-in-string ()
  "Are we in a string?"
  (nth 3 (syntax-ppss)))

(defun ttl-electric-comma ()
  "Insert spaced comma, indent, insert newline and reindent."
  (interactive)
  (if (ttl-insulate) (insert ",")
    (progn
      (if (not (looking-back " " 1)) (insert " "))
      (insert ", ")
      (ttl-indent-line))))

(defun ttl-electric-semicolon ()
  "Insert spaced semicolon, indent, insert newline and reindent next line."
  (interactive)
  (if (ttl-insulate) (insert ";")
    (if (not (looking-back " " 1)) (insert " "))
    (insert ";")
    (reindent-then-newline-and-indent)))

(defun ttl-electric-dot ()
  "Insert spaced dot, insert newline, indent."
  (interactive)
  (if (and (ttl-in-blank-node)
	   (not (ttl-in-comment))
	   (not (ttl-in-string)))
      (message "No period in blank nodes.")
    (if (ttl-insulate) (insert ".")
      (if (not (looking-back " " 1)) (insert " "))
      (insert ".")
      (reindent-then-newline-and-indent))))

(defun ttl-skip-ws-backwards ()  ;adapted from cc-mode
  "Move backwards across whitespace."
  (while (progn
           (skip-chars-backward " \t\n\r\f\v")
           (and (eolp)
                (eq (char-before) ?\\)))
    (backward-char)))

(defun ttl-hungry-delete-backwards ()
  "Delete backwards hungrily.

Deletes either all of the preceding whitespace, or a single
non-whitespace character if there is no whitespace before point."
  (interactive)
  (let ((here (point)))
    (ttl-skip-ws-backwards)
    (if (/= (point) here)
        (delete-region (point) here)
      (backward-delete-char-untabify 1))))

(provide 'ttl-mode)
;;; ttl-mode.el ends here
