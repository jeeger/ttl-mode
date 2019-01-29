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
;;
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


;;;###autoload
(define-derived-mode ttl-mode prog-mode "N3/Turtle mode"
  "Major mode for Turtle RDF documents."

  ;; Comments syntax
  (set (make-local-variable 'comment-start) "# ")
  (modify-syntax-entry ?# "< b" ttl-mode-syntax-table)
  (modify-syntax-entry ?\n "> b" ttl-mode-syntax-table)

  ;; fontification
  (setq font-lock-defaults
        `((,(regexp-opt '("@prefix" "@base" "@keywords" "PREFIX" "BASE" "@forAll" "@forSome" "a") 'symbols)  ;keywords
           ("\\^\\^[^,;.]+" 0 font-lock-preprocessor-face t) ;literal types
	   ("\\?[[:word:]_]+" 0 font-lock-variable-name-face) ;Existentially quantified variables
	   ("_:[[:word:]_]+" 0 font-lock-variable-name-face) ; Named anonymous nodes
           ("@[[:word:]_]+" . font-lock-preprocessor-face) ;languages
           ("\\S-*?:" . font-lock-type-face)       ;prefix
           (":\\([[:word:]_-]+\\)\\>" 1 font-lock-constant-face nil) ;suffix
           ("<.*?>" 0 font-lock-function-name-face t) ;resources
           ("[,;.]" 0 font-lock-keyword-face) ;punctuation
           ("^\\s-*\\(#.*\\)" 1 font-lock-comment-face t) ;comment
           ) nil))

  ;; indentation
  (set (make-local-variable 'indent-line-function) 'ttl-indent-line)
  (set (make-local-variable 'indent-tabs-mode) nil))

;; electric punctuation
(define-key ttl-mode-map (kbd "\,") 'ttl-electric-comma)
(define-key ttl-mode-map (kbd "\;") 'ttl-electric-semicolon)
(define-key ttl-mode-map (kbd "\.") 'ttl-electric-dot)
(define-key ttl-mode-map [backspace] 'ttl-hungry-delete-backwards)


(defgroup ttl nil "Customization for ttl-mode")

(defcustom ttl-indent-level 4
  "Number of spaces for each indentation step in `ttl-mode'."
  :type 'integer)

(defcustom ttl-electric-punctuation t
  "*If non-nil, `\;' or `\.' will self insert, reindent the line, and do a newline. (To insert while t, do: \\[quoted-insert] \;)."
  :type 'boolean)


(defun ttl-indent-line ()
  (interactive)
  (save-excursion
    (indent-line-to
     (or (ignore-errors (ttl-calculate-indentation)) 0)))
  (move-to-column (max (current-indentation) (current-column))))

(defun ttl-calculate-indentation ()
  (save-excursion
    (backward-to-indentation 0)
    (let* ((bracket-level (first (syntax-ppss)))
	   (last-indent (save-excursion (forward-comment -2) (current-indentation)))
	   (base-indent (* bracket-level ttl-indent-level))
	   (last-character (save-excursion (forward-comment -2) (char-to-string (char-before)))))
      (cond
       ;; in multiline string
       ((nth 3 (syntax-ppss)) (current-indentation))
       ;; empty line
       ((looking-at "$") last-indent)
       ;; beginning of stanza
       ((and (or (looking-at "@")         ; @prefix, @base, @keywords.
		 (looking-at "#")	     ; Comments
		 (looking-at "PREFIX:")
		 (looking-at "BASE:"))
	     (not (looking-at "\\(@forSome\\)\\|\\(@forAll\\)"))) ; @forAll and @forSome should be indented normally.
	0)
       ((looking-at "[})]") (max 0 (- base-indent ttl-indent-level))) ; Closing brackets
       ((looking-at "]") (- last-indent ttl-indent-level))
       ((ttl-in-blank-node) 		; Inside blank node, all bets are off ☺
	(if (string-match-p "\\[" last-character) ; First line of blank node
	    (+ last-indent ttl-indent-level)
	  last-indent))
       ((ttl-in-object-list)		; Inside object list
	(if (string-match-p "(" last-character) ; First line of expanded object list
	    (+ last-indent ttl-indent-level)
	  last-indent))
       ((string-match-p "\\." last-character) base-indent)
       ((string-match-p ";" last-character) (+ base-indent ttl-indent-level))
       (t base-indent)))))

(defun ttl-insulate ()
  "Return true if this location should not be electrified"
  (or (not ttl-electric-punctuation)
      (let '(s (syntax-ppss))
        (or (nth 3 s)
            (nth 4 s)
            (ttl-in-resource-p)))))

(defun ttl-in-blank-node ()
  "Is point within a blank node, marked by [...]?"
  (let ((list-start (nth 1 (syntax-ppss))))
    (and list-start			; If not inside list, returns nil.
	 (equal (buffer-substring list-start (1+ list-start)) "["))))

(defun ttl-in-object-list ()
  "Are we in a node list, marked by (...)?"
  (let ((list-start (nth 1 (syntax-ppss))))
    (and list-start			; If not inside list, list-start is nil.
	 (equal (buffer-substring list-start (1+ list-start)) "("))))
  
(defun ttl-in-resource-p ()
  "Is point within a resource, marked by <...>?"
  (save-excursion
    (and (re-search-backward "[<> ]" nil t)
         (looking-at "<"))))

(defun ttl-electric-comma ()
  (interactive)
  (if (ttl-insulate) (insert ",")
    (if (not (looking-back " ")) (insert " "))
    (insert ",")
    (reindent-then-newline-and-indent)))

(defun ttl-electric-semicolon ()
  (interactive)
  (if (ttl-insulate) (insert ";")
    (if (not (looking-back " ")) (insert " "))
    (insert ";")
    (reindent-then-newline-and-indent)))

(defun ttl-electric-dot ()
  (interactive)
  (if (ttl-in-blank-node)
      (message "No period in blank nodes.")
    (if (ttl-insulate) (insert ".")
      (if (not (looking-back " ")) (insert " "))
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
  "Delete backwards, either all of the preceding whitespace,
or a single non-whitespace character if there is no whitespace before point."
  (interactive)
  (let ((here (point)))
    (ttl-skip-ws-backwards)
    (if (/= (point) here)
        (delete-region (point) here)
      (backward-delete-char-untabify 1))))

(provide 'ttl-mode)
;;; ttl-mode.el ends here
