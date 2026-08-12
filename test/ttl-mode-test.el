;;; ttl-mode-test.el --- Tests for ttl-mode -*- lexical-binding: t; -*-

;;; Commentary:

;; Buttercup tests for ttl-mode.

;;; Code:

(require 'buttercup)
(require 'ttl-mode)

(defun ttl-lines (&rest lines)
  "Join LINES with newlines into one buffer-ready string.
The result ends with a newline, like a text file, so fixtures can be
written one line per string instead of as a flush-left literal."
  (concat (mapconcat #'identity lines "\n") "\n"))

(defun ttl-test-reindent (text)
  "Return TEXT after re-indenting every line in `ttl-mode'."
  (with-temp-buffer
    (insert text)
    (ttl-mode)
    (indent-region (point-min) (point-max))
    (buffer-string)))

(defun ttl-test-face-at (text search)
  "Fontify TEXT in `ttl-mode' and return the face where SEARCH begins."
  (with-temp-buffer
    (insert text)
    (ttl-mode)
    (font-lock-ensure)
    (goto-char (point-min))
    (search-forward search)
    (get-text-property (match-beginning 0) 'face)))

(describe "ttl-mode indentation is idempotent"
  (dolist (case `((:desc "prefixes and a simple statement"
                   :text ,(ttl-lines
                           "@prefix ex: <http://example.org/> ."
                           "@prefix sh: <http://www.w3.org/ns/shacl#> ."
                           ""
                           "ex:s a ex:Thing ;"
                           "    ex:p ex:o ;"
                           "    ex:q 1 ."))
                  (:desc "blank nodes written inline with the predicate"
                   :text ,(ttl-lines
                           ":Shape a sh:NodeShape ;"
                           "    sh:property [ sh:datatype xsd:string ;"
                           "            sh:maxCount 1 ;"
                           "            sh:path rdfs:label ],"
                           "        [ sh:datatype xsd:double ;"
                           "            sh:path :rate ] ;"
                           "    sh:targetClass :X ."))
                  (:desc "a blank node opened at end of line"
                   :text ,(ttl-lines
                           "ex:s ex:p ["
                           "            ex:a 1 ;"
                           "            ex:b 2 ] ."))
                  (:desc "a TriG named graph"
                   :text ,(ttl-lines
                           "ex:g {"
                           "    ex:s ex:p ex:o ;"
                           "        ex:q ex:r ."
                           "}"))
                  (:desc "a comment line between statements"
                   :text ,(ttl-lines
                           "ex:s ex:p ex:o ;"
                           "    # a comment"
                           "    ex:q 1 ."))
                  (:desc "a hash inside a resource is not a comment"
                   :text ,(ttl-lines
                           "ex:s ex:p <http://example.org/thing#frag> ;"
                           "    ex:q 1 ."))
                  (:desc "a closing bracket on its own line aligns under its opener"
                   :text ,(ttl-lines
                           "ex:s ex:p ["
                           "            ex:a 1 ;"
                           "            ex:b 2"
                           "] ."))
                  (:desc "several brackets opened on one line count as one step"
                   :text ,(ttl-lines
                           "ex:s ex:p [ ex:q [ ex:a 1 ;"
                           "            ex:b 2 ] ] ."))))
    (let ((desc (plist-get case :desc))
          (text (plist-get case :text)))
      (it desc
        (expect (ttl-test-reindent text) :to-equal text)))))

(describe "ttl-mode indentation converges"
  (it "restores nesting a flattened blank-node list lost"
    (expect
     (ttl-test-reindent
      (ttl-lines
       ":Shape a sh:NodeShape ;"
       "    sh:property [ sh:datatype xsd:string ;"
       "    sh:maxCount 1 ;"
       "    sh:path rdfs:label ],"
       "    [ sh:datatype xsd:double ;"
       "    sh:path :rate ] ;"
       "    sh:targetClass :X ."))
     :to-equal
     (ttl-lines
      ":Shape a sh:NodeShape ;"
      "    sh:property [ sh:datatype xsd:string ;"
      "            sh:maxCount 1 ;"
      "            sh:path rdfs:label ],"
      "        [ sh:datatype xsd:double ;"
      "            sh:path :rate ] ;"
      "    sh:targetClass :X .")))

  (it "moves a mis-indented first line to column 0"
    (expect (ttl-test-reindent "        ex:s ex:p ex:o .\n")
            :to-equal "ex:s ex:p ex:o .\n"))

  (it "snaps a mis-indented @prefix to column 0"
    (expect (ttl-test-reindent "        @prefix ex: <http://example.org/> .\n")
            :to-equal "@prefix ex: <http://example.org/> .\n")))

(describe "ttl-mode comment syntax"
  (it "treats a hash after whitespace as a comment"
    (with-temp-buffer
      (insert "ex:s ex:p ex:o . # a comment\n")
      (ttl-mode)
      (goto-char (point-min))
      (search-forward "a comment")
      (expect (ttl-in-comment) :to-be-truthy)))

  (it "leaves a hash inside a resource alone"
    (with-temp-buffer
      (insert "ex:s ex:p <http://example.org/#frag> .\n")
      (ttl-mode)
      (goto-char (point-min))
      (search-forward "#frag")
      (expect (ttl-in-comment) :to-be nil)))

  (it "treats a hash at the very start of the buffer as a comment"
    (with-temp-buffer
      (insert "# leading comment\nex:s ex:p ex:o .\n")
      (ttl-mode)
      (goto-char (point-min))
      (search-forward "leading")
      (expect (ttl-in-comment) :to-be-truthy))))

(describe "ttl-in-blank-node"
  (it "is truthy inside square brackets"
    (with-temp-buffer
      (insert "ex:s ex:p [ ex:a 1 ] .\n")
      (ttl-mode)
      (goto-char (point-min))
      (search-forward "ex:a")
      (expect (ttl-in-blank-node) :to-be-truthy)))

  (it "is nil in an ordinary statement"
    (with-temp-buffer
      (insert "ex:s ex:p ex:o .\n")
      (ttl-mode)
      (goto-char (point-min))
      (search-forward "ex:o")
      (expect (ttl-in-blank-node) :to-be nil))))

(describe "ttl-in-string"
  (it "treats single-quoted text as a string"
    (with-temp-buffer
      (insert "ex:s ex:p 'hello world' .\n")
      (ttl-mode)
      (goto-char (point-min))
      (search-forward "hello")
      (expect (ttl-in-string) :to-be-truthy)))

  (it "is nil outside strings"
    (with-temp-buffer
      (insert "ex:s ex:p ex:o .\n")
      (ttl-mode)
      (goto-char (point-min))
      (search-forward "ex:o")
      (expect (ttl-in-string) :to-be nil))))

(describe "ttl-electric-dot"
  (it "refuses to insert a period inside a blank node"
    (with-temp-buffer
      (insert "ex:s ex:p [ ex:a 1 ")
      (ttl-mode)
      (goto-char (point-max))
      (let ((before (buffer-string)))
        (ttl-electric-dot)
        (expect (buffer-string) :to-equal before))))

  ;; jeeger/ttl-mode issue #3: a dot belongs in a string even in a blank node.
  (it "allows a period inside a string within a blank node"
    (with-temp-buffer
      (insert "ex:s ex:p [ ex:a 'text")
      (ttl-mode)
      (goto-char (point-max))
      (let ((before (buffer-string)))
        (ttl-electric-dot)
        (expect (buffer-string) :to-equal (concat before "."))))))

(describe "ttl-mode highlighting"
  ;; jeeger/ttl-mode issue #5: a dash is a valid prefix character.
  (it "highlights a prefix containing a dash"
    (expect (ttl-test-face-at "my-prefix:thing a ex:Y ." "my-prefix")
            :to-be 'font-lock-type-face))

  ;; jeeger/ttl-mode issue #4: quoted literals are strings.
  (it "highlights a single-quoted literal as a string"
    (expect (ttl-test-face-at "ex:s ex:p 'a literal' ." "literal")
            :to-be 'font-lock-string-face)))

;;; ttl-mode-test.el ends here
