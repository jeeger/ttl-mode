;;; ttl-mode-test.el --- Tests for ttl-mode -*- lexical-binding: t; -*-

;;; Commentary:

;; Buttercup tests for ttl-mode.

;;; Code:

(require 'buttercup)
(require 'ttl-mode)

(defun ttl-test-reindent (text)
  "Return TEXT after re-indenting every line in `ttl-mode'."
  (with-temp-buffer
    (insert text)
    (ttl-mode)
    (indent-region (point-min) (point-max))
    (buffer-string)))

(describe "ttl-mode indentation is idempotent"
  (dolist (case '((:desc "prefixes and a simple statement"
                   :text "@prefix ex: <http://example.org/> .
@prefix sh: <http://www.w3.org/ns/shacl#> .

ex:s a ex:Thing ;
    ex:p ex:o ;
    ex:q 1 .
")
                  (:desc "blank nodes written inline with the predicate"
                   :text ":Shape a sh:NodeShape ;
    sh:property [ sh:datatype xsd:string ;
            sh:maxCount 1 ;
            sh:path rdfs:label ],
        [ sh:datatype xsd:double ;
            sh:path :rate ] ;
    sh:targetClass :X .
")
                  (:desc "a blank node opened at end of line"
                   :text "ex:s ex:p [
            ex:a 1 ;
            ex:b 2 ] .
")
                  (:desc "a TriG named graph"
                   :text "ex:g {
    ex:s ex:p ex:o ;
        ex:q ex:r .
}
")))
    (it (plist-get case :desc)
      (let ((text (plist-get case :text)))
        (expect (ttl-test-reindent text) :to-equal text)))))

(describe "ttl-mode indentation converges"
  (it "restores nesting a flattened blank-node list lost"
    (expect
     (ttl-test-reindent ":Shape a sh:NodeShape ;
    sh:property [ sh:datatype xsd:string ;
    sh:maxCount 1 ;
    sh:path rdfs:label ],
    [ sh:datatype xsd:double ;
    sh:path :rate ] ;
    sh:targetClass :X .
")
     :to-equal ":Shape a sh:NodeShape ;
    sh:property [ sh:datatype xsd:string ;
            sh:maxCount 1 ;
            sh:path rdfs:label ],
        [ sh:datatype xsd:double ;
            sh:path :rate ] ;
    sh:targetClass :X .
")))

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
      (expect (ttl-in-comment) :to-be nil))))

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

;;; ttl-mode-test.el ends here
