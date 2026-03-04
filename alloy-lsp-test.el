;;; alloy-lsp-test.el --- Tests for alloy-lsp -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

;; Load alloy-lsp without starting the LSP client
(require 'alloy-lsp)

;; ---- Helpers

(defun alloy-lsp-test--make-lens (sline schar eline echar cmd-name args)
  "Build a CodeLens hash table matching the JSON structure from the server."
  (let ((lens (make-hash-table :test 'equal))
        (range (make-hash-table :test 'equal))
        (start (make-hash-table :test 'equal))
        (end (make-hash-table :test 'equal))
        (cmd (make-hash-table :test 'equal)))
    (puthash "line" sline start)
    (puthash "character" schar start)
    (puthash "line" eline end)
    (puthash "character" echar end)
    (puthash "start" start range)
    (puthash "end" end range)
    (puthash "range" range lens)
    (puthash "command" cmd-name cmd)
    (puthash "arguments" args cmd)
    (puthash "command" cmd lens)
    lens))

;; ---- alloy-lsp--lens-at-point

(ert-deftest alloy-lsp-test-lens-at-point-inside ()
  "Point inside a lens range returns that lens."
  (let ((lens (alloy-lsp-test--make-lens 5 0 5 20 "ExecuteAlloyCommand" [])))
    (cl-letf (((symbol-function 'lsp--cur-position)
               (lambda () '(:line 5 :character 10))))
      (should (eq (alloy-lsp--lens-at-point (list lens)) lens)))))

(ert-deftest alloy-lsp-test-lens-at-point-start-boundary ()
  "Point at exact start of range is inside."
  (let ((lens (alloy-lsp-test--make-lens 3 5 3 15 "ExecuteAlloyCommand" [])))
    (cl-letf (((symbol-function 'lsp--cur-position)
               (lambda () '(:line 3 :character 5))))
      (should (eq (alloy-lsp--lens-at-point (list lens)) lens)))))

(ert-deftest alloy-lsp-test-lens-at-point-end-boundary ()
  "Point at exact end of range is inside (<=)."
  (let ((lens (alloy-lsp-test--make-lens 3 5 3 15 "ExecuteAlloyCommand" [])))
    (cl-letf (((symbol-function 'lsp--cur-position)
               (lambda () '(:line 3 :character 15))))
      (should (eq (alloy-lsp--lens-at-point (list lens)) lens)))))

(ert-deftest alloy-lsp-test-lens-at-point-before-range ()
  "Point before range returns nil."
  (let ((lens (alloy-lsp-test--make-lens 5 10 5 20 "ExecuteAlloyCommand" [])))
    (cl-letf (((symbol-function 'lsp--cur-position)
               (lambda () '(:line 5 :character 9))))
      (should-not (alloy-lsp--lens-at-point (list lens))))))

(ert-deftest alloy-lsp-test-lens-at-point-after-range ()
  "Point after range returns nil."
  (let ((lens (alloy-lsp-test--make-lens 5 10 5 20 "ExecuteAlloyCommand" [])))
    (cl-letf (((symbol-function 'lsp--cur-position)
               (lambda () '(:line 5 :character 21))))
      (should-not (alloy-lsp--lens-at-point (list lens))))))

(ert-deftest alloy-lsp-test-lens-at-point-multiline ()
  "Point on a middle line of a multiline range matches."
  (let ((lens (alloy-lsp-test--make-lens 2 0 8 0 "ExecuteAlloyCommand" [])))
    (cl-letf (((symbol-function 'lsp--cur-position)
               (lambda () '(:line 5 :character 0))))
      (should (eq (alloy-lsp--lens-at-point (list lens)) lens)))))

(ert-deftest alloy-lsp-test-lens-at-point-line-before-multiline ()
  "Point on line before a multiline range returns nil."
  (let ((lens (alloy-lsp-test--make-lens 3 0 8 0 "ExecuteAlloyCommand" [])))
    (cl-letf (((symbol-function 'lsp--cur-position)
               (lambda () '(:line 2 :character 50))))
      (should-not (alloy-lsp--lens-at-point (list lens))))))

(ert-deftest alloy-lsp-test-lens-at-point-line-after-multiline ()
  "Point on line after a multiline range returns nil."
  (let ((lens (alloy-lsp-test--make-lens 3 0 8 0 "ExecuteAlloyCommand" [])))
    (cl-letf (((symbol-function 'lsp--cur-position)
               (lambda () '(:line 9 :character 0))))
      (should-not (alloy-lsp--lens-at-point (list lens))))))

(ert-deftest alloy-lsp-test-lens-at-point-multiple-returns-first ()
  "With overlapping lenses, returns the first match."
  (let ((a (alloy-lsp-test--make-lens 1 0 1 20 "ExecuteAlloyCommand" []))
        (b (alloy-lsp-test--make-lens 1 5 1 15 "ExecuteAlloyCommand" [])))
    (cl-letf (((symbol-function 'lsp--cur-position)
               (lambda () '(:line 1 :character 10))))
      (should (eq (alloy-lsp--lens-at-point (list a b)) a)))))

(ert-deftest alloy-lsp-test-lens-at-point-empty-list ()
  "Empty lens list returns nil."
  (cl-letf (((symbol-function 'lsp--cur-position)
             (lambda () '(:line 0 :character 0))))
    (should-not (alloy-lsp--lens-at-point '()))))

;; ---- alloy-lsp--lens-command-args

(ert-deftest alloy-lsp-test-lens-command-args-vector ()
  "Args stored as vector are converted to a list."
  (let ((lens (alloy-lsp-test--make-lens 0 0 0 0 "ExecuteAlloyCommand"
                                         ["file:///foo.als" 0 3 0])))
    (should (equal (alloy-lsp--lens-command-args lens)
                   '("file:///foo.als" 0 3 0)))))

(ert-deftest alloy-lsp-test-lens-command-args-list ()
  "Args already a list are returned as-is."
  (let ((lens (alloy-lsp-test--make-lens 0 0 0 0 "ExecuteAlloyCommand"
                                         '("file:///foo.als" 1 5 0))))
    (should (equal (alloy-lsp--lens-command-args lens)
                   '("file:///foo.als" 1 5 0)))))

(ert-deftest alloy-lsp-test-lens-command-args-wrong-command ()
  "Non-ExecuteAlloyCommand raises user-error."
  (let ((lens (alloy-lsp-test--make-lens 0 0 0 0 "SomeOtherCommand" [])))
    (should-error (alloy-lsp--lens-command-args lens) :type 'user-error)))

(ert-deftest alloy-lsp-test-lens-command-args-nil-command ()
  "Lens with no command hash raises user-error."
  (let ((lens (make-hash-table :test 'equal))
        (range (make-hash-table :test 'equal))
        (start (make-hash-table :test 'equal))
        (end (make-hash-table :test 'equal)))
    (puthash "line" 0 start) (puthash "character" 0 start)
    (puthash "line" 0 end) (puthash "character" 0 end)
    (puthash "start" start range) (puthash "end" end range)
    (puthash "range" range lens)
    ;; no "command" key
    (should-error (alloy-lsp--lens-command-args lens) :type 'user-error)))

(provide 'alloy-lsp-test)
;;; alloy-lsp-test.el ends here
