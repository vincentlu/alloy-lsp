;;; alloy-ts-mode.el --- Tree-sitter major mode for Alloy 6 -*- lexical-binding: t; -*-

;; Requires: Emacs 29+ with tree-sitter support
;; Put this file on your load-path, then:
;;   (require 'alloy-ts-mode)
;; Opening a .als file should activate alloy-ts-mode automatically.

(require 'treesit)

(defgroup alloy-ts nil
  "Tree-sitter support for the Alloy specification language."
  :group 'languages)

(defcustom alloy-ts-mode-indent-offset 2
  "Number of spaces for each indentation level in `alloy-ts-mode'."
  :type 'integer)

;; ---- Font-lock ----

(defvar alloy-ts-mode--font-lock-settings
  (treesit-font-lock-rules
   :language 'alloy
   :feature 'comment
   '((line_comment) @font-lock-comment-face
     (block_comment) @font-lock-comment-face)

   :language 'alloy
   :feature 'string
   '((string) @font-lock-string-face)

   :language 'alloy
   :feature 'number
   '((number) @font-lock-number-face)

   :language 'alloy
   :feature 'keyword
   '(["module" "open" "as" "sig" "extends" "in" "fact" "pred" "fun"
      "assert" "run" "check" "enum" "let" "for" "but" "expect"
      "exactly" "abstract" "private" "var" "disj"
      "all" "no" "some" "lone" "one" "sum" "set" "seq"
      "not" "and" "or" "implies" "iff" "else"
      "always" "eventually" "after" "before" "historically" "once"
      "until" "since" "releases" "triggered"
      "int"] @font-lock-keyword-face)

   :language 'alloy
   :feature 'builtin
   '((this_expr) @font-lock-builtin-face
     (iden_expr) @font-lock-builtin-face
     (none_expr) @font-lock-builtin-face
     (univ_expr) @font-lock-builtin-face
     (int_expr) @font-lock-builtin-face)

   :language 'alloy
   :feature 'type
   '((sig_decl names: (name_list (name) @font-lock-type-face))
     (enum_decl name: (name) @font-lock-type-face)
     (sig_extension (sig_ref (qual_name (name) @font-lock-type-face)))
     (sig_extension (sig_ref_union (sig_ref (qual_name (name) @font-lock-type-face)))))

   :language 'alloy
   :feature 'function
   '((pred_decl name: (name) @font-lock-function-name-face)
     (fun_decl name: (name) @font-lock-function-name-face))

   :language 'alloy
   :feature 'variable
   '((field_decl (name_list (name) @font-lock-variable-name-face))
     (decl (name_list (name) @font-lock-variable-name-face))
     (at_name (name) @font-lock-variable-name-face))

   :language 'alloy
   :feature 'label
   '((fact_decl name: (name) @font-lock-constant-face)
     (assert_decl name: (name) @font-lock-constant-face)
     (command label: (name) @font-lock-constant-face))

   :language 'alloy
   :feature 'module
   '((module_decl name: (qual_name) @font-lock-constant-face)
     (open_decl name: (qual_name) @font-lock-constant-face)
     (open_decl alias: (name) @font-lock-constant-face))

   :language 'alloy
   :feature 'operator
   '(["=" "!=" "!" "!in" "!<" "!>" "!<=" "!>=" "=<"
      "<" ">" "<=" ">=" "<=>" "=>" "&&" "||"
      "+" "-" "++" "&" "->" "<:" ":>" "." "~" "^" "*"
      "#" "<<" ">>" ">>>" ";" "'"] @font-lock-operator-face)

   :language 'alloy
   :feature 'bracket
   '(["(" ")" "[" "]" "{" "}"] @font-lock-bracket-face)

   :language 'alloy
   :feature 'delimiter
   '(["," ":" "|" "/" ".."] @font-lock-delimiter-face))
  "Tree-sitter font-lock settings for `alloy-ts-mode'.")

;; ---- Indentation ----

(defvar alloy-ts-mode--indent-rules
  `((alloy
     ;; Top-level: no indent
     ((parent-is "source_file") column-0 0)
     ;; Inside blocks/braces: indent
     ((parent-is "block_expression") parent-bol alloy-ts-mode-indent-offset)
     ((parent-is "sig_body") parent-bol alloy-ts-mode-indent-offset)
     ((parent-is "set_comprehension") parent-bol alloy-ts-mode-indent-offset)
     ;; Closing brace: align with opening
     ((node-is "}") parent-bol 0)
     ((node-is "]") parent-bol 0)
     ((node-is ")") parent-bol 0)
     ;; Parameters
     ((parent-is "decl_list") parent-bol alloy-ts-mode-indent-offset)
     ((parent-is "expr_list") parent-bol alloy-ts-mode-indent-offset)
     ;; General continuation
     ((parent-is "sig_decl") parent-bol alloy-ts-mode-indent-offset)
     ((parent-is "pred_decl") parent-bol alloy-ts-mode-indent-offset)
     ((parent-is "fun_decl") parent-bol alloy-ts-mode-indent-offset)
     ((parent-is "fact_decl") parent-bol alloy-ts-mode-indent-offset)
     ((parent-is "assert_decl") parent-bol alloy-ts-mode-indent-offset)
     ((parent-is "command") parent-bol alloy-ts-mode-indent-offset)
     ((parent-is "enum_decl") parent-bol alloy-ts-mode-indent-offset)
     ;; Default: no change
     (no-node parent-bol 0)))
  "Tree-sitter indentation rules for `alloy-ts-mode'.")

;; ---- Navigation / defun ----

(defvar alloy-ts-mode--defun-type-regexp
  (regexp-opt '("sig_decl" "pred_decl" "fun_decl" "fact_decl"
                "assert_decl" "command" "enum_decl" "let_decl"))
  "Regexp matching tree-sitter node types that count as top-level definitions.")

;; ---- Imenu ----

(defvar alloy-ts-mode--imenu-settings
  '(("Sig" "\\`sig_decl\\'" nil nil)
    ("Pred" "\\`pred_decl\\'" nil nil)
    ("Fun" "\\`fun_decl\\'" nil nil)
    ("Fact" "\\`fact_decl\\'" nil nil)
    ("Assert" "\\`assert_decl\\'" nil nil)
    ("Command" "\\`command\\'" nil nil)
    ("Enum" "\\`enum_decl\\'" nil nil))
  "Imenu settings for `alloy-ts-mode'.")

;; ---- Major mode ----

;;;###autoload
(define-derived-mode alloy-ts-mode prog-mode "Alloy"
  "Major mode for editing Alloy 6 specifications, powered by tree-sitter.

\\{alloy-ts-mode-map}"
  :group 'alloy-ts
  :syntax-table (let ((table (make-syntax-table)))
                  ;; Comments: // and --
                  (modify-syntax-entry ?/ ". 124" table)
                  (modify-syntax-entry ?* ". 23b" table)
                  (modify-syntax-entry ?- ". 12" table)
                  (modify-syntax-entry ?\n ">" table)
                  ;; Strings
                  (modify-syntax-entry ?\" "\"" table)
                  ;; Punctuation
                  (modify-syntax-entry ?_ "w" table)
                  (modify-syntax-entry ?$ "w" table)
                  table)

  (unless (treesit-ready-p 'alloy t)
    (error "Tree-sitter grammar for alloy is not available"))

  ;; Comments
  (setq-local comment-start "// ")
  (setq-local comment-end "")
  (setq-local comment-start-skip "\\(?://+\\|--+\\)\\s-*")

  ;; Tree-sitter
  (treesit-parser-create 'alloy)

  ;; Font-lock
  (setq-local treesit-font-lock-settings alloy-ts-mode--font-lock-settings)
  (setq-local treesit-font-lock-feature-list
              '((comment string)
                (keyword type function)
                (number builtin variable label module)
                (operator bracket delimiter)))

  ;; Indentation
  (setq-local treesit-simple-indent-rules alloy-ts-mode--indent-rules)

  ;; Navigation
  (setq-local treesit-defun-type-regexp alloy-ts-mode--defun-type-regexp)
  (setq-local treesit-defun-name-function
              (lambda (node)
                (let ((name-node (or (treesit-node-child-by-field-name node "name")
                                     (treesit-node-child-by-field-name node "names"))))
                  (when name-node
                    (treesit-node-text name-node t)))))

  ;; Imenu
  (setq-local treesit-simple-imenu-settings alloy-ts-mode--imenu-settings)

  (treesit-major-mode-setup))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.als\\'" . alloy-ts-mode))

(provide 'alloy-ts-mode)
;;; alloy-ts-mode.el ends here
