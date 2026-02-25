;;; alloy-lsp.el --- Alloy LSP + runner UI (VS Code-like) via lsp-mode/stdin-stdout -*- lexical-binding: t; -*-

;; Requires: lsp-mode, json, cl-lib, button
;; Put this file on your load-path, then:
;;   (require 'alloy-lsp)
;;   (add-hook 'alloy-mode-hook #'alloy-lsp-ensure)
;;
;; Configure the server command:
;;   (setq alloy-lsp-server-command
;;         '("java" "-jar" "/path/to/org.alloytools.alloy.dist.jar" "ls"))
;;
;; NOTE: The server must run in stdio mode (LSP over stdin/stdout). Depending on
;; your jar/CLI, you may need to adjust args so that it DOES NOT take a single
;; numeric port argument.

(require 'cl-lib)
(require 'lsp-mode)
(require 'json)
(require 'button)

(defgroup alloy-lsp nil
  "Alloy LSP + execution UI."
  :group 'tools)

(defvar alloy-lsp--directory
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory where alloy-lsp.el is installed.")

(defcustom alloy-lsp-server-command
  `("java" "-jar"
    ,(expand-file-name "org.alloytools.alloy.dist.jar" alloy-lsp--directory)
    "ls")
  "Command list to start the Alloy language server in stdio mode.
This must start the server so it speaks LSP over stdin/stdout."
  :type '(repeat string))

(defcustom alloy-lsp-output-buffer-name "*Alloy*"
  "Buffer name for Alloy execution output."
  :type 'string)

(defcustom alloy-lsp-pop-output-buffer t
  "If non-nil, pop the output buffer when messages arrive."
  :type 'boolean)

(defcustom alloy-lsp-output-window-side 'right
  "Side to show output buffer (if popping)."
  :type '(choice (const right) (const left) (const bottom) (const top)))

;; ---- Internal state

(defvar alloy-lsp--latest-instance-link nil)
(defvar alloy-lsp--last-replace-node nil)
(defvar alloy-lsp--workspace nil
  "Saved LSP workspace for sending notifications from the output buffer.")

;; messageType enum (from AlloyPanel.html comment / server enum):
;; 0 RunStarted, 1 RunInProgress, 2 RunResult, 3 RunCompleted, 4 Warning
(defun alloy-lsp--message-face (msgtype bold)
  (cond
   ((= msgtype 4) 'error)
   (bold 'bold)
   (t 'default)))

(defun alloy-lsp--link-face (msgtype bold)
  "Return a face for clickable link output items."
  (cond
   ((= msgtype 4) '(link error))
   (bold '(link bold))
   (t 'link)))

(defun alloy-lsp--ensure-output-buffer ()
  (let ((buf (get-buffer-create alloy-lsp-output-buffer-name)))
    (with-current-buffer buf
      (unless (derived-mode-p 'alloy-lsp-output-mode)
        (alloy-lsp-output-mode)))
    buf))

(defun alloy-lsp--maybe-display-output-buffer ()
  (when alloy-lsp-pop-output-buffer
    (let ((buf (alloy-lsp--ensure-output-buffer)))
      (display-buffer-in-side-window
       buf `((side . ,alloy-lsp-output-window-side)
             (window-width . 0.42)
             (window-height . 0.33))))))

(defun alloy-lsp--insert-hr ()
  (let ((inhibit-read-only t))
    (goto-char (point-max))
    (insert (propertize "\n" 'face 'default))
    (insert (propertize (make-string 60 ?-) 'face 'shadow))
    (insert "\n")))

(defun alloy-lsp--insert-text (text face &optional replace-last)
  (let ((inhibit-read-only t))
    (goto-char (point-max))
    ;; emulate "replaceLast": remove the most recent inserted node if asked
    (when (and replace-last alloy-lsp--last-replace-node
               (marker-buffer alloy-lsp--last-replace-node))
      (let ((m alloy-lsp--last-replace-node))
        (with-current-buffer (marker-buffer m)
          (save-excursion
            (let ((inhibit-read-only t))
              (goto-char m)
              (delete-region m (point-max)))))))
    (let ((start (point)))
      (insert (propertize (replace-regexp-in-string "\r\n" "\n" (or text "")) 'face face))
      ;; Track last insertion for replaceLast updates:
      (setq alloy-lsp--last-replace-node (copy-marker start t))
      (goto-char (point-max)))))

(defun alloy-lsp--insert-link (label link face &optional replace-last)
  (let ((inhibit-read-only t))
    (goto-char (point-max))
    (when (and replace-last alloy-lsp--last-replace-node
               (marker-buffer alloy-lsp--last-replace-node))
      (let ((m alloy-lsp--last-replace-node))
        (with-current-buffer (marker-buffer m)
          (save-excursion
            (let ((inhibit-read-only t))
              (goto-char m)
              (delete-region m (point-max)))))))
    (let ((start (point)))
      (insert-text-button
       (or label link)
       'face face
       'follow-link t
       'help-echo link
       'action (lambda (_btn)
                 (setq alloy-lsp--latest-instance-link link)
                 (alloy-lsp-open-model link)))
      (insert "\n")
      (setq alloy-lsp--last-replace-node (copy-marker start t))
      (goto-char (point-max)))))

(defun alloy-lsp--notify (method params)
  "Send a custom JSON-RPC notification METHOD with PARAMS to the server.
Uses the saved workspace when called from a non-LSP buffer (e.g. *Alloy*)."
  (if (bound-and-true-p lsp-mode)
      (lsp-notify method params)
    (when alloy-lsp--workspace
      (with-lsp-workspace alloy-lsp--workspace
        (lsp-notify method params)))))

(defun alloy-lsp--uri ()
  (or (and (buffer-file-name) (lsp--path-to-uri (buffer-file-name)))
      (user-error "Buffer is not visiting a file")))

;; ---- WebView-equivalent output buffer

(define-derived-mode alloy-lsp-output-mode special-mode "Alloy"
  "Major mode for Alloy execution output."
  (setq-local truncate-lines t)
  (setq-local buffer-read-only t))

;; ---- Notification handlers from server

(defun alloy-lsp--on-show-execution-output (workspace params)
  "Handle alloy/showExecutionOutput notifications."
  (setq alloy-lsp--workspace workspace)
  (let* ((msg (or (gethash "message" params) ""))
         (msgtype (or (gethash "messageType" params) 1))
         (bold (eq t (gethash "bold" params)))
         (link (gethash "link" params))
         (replace-last (eq t (gethash "replaceLast" params)))
         (line-break (let ((v (gethash "lineBreak" params :missing)))
                       (if (eq v :missing) t v))) ; default true per server
         (buf (alloy-lsp--ensure-output-buffer)))
    (setq alloy-lsp--latest-instance-link (or link alloy-lsp--latest-instance-link))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (cond
         (link
          (alloy-lsp--insert-link (and (stringp msg) (string-trim msg))
                                  link
                                  (alloy-lsp--link-face msgtype bold)
                                  replace-last))
         (t
          (let ((text (if (and line-break (not (string-suffix-p "\n" msg)))
                          (concat msg "\n")
                        msg)))
            (alloy-lsp--insert-text text
                                    (alloy-lsp--message-face msgtype bold)
                                    replace-last))))
        (when (= msgtype 3) ;; RunCompleted
          (alloy-lsp--insert-hr))))
    (alloy-lsp--maybe-display-output-buffer)))

(defun alloy-lsp--on-commands-list-result (workspace params)
  "Handle alloy/commandsListResult notifications."
  (setq alloy-lsp--workspace workspace)
  ;; Expected shape: { commands: [ {title: string, command: {command: string, arguments: [...]}} ... ] }
  (let* ((cmds (gethash "commands" params))
         (items '()))
    (when (and (vectorp cmds) (> (length cmds) 0))
      (dotimes (i (length cmds))
        (let* ((it (aref cmds i))
               (title (gethash "title" it))
               (cmd (gethash "command" it))
               (cmd-name (and cmd (gethash "command" cmd)))
               (args (and cmd (gethash "arguments" cmd))))
          (push (list :title title :cmd cmd-name :args args) items))))
    (setq items (nreverse items))
    (if (null items)
        (message "Alloy: no commands returned.")
      (let* ((choice (completing-read "Alloy command: " (mapcar (lambda (x) (plist-get x :title)) items) nil t))
             (picked (cl-find-if (lambda (x) (string= (plist-get x :title) choice)) items)))
        (when picked
          (pcase (plist-get picked :cmd)
            ("ExecuteAlloyCommand"
             (let ((a (plist-get picked :args)))
               ;; args is a JSON array decoded as vector/list; normalize to list
               (setq a (if (vectorp a) (append a nil) a))
               (apply #'alloy-lsp-execute-alloy-command a)))
            (_
             ;; Fall back: if server provides other client commands someday, attempt to forward
             ;; them as custom notifications with the same name.
             (alloy-lsp--notify (plist-get picked :cmd) (plist-get picked :args)))))))))

;; ---- Action handler for CodeLens clicks

(lsp-defun alloy-lsp--handle-execute-command-action ((&Command :arguments?))
  "Handle ExecuteAlloyCommand from CodeLens click.
Intercepts lsp-mode's default `workspace/executeCommand' dispatch and
sends the custom notification that the Alloy server expects."
  (lsp-notify "ExecuteAlloyCommand" arguments?))

;; ---- LSP client registration

(defvar alloy-lsp--client
  (make-lsp-client
   :new-connection (lsp-stdio-connection (lambda () alloy-lsp-server-command))
   :major-modes '(alloy-mode markdown-mode)
   :server-id 'alloy-lsp
   :notification-handlers (ht
                           ("alloy/showExecutionOutput" #'alloy-lsp--on-show-execution-output)
                           ("alloy/commandsListResult"  #'alloy-lsp--on-commands-list-result))
   :action-handlers (ht
                     ("ExecuteAlloyCommand" #'alloy-lsp--handle-execute-command-action))
   :initialized-fn (lambda (workspace)
                     (setq alloy-lsp--workspace workspace))
   :priority 1))

(lsp-register-client alloy-lsp--client)

;;;###autoload
(defun alloy-lsp-ensure ()
  "Ensure lsp-mode is running for the current buffer."
  (interactive)
  ;; Disable DocumentLinks — the server returns command: URIs meant for
  ;; VS Code which lsp-mode cannot handle.  We use CodeLens instead.
  (setq-local lsp-enable-links nil)
  (unless (bound-and-true-p lsp-mode)
    (lsp-deferred))
  (alloy-lsp-mode 1))

;; ---- Command discovery (CodeLens-based) like VS Code extension

(defun alloy-lsp--code-lenses ()
  "Request CodeLens from the server and return a list of lenses."
  ;; lsp-request is synchronous; fine for interactive commands
  (lsp-request "textDocument/codeLens"
               `(:textDocument (:uri ,(alloy-lsp--uri)))))

(defun alloy-lsp--lens-at-point (lenses)
  "Find the first lens whose range contains point."
  (let ((pos (lsp--cur-position)))
    (cl-find-if
     (lambda (lens)
       (let* ((range (gethash "range" lens))
              (start (gethash "start" range))
              (end (gethash "end" range))
              (sline (gethash "line" start)) (schar (gethash "character" start))
              (eline (gethash "line" end))   (echar (gethash "character" end))
              (pline (plist-get pos :line))  (pchar (plist-get pos :character)))
         (and (or (> pline sline) (and (= pline sline) (>= pchar schar)))
              (or (< pline eline) (and (= pline eline) (<= pchar echar))))))
     lenses)))

(defun alloy-lsp--lens-command-args (lens)
  "Extract ExecuteAlloyCommand args from a lens command."
  (let* ((cmd (gethash "command" lens))
         (cmd-name (and cmd (gethash "command" cmd)))
         (args (and cmd (gethash "arguments" cmd))))
    (unless (and (stringp cmd-name) (string= cmd-name "ExecuteAlloyCommand"))
      (user-error "Lens command is not ExecuteAlloyCommand (got %S)" cmd-name))
    (setq args (if (vectorp args) (append args nil) args))
    args))

;; ---- Public interactive commands (VS Code-like)

(defun alloy-lsp-execute-alloy-command (uri ind line char)
  "Low-level: send ExecuteAlloyCommand to server with given args."
  (interactive)
  (alloy-lsp--notify "ExecuteAlloyCommand" (vector uri ind line char)))

;;;###autoload
(defun alloy-lsp-execute-command-under-cursor ()
  "Execute the Alloy command whose CodeLens range contains point."
  (interactive)
  (let* ((lenses (alloy-lsp--code-lenses))
         (lens (and (vectorp lenses) (alloy-lsp--lens-at-point (append lenses nil)))))
    (unless lens
      (user-error "Cursor is not inside an Alloy command"))
    (apply #'alloy-lsp-execute-alloy-command (alloy-lsp--lens-command-args lens))))

;;;###autoload
(defun alloy-lsp-execute-all-commands ()
  "Execute all Alloy commands in the current file."
  (interactive)
  (alloy-lsp--notify "ExecuteAlloyCommand" (vector (alloy-lsp--uri) -1 0 0)))

;;;###autoload
(defun alloy-lsp-stop-execution ()
  "Stop current Alloy execution."
  (interactive)
  (alloy-lsp--notify "StopExecution" nil))

;;;###autoload
(defun alloy-lsp-list-commands ()
  "Ask server to list runnable Alloy commands (shows a chooser)."
  (interactive)
  (alloy-lsp--notify "ListAlloyCommands" (alloy-lsp--uri)))

;;;###autoload
(defun alloy-lsp-open-model (&optional link)
  "Open a model/instance by LINK (or prompt / use latest)."
  (interactive)
  (let ((l (or link
               alloy-lsp--latest-instance-link
               (read-string "Alloy link: "))))
    (setq alloy-lsp--latest-instance-link l)
    (alloy-lsp--notify "OpenModel" l)))

;;;###autoload
(defun alloy-lsp-open-latest-instance ()
  "Open the latest instance link received from the server."
  (interactive)
  (unless alloy-lsp--latest-instance-link
    (user-error "No Alloy instances generated yet"))
  (alloy-lsp-open-model alloy-lsp--latest-instance-link))

;; ---- Minor mode + keybindings (similar spirit to VS Code ones)

(defvar alloy-lsp-mode-map
  (let ((m (make-sparse-keymap)))
    ;; Feel free to change these. They roughly mirror the VS Code extension intent.
    (define-key m (kbd "C-c a e") #'alloy-lsp-execute-command-under-cursor)
    (define-key m (kbd "C-c a a") #'alloy-lsp-execute-all-commands)
    (define-key m (kbd "C-c a c") #'alloy-lsp-list-commands)
    (define-key m (kbd "C-c a l") #'alloy-lsp-open-latest-instance)
    (define-key m (kbd "C-c a s") #'alloy-lsp-stop-execution)
    m)
  "Keymap for `alloy-lsp-mode'.")

;;;###autoload
(define-minor-mode alloy-lsp-mode
  "Minor mode providing VS Code-like Alloy runner commands."
  :lighter " Alloy"
  :keymap alloy-lsp-mode-map)

(provide 'alloy-lsp)
;;; alloy-lsp.el ends here
