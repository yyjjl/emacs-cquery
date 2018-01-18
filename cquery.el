;;; cquery.el --- cquery client for lsp-mode     -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Tobias Pisani

;; Author:  Tobias Pisani
;; Package-Version: 20180115.1
;; Version: 0.1
;; Homepage: https://github.com/jacobdufault/cquery
;; Package-Requires: ((emacs "25.1") (lsp-mode "3.4") (dash "0.13"))
;; Keywords: languages, lsp, c++

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and-or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.
;;
;;; Commentary:

;;
;; To enable, call (lsp-cquery-enable) in your c++-mode hook.
;;

;;; Code:

(require 'cquery-common)
(require 'cquery-semantic-highlighting)
(require 'cquery-codelens)
(require 'cquery-tree)

;; ---------------------------------------------------------------------
;;   Customization
;; ---------------------------------------------------------------------

(defcustom cquery-executable
  "cquery"
  "Path of the cquery executable."
  :type 'file
  :group 'cquery)

(defcustom cquery-resource-dir
  nil
  "The clang resource directory."
  :type '(choice
          (const :tag "Use default resource directory" :value nil)
          (directory :tag "Custom resource directory"))
  :group 'cquery)

(defcustom cquery-indexer-count
  0
  "Number of workers cquery will use to index each project.
When left at 0, cquery will computer this value automatically."
  :type 'number
  :group 'cquery)

(defcustom cquery-additional-arguments
  nil
  "Additional arguments passed to cquery."
  :type 'list
  :group 'cquery)

(defcustom cquery-extra-init-params
  nil
  "Additional initializationOptions passed to cquery."
  :type '(repeat string)
  :group 'cquery)

;; ---------------------------------------------------------------------
;;   Notification handlers
;; ---------------------------------------------------------------------

(defconst cquery--handlers
  '(("$cquery/setInactiveRegions" . (lambda (w p) (cquery--set-inactive-regions w p)))
    ("$cquery/publishSemanticHighlighting" . (lambda (w p) (cquery--publish-semantic-highlighting w p)))
    ("$cquery/progress" . (lambda (_w _p)))))

;; ---------------------------------------------------------------------
;;   Other cquery-specific methods
;; ---------------------------------------------------------------------

(defun cquery-xref-find-custom (method &optional display-action)
  "Find cquery-specific cross references.

Choices of METHOD include \"$cquery/base\", \"$cquery/callers\",
\"$cquery/derived\", \"$cquery/vars\".
Read document for all choices. DISPLAY-ACTION is passed to xref--show-xrefs."
  (lsp--cur-workspace-check)
  (let ((xrefs (lsp--locations-to-xref-items
                (lsp--send-request
                 (lsp--make-request method
                                    (lsp--text-document-position-params))))))
    (unless xrefs
      (user-error "No %s found" method))
    (xref--show-xrefs xrefs display-action)))

;; ---------------------------------------------------------------------
;;   CodeAction Commands
;; ---------------------------------------------------------------------

(defun cquery-select-codeaction ()
  "Show a list of codeactions using ivy, and pick one to apply."
  (interactive)
  (let ((name-func
         (lambda (action)
           (let ((edit (caadr (gethash "arguments" action))))
             (format "%s: %s" (lsp--position-to-point
                               (gethash "start" (gethash "range" edit)))
                     (gethash "title" action))))))
    (if (null lsp-code-actions)
        (message "No code actions avaliable")
      (ivy-read "Apply CodeAction: "
                (mapcar (lambda (action)
                          (funcall name-func action))
                        lsp-code-actions)
                :action (lambda (str)
                          (dolist (action lsp-code-actions)
                            (when (equal (funcall name-func action) str)
                              (cquery--execute-command (gethash "command" action) (gethash "arguments" action))
                              (lsp--text-document-code-action))))))))


;; ---------------------------------------------------------------------
;;  Register lsp client
;; ---------------------------------------------------------------------

(defun cquery--make-renderer (mode)
  `(lambda (str)
     (with-temp-buffer
       (delay-mode-hooks (,(intern (format "%s-mode" mode))))
       (insert str)
       (font-lock-ensure)
       (buffer-string))))

(defun cquery--initialize-client (client)
  (dolist (p cquery--handlers)
    (lsp-client-on-notification client (car p) (cdr p)))
  (lsp-provide-marked-string-renderer client "c" (cquery--make-renderer "c"))
  (lsp-provide-marked-string-renderer client "cpp" (cquery--make-renderer "c++"))
  (lsp-provide-marked-string-renderer client "objectivec" (cquery--make-renderer "objc")))

(defun cquery--get-init-params (workspace)
  (let ((json-false :json-false))
    `(:cacheDirectory ,(file-name-as-directory
                        (expand-file-name cquery-cache-dir (lsp--workspace-root workspace)))
                      ,@(when cquery-resource-dir
                          `(:resourceDirectory ,(expand-file-name cquery-resource-dir)))
                      :indexerCount ,cquery-indexer-count
                      :enableProgressReports ,json-false
                      ,@cquery-extra-init-params))) ; TODO: prog reports for modeline

(lsp-define-stdio-client
 lsp-cquery "cpp" #'cquery--get-root
 `(,cquery-executable "--language-server" ,@cquery-additional-arguments)
 :initialize #'cquery--initialize-client
 :extra-init-params #'cquery--get-init-params)

(provide 'cquery)
;;; cquery.el ends here
