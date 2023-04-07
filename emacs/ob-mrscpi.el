;; -*- Mode:emacs-lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;.H.S.;;
;;;
;; @file      ob-mrscpi.el
;; @author    Mitch Richling http://www.mitchr.me/
;; @brief     org-babel functions for mrSCPI evaluation.@EOL
;; @keywords  org-mode
;; @std       Emacs Lisp
;; @copyright 
;;  @parblock
;;  Copyright (c) 2023, Mitchell Jay Richling <http://www.mitchr.me/> All rights reserved.
;;
;;  Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:
;;
;;  1. Redistributions of source code must retain the above copyright notice, this list of conditions, and the following disclaimer.
;;
;;  2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions, and the following disclaimer in the documentation
;;     and/or other materials provided with the distribution.
;;
;;  3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products derived from this software
;;     without specific prior written permission.
;;
;;  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;;  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
;;  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
;;  OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;;  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
;;  DAMAGE.
;;  @endparblock
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;.H.E.;;

;;##############################################################################################################################################################
(require 'cl-lib)
(require 'ob)
(require 'ob-ref)
(require 'ob-comint)
(require 'ob-eval)

;; Set the path to mrSCPI.rb
(setq MJR-mrscpi-path "~/bin/mrSCPI.rb")

;; optionally define a file extension for this language
(add-to-list 'org-babel-tangle-lang-exts '("mrscpi" . "mrscpi"))

;; optionally declare default header arguments for this language
(defvar org-babel-default-header-args:mrscpi '())

;; Adds :var statements for header variables.  Called by ORG-BABEL-EXECUTE:MRSCPI
(defun org-babel-expand-body:mrscpi (body params &optional processed-params)
  "Expand BODY according to PARAMS, return the expanded body."
  (let ((processed-params (or processed-params (org-babel-process-params params))))
    (concat (mapconcat (lambda (x)
                         (if (eq (car x) :var)
                             (format ":var %s=%s\n" (car (cdr x)) (cdr (cdr x)))))
                       processed-params
                       "")
            "\n" 
            body
            "\n")))

;; Called to evaluate a code block.
(defun org-babel-execute:mrscpi (body params)
  "Execute a block of Mrscpi code with org-babel. This function is called by `org-babel-execute-src-block'"
  (message "executing mrSCPI source code block")
  (let* ((processed-params (org-babel-process-params params))
         (result-type      (nth 3 processed-params))          ;; either OUTPUT or VALUE
         (in-file          (org-babel-temp-file "mrscpi-"))
         (da-cmd           (concat "ruby " 
                                   MJR-mrscpi-path
                                   " --log_file STDOUT --exit_0 true -f STDIN"))
         (raw-result       (org-babel-eval da-cmd (org-babel-expand-body:mrscpi body params processed-params))))
    (if 't ;; Should do something with raw-result here...
        raw-result)))

;; ;; Assign any variables in params in the context of the session environment.  We don't support sessions, so we do nothing
;; (defun org-babel-prep-session:mrscpi (session params)
;;   "Prepare SESSION according to the header arguments specified in PARAMS.")

;; (defun org-babel-mrscpi-var-to-mrscpi (var)
;;   "Convert an elisp var into a string of mrscpi source code
;; specifying a var of the same value."
;;   (format "%S" var))

;; (defun org-babel-mrscpi-table-or-string (results)
;;   "If the results look like a table, then convert them into an
;; Emacs-lisp table, otherwise return the results as a string."
;;   (format "%S" results))

(provide 'ob-mrscpi)
;;; ob-mrscpi.el ends here

