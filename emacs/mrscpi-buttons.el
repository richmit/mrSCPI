;; -*- Mode:emacs-lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;.H.S.;;
;;;
;; @file      mrscpi-buttons.el
;; @author    Mitch Richling http://www.mitchr.me/
;; @date      2023-04-04
;; @brief     Create buttons to run mrSCPI code blocks in an org-mode document.@EOL
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
;; @filedetails
;;
;;  I have a big org-mode document with test equipment notes.  In the notes for each section are little mrSCPI scripts to do things like:
;;
;;       - run *IDN?
;;       - Reset a device to a default configuration
;;       - Configure a device to some set of set of saved conditions
;;       - Perform common tasks
;;         Example 1: Have my power supply report voltage & current on all channels
;;         Example 2: Have my scope measure phase between ch1 & ch2 as well as Vpp on both channels
;;         Example 3: Have my DMM do a very accurate DC voltage measurement
;;         Example 4: Have my DMM measure frequency and AC voltage
;;
;;  This function will find all those little mrSCPI scripts (the ones with names like "btn-"), and will make buttons to run them.  When I'm at the bench, I
;;  normally have this button buffer up with my laboratory notebook in Emacs all the time.  It's super handy.
;;
;;  I didn't include this function in the other Emacs code files because I'm not sure if it is something most users really want.  So I figured I would just
;;  put in another file, and give people the option of loading it up if they were interested in using it.
;;
;;  I load this up right after the org-mode configuration stuff in my dot files.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;.H.E.;;

;;##############################################################################################################################################################
(defun MJR-mrSCPI-buttons ()
  "Create a buffer with a buttons to execute mrscpi code blocks in the current org document with names starting with but-"
  (interactive)
  (if (not (equal major-mode 'org-mode))
      (error "MJR-mrSCPI-buttons: Must be called in active org-mode buffer!")
      (let ((da-org-buffer (current-buffer))
            (da-but-buffer (get-buffer-create (concat "*mrSCPI-buttons-" (buffer-name) "*")))
            (da-but-list   nil))
        (with-current-buffer da-org-buffer
          (org-element-map
              (org-element-parse-buffer 'greater-element)
              'src-block
            (lambda (elt)
              (let ((blk-name  (org-element-property :name elt))             ;; Block Name
                    (blk-begin (org-element-property :begin elt))            ;; Position at start of #+NAME block
                    (blk-value (org-element-property :value elt))            ;; This is the code!
                    (blk-end   (org-element-property :end elt))              ;; Position of end of block
                    (blk-lang  (org-element-property :language elt))         ;; Language
                    (blk-parm  (org-element-property :parameters elt))       ;; Things like vars, etc...
                    (blk-post  (org-element-property :post-affiliated elt))) ;; Position of actual block begin
                (if (and (not (null blk-name))
                         (not (string-empty-p blk-name))
                         (string-match "^btn-" blk-name)
                         (not (null blk-lang))
                         (string-equal "mrscpi" blk-lang))
                    (push (list blk-name blk-post blk-end (buffer-substring-no-properties blk-post blk-end)) da-but-list))))))
        (if (null da-but-list)
            (error "MJR-mrSCPI-buttons: No button code blocks found!")
            (let ((cur-heading nil)
                  (cur-line-len 0)
                  (max-line-len 120))
              (with-current-buffer da-but-buffer
                (read-only-mode -1)
                (erase-buffer)
                (insert (format "mrSCPI Buttons from %s.   q: kill this buffer, s: switch to %s, r: refresh buttons.\n"
                                (buffer-name da-org-buffer) 
                                (buffer-name da-org-buffer))))
              (dolist (but (reverse da-but-list))
                (let* ((but-name  (substring (nth 0 but) 4))
                       (but-begin (nth 1 but))
                       (but-end   (nth 2 but))
                       (but-code  (nth 3 but))
                       (but-pfx   (if (string-match "-" but-name) (replace-regexp-in-string "-.*$" "" but-name) ""))
                       (but-lab   (if (string-match "-" but-name) (replace-regexp-in-string "^[^-]*-" "" but-name) but-name)))
                  (with-current-buffer da-but-buffer
                    (if (not (equal cur-heading but-pfx))
                        (progn (if (> cur-line-len 0) (insert "\n"))
                               (setq cur-heading  but-pfx
                                     cur-line-len 0)
                               (insert (concat "\n" cur-heading "\n"))))
                    (if (and (> cur-line-len 0) (> (+ cur-line-len (length but-lab) 3) max-line-len))
                        (progn (setq cur-line-len 0)
                               (insert "\n")))
                    (setq cur-line-len (+ cur-line-len (length but-lab) 3))
                    (insert-button (format " %s " but-lab)
                                   'action (eval (macroexpand `(lambda (button) 
                                                                 (if (not (buffer-live-p ,da-org-buffer))
                                                                     (error "mrSCPI Button: Original org-mode buffer not found!")
                                                                     (with-current-buffer ,da-org-buffer
                                                                       (if (not (string-equal ,but-code (buffer-substring-no-properties ,but-begin ,but-end)))
                                                                           (error "mrSCPI Button: Button buffer is out of sync with original org-mode buffer! Use 'r' to refresh.")
                                                                           (progn  (message "mrSCPI Button: %s" ,but-name)
                                                                                   (shell-command-on-region ,but-begin
                                                                                                            ,but-end
                                                                                                            (concat "ruby " 
                                                                                                                    MJR-mrscpi-path
                                                                                                                    " --log_file STDOUT --exit_0 true -f STDIN"))))))))))
                    (insert " "))))
              (with-current-buffer da-but-buffer
                (read-only-mode 1)
                (setq truncate-lines 't)
                (local-set-key (kbd "q") 'kill-current-buffer)
                (local-set-key (kbd "s") (eval (macroexpand `(lambda () (interactive) (switch-to-buffer ,da-org-buffer)))))
                (local-set-key (kbd "r") (eval (macroexpand `(lambda () (interactive) (switch-to-buffer ,da-org-buffer) (MJR-mrSCPI-buttons))))))
              (switch-to-buffer da-but-buffer))))))
