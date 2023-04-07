;; -*- Mode:emacs-lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;.H.S.;;
;;;
;; @file      mrscpi-mode.el
;; @author    Mitch Richling http://www.mitchr.me/
;; @brief     Major mode for the mrSCPI script language.@EOL
;; @keywords  SCPI
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
(define-generic-mode mrscpi-mode
  '()
  '()
  (let* ((mrscpi-trail-comment   "[[:blank:]]*\\(#.*\\|$\\)")
         (mrscpi-keywords-string '("eol" "ip_address" "log_file" "name" "out_file"
                                   "scpi_prefix" "url" "result_split_arg" "skip_if" "next_if" 
                                   "stop" "print" "goto" "lab" "var" "read_eot_sentinel"
                                   ))
         (mrscpi-keywords-bool   '("execute_on_cmd" "exit_on_error" "print_cmd" "good_std_eot"
                                   "echo" "result_chomp" "print_raw_result" "print_result"
                                   "result_macro_tmc" "result_macro_ascii" "result_macro_block"
                                   "result_macro_csv" "result_extract_tmc" "result_macro_debug"
                                   "result_last_word" "result_strip" "socket_close"
                                   "socket_close_write" "store_cmd" "store_raw_results"
                                   "store_results" ))
         (mrscpi-keywords-int    '("delay_after_complete" "delay_before_first_read" 
                                   "read_retry_delay" "net_port" "verbose"
                                   "read_timeout_first_byte" "read_timeout_next_byte"))
         (mrscpi-keywords-all (append '("cmd" "result_type" "result_split" "net_protocol"  "eval")
                                      mrscpi-keywords-string mrscpi-keywords-bool mrscpi-keywords-int))
         (mrscpi-splits          '("nil" ":char" ":string" ":whitespace"
                                   ":unpack" ":tmc" ":regex" ":lines" 
                                   ":csv" ":ssv"))
         (mrscpi-result-types    '(":string" ":float" ":int" ":bool" "nil"))
         (mrscpi-result-protos   '(":soip" ":raw" ":t3k"))
         (mrscpi-bool-values     '("true" "false" "t" "nil" "null" "0" "1" "on" "off"))
         )

    (list
     (cons (concat "^:"
                   (regexp-opt mrscpi-keywords-all t))                 '(0 font-lock-keyword-face t))            ;; Option keywords
     (cons (concat "^:result_type[[:blank:]]+"
                   (regexp-opt mrscpi-result-types 't)
                   mrscpi-trail-comment)                               '(1 font-lock-constant-face t))           ;; result_type
     (cons (concat "^:net_protocol[[:blank:]]+"
                   (regexp-opt mrscpi-result-protos 't)
                   mrscpi-trail-comment)                               '(1 font-lock-constant-face t))           ;; net_protocol
     (cons (concat "^:cmd" 
                   "[[:blank:]]+\\(.+\\)$")                            '(1 font-lock-function-name-face t))      ;; SCPI commands
     (cons (concat "^:eval"
                   "[[:blank:]]\\([a-z][A-Za-z0-9]*\\)=\\(.+\\)$")     '(1 font-lock-variable-name-face t))       ;; Variable set via eval
     (cons (concat "^:eval"
                   "[[:blank:]]\\([a-z][A-Za-z0-9]*\\)=\\(.+\\)$")     '(2 font-lock-preprocessor-face t))       ;; Code in an eval
     (cons (concat "^:result_split[[:blank:]]+"
                   (regexp-opt mrscpi-splits 't)
                   mrscpi-trail-comment)                               '(1 font-lock-constant-face t))           ;; result_split
     (cons (concat "^:"
                   (regexp-opt mrscpi-keywords-string t)
                   "[[:blank:]]+\\(.*\\)$")                            '(2 font-lock-string-face t))             ;; String values
     (cons (concat "^:"
                   (regexp-opt mrscpi-keywords-bool t)
                   "[[:blank:]]+"
                   (regexp-opt mrscpi-bool-values t)
                   mrscpi-trail-comment)                               '(2 font-lock-constant-face t))           ;; boolean values
     (cons (concat "^:"
                   (regexp-opt mrscpi-keywords-int t)
                   "[[:blank:]]+\\([0-9]+\\)"
                   mrscpi-trail-comment)                               '(2 font-lock-string-face t))             ;; integer values
     (cons "\\${[^}]+}"                                                '(0 font-lock-variable-name-face t))       ;; Variables
     (cons "\\${[^}:]+\\(:[^}]+\\)}"                                   '(1 font-lock-string-face t))             ;; Default value part of variable sub
     (cons "#.*$"                                                      '(0 font-lock-comment-delimiter-face t))  ;; comments
     ))
  '(".mrscpi\\'")
  nil
  "Major mode mrSCPI programs")
