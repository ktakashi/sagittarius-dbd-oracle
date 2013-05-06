;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; dbd/oracle.scm - DBD implementation for Oracle
;;;  
;;;   Copyright (c) 2013  Takashi Kato  <ktakashi@ymail.com>
;;;   
;;;   Redistribution and use in source and binary forms, with or without
;;;   modification, are permitted provided that the following conditions
;;;   are met:
;;;   
;;;   1. Redistributions of source code must retain the above copyright
;;;      notice, this list of conditions and the following disclaimer.
;;;  
;;;   2. Redistributions in binary form must reproduce the above copyright
;;;      notice, this list of conditions and the following disclaimer in the
;;;      documentation and/or other materials provided with the distribution.
;;;  
;;;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;;   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;;   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;  

(library (dbd oracle)
    (export make-oracle-driver)
    (import (rnrs)
	    (dbi)
	    (oracle)
	    (sagittarius)
	    (sagittarius control)
	    (sagittarius object)
	    (clos user))
  (define-class <dbi-oracle-driver> (<dbi-driver>)
    ((env :init-keyword :env :reader oracle-env)))
  (define-class <dbi-oracle-connection> (<dbi-connection>)
    ((connection :init-keyword :connection :reader oracle-connection)))
  (define-class <dbi-oracle-query> (<dbi-query>)
    ((statement :init-keyword :statement :reader oracle-statement)
     (cursor    :init-value #f)))

  (define-method dbi-make-connection ((driver <dbi-oracle-driver>)
				      options option-alist 
				      :key (username "") (password "")
				      (auto-commit #f)
				      :allow-other-keys)
    (define (get-option name :optional (default #f))
      (or (and-let* ((v (assoc name option-alist))) (cdr v))
	  default))
    (define (->boolean s)
      (if (string? s) (not (string=? s "false")) s))
    (define (make-dsn name host port proto)
      (construct-connection-string name host :port port :protocol proto))
    (let ((env (oracle-env driver))
	  (database (get-option "database"))
	  (host     (get-option "host"))
	  (port     (get-option "port" 1521))
	  (protocol (get-option "protocol" 'TCP)))
      (unless database
	(assertion-violation 'dbi-make-connection
			     "database option is required"))
      (make <dbi-oracle-connection>
	:connection (connect-database env
				      (if host 
					  (make-dsn database host port protocol)
					  database)
				      username password
				      :auto-commit auto-commit))))

  (define-method dbi-open? ((conn <dbi-oracle-connection>))
    (connection-open? (oracle-connection conn)))

  (define-method dbi-close ((conn <dbi-oracle-connection>))
    (disconnect-database (oracle-connection conn)))

  (define (convert-sql sql)
    ;; convert ? to numbers
    (call-with-string-output-port
     (lambda (out)
       (let1 counter 0
	 (string-for-each (lambda (c)
			    (cond ((char=? c #\?)
				   (set! counter (+ counter 1))
				   (display #\: out)
				   (display counter out))
				  (else (display c out)))) sql)))))

  (define-method dbi-prepare ((conn <dbi-oracle-connection>)
			      sql . args)
    (let1 stmt (create-statement (oracle-connection conn) (convert-sql sql))
      (unless (null? args)
	(let loop ((i 1) (args args))
	  (unless (null? args)
	    (bind-parameter stmt i (car args))
	    (loop (+ i 1) (cdr args)))))
      (make <dbi-oracle-query> :statement stmt)))

  (define-method dbi-open? ((q <dbi-oracle-query>))
    (statement-open? (~ q 'statement)))

  (define-method dbi-close ((q <dbi-oracle-query>))
    (release-statement (~ q 'statement)))

  (define-method dbi-commit! ((conn <dbi-oracle-connection>))
    (commit (oracle-connection conn)))

  (define-method dbi-rollback! ((conn <dbi-oracle-connection>))
    (rollback (oracle-connection conn)))

  (define-method dbi-bind-parameter! ((query <dbi-oracle-query>)
				      (index <integer>) value)
    (let ((stmt (oracle-statement query)))
      (bind-parameter stmt index value)))

  (define-method dbi-bind-parameter! ((query <dbi-oracle-query>)
				      (index <keyword>) value)
    (let ((stmt (oracle-statement query)))
      (bind-parameter stmt index value)))

  (define-method dbi-execute! ((query <dbi-oracle-query>) . args)
    (let ((stmt (oracle-statement query)))
      (unless (null? args)
	;; bind
	(do ((i 1 (+ i 1))
	     (params args (cdr params)))
	    ((null? params) #t)
	  (bind-parameter stmt i (car params))))
      (let1 cursor (execute-query stmt)
	(if (integer? cursor)
	    cursor
	    (begin (set! (~ query 'cursor) cursor) -1)))))

  (define-method dbi-fetch! ((query <dbi-oracle-query>))
    (let1 cursor (~ query 'cursor)
      (if cursor
	  (fetch-row cursor)
	  (error 'dbi-fetch! "query is not SELECT"))))

  (define-method dbi-commit! ((query <dbi-oracle-query>))
    (let ((stmt (oracle-statement query)))
      (commit (~ stmt 'connection))))

  (define-method dbi-rollback! ((query <dbi-oracle-query>))
    (let ((stmt (oracle-statement query)))
      (rollback (~ stmt 'connection))))

  (define-method dbi-columns ((query <dbi-oracle-query>))
    (let ((cursor (~ query 'cursor)))
      (if cursor
	  (query-columns cursor)
	  ;; should we raise an error?
	  #())))

  (define (make-oracle-driver)
    (make <dbi-oracle-driver> :env (create-oracle-env)))
)