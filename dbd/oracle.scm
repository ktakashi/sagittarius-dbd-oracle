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
	    (clos user)
	    (srfi :1 lists))
  (define-class <dbi-oracle-driver> (<dbi-driver>)
    ((env :init-keyword :env :reader oracle-env)))
  (define-class <dbi-oracle-connection> (<dbi-connection>)
    ((connection :init-keyword :connection :reader oracle-connection)))
  (define-class <dbi-oracle-query> (<dbi-query>)
    ((statement :init-keyword :statement :reader oracle-statement)
     (cursor    :init-value #f)))

  (define-class <dbi-oracle-table> (<dbi-table>)
    ((conn :init-keyword :conn)))
  (define-class <dbi-oracle-column> (<dbi-column>)
    ())


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
    (define (make-dsn name host port proto sid?)
      (construct-connection-string name host :port port :protocol proto
				   :use-sid sid?))
    (let ((env (oracle-env driver))
	  (database (get-option "database"))
	  (sid      (get-option "sid"))
	  (host     (get-option "host"))
	  (port     (get-option "port" 1521))
	  (protocol (get-option "protocol" 'TCP)))
      (unless (or database sid)
	(assertion-violation 'dbi-make-connection 
			     "database or sid option is required"))
      (when (and database sid)
	(assertion-violation 'dbi-make-connection 
			     "database and sid can't be specified both"))
      (make <dbi-oracle-connection>
	:connection (connect-database env
				      (if host 
					  (make-dsn (or database sid)
						    host port protocol
						    sid)
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

  ;; couldn't find using OCI so just query it
  (define-method dbi-tables ((conn <dbi-oracle-connection>)
			     :key (schema "") (table "") (types '(table view)))
    (define (construct-sql type schema table)
      (let1 base (format "select OWNER, ~a_name from all_~as where 1=1"
			 type type)
	(string-append base
		       (if (zero? (string-length schema))
			   ""
			   (string-append " and OWNER like ?"))
		       (if (zero? (string-length table))
			   ""
			   (format " and ~a_name like ?" type)))))
    (define (filter-params . params)
      (filter-map (lambda (s) (and (not (zero? (string-length s))) s)) params))
    (append-map! (lambda (type)
		   (let1 q (dbi-prepare conn 
					(construct-sql type schema table))
		     (apply dbi-execute! q (filter-params schema table))
		     (dbi-query-map q 
				    (lambda (r)
				      (make <dbi-oracle-table>
					:schema (~ r 0) :name (~ r 1)
					:type type :conn conn))))) types))
  
  ;; TODO 
  (define-method dbi-table-columns ((table <dbi-oracle-table>))
    (let1 q (dbi-execute-query-using-connection! 
	     (~ table 'conn) 
	     "select column_name, data_type, nullable from all_tab_cols \
              where table_name = ? and owner = ?"
	     (~ table 'name) (~ table 'schema))
      (dbi-query-map q (lambda (r)
			 ;; TODO check nullable
			 (make <dbi-oracle-column>
			   :name (~ r 0) :table table
			   :column-type (~ r 1)
			   :nullable? (string=? (~ r 2) "Y"))))))
  
)