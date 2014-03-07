;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; oracle.scm - OCI binding library
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

;; oracle driver using oci
(library (oracle)
    (export create-oracle-env
	    release-oracle-env
	    construct-connection-string
	    connect-database
	    disconnect-database
	    create-statement
	    release-statement
	    execute-query
	    bind-parameter
	    query-columns
	    row-count
	    commit rollback
	    fetch-row
	    ;; diagnosis
	    connection-open?
	    statement-open?)
    (import (rnrs)
	    (rnrs mutable-strings)
	    (oracle oci)
	    (oracle constant)
	    (sagittarius)
	    (sagittarius control)
	    (sagittarius object)
	    (sagittarius ffi)
	    (sagittarius time)		; to use <date> and <time>
	    (clos user)
	    (clos core)
	    (binary pack)
	    (srfi :26 cut))

  ;; context holder
  (define-class <oracle-env> ()
    ((envhp :init-keyword :envhp :reader envhp)
     (errhp :init-keyword :errhp :reader errhp)))

  (define-class <oracle-connection> ()
    ((name :init-keyword :name)
     (env  :init-keyword :env)
     (svchp :init-keyword :svchp :reader svchp)
     (srvhp :init-keyword :srvhp :reader srvhp)
     (user :init-keyword :user)
     (session :init-keyword :session)
     (data-source-name  :init-keyword :dsn)
     (auto-commit :init-keyword :auto-commit)))

  (define-class <oracle-statement> ()
    ((connection  :init-keyword :connection)
     (stmthp      :init-keyword :stmthp)
     (type        :init-keyword :type)
     ;; keep binds to prevent GC
     (binds       :init-value '())))

  (define-class <oracle-cursor> ()
    ((statement :init-keyword :statement)
     ;; vector of column information
     (columns  :init-keyword :columns)
     (offset   :init-value 0)))

  (define (check-error who errhp status :optional (ignores '()))
    (if (= status +oci-success+)
	(values +oci-success+ "no error")
	(let ((bv (allocate-pointer 512))
	      (ecode (empty-pointer)))
	  (oci-error-get errhp 1 null-pointer (address ecode)
			 bv 512 +oci-htype-error+)
	  (let1 code (pointer->integer ecode)
	    (cond ((and (negative? status) (not (memv code ignores)))
		   (error who (pointer->string bv) 
			  `((status ,status) (code ,code))))
		  (else 
		   ;; return in case
		   (values code (pointer->string bv))))))))
  
  (define-syntax with-check-error
    (syntax-rules ()
      ((_ (who errhp r)) r)
      ((_ (who errhp r) expr1 exprs ...)
       (let ((errhp errhp)
	     (status expr1))
	 (check-error 'who errhp status)
	 (with-check-error (who errhp status) exprs ...)))
      ;; entry point
      ((_ (who errhp) expr1 exprs ...)
       (with-check-error (who errhp 0) expr1 exprs ...))))

  (define (create-oracle-env)
    (let ((envhp (empty-pointer))
	  (errhp (empty-pointer))
	  )
      ;; TODO check oci7 (who uses it?)
      ;; TODO error check
      (unless (= (oci-env-create (address envhp) +oci-default+
				 null-pointer null-pointer
				 null-pointer null-pointer
				 0 null-pointer)
		 +oci-success+)
	(error 'create-oracle-env "failed to create an oracle environment"))
      (with-check-error (create-oracle-env envhp)
	(oci-handle-alloc envhp (address errhp)
			  +oci-htype-error+ 0 null-pointer))
      (make <oracle-env> :envhp envhp :errhp errhp)))

  (define (release-oracle-env env)
    (oci-handle-free (errhp env) +oci-htype-error+)
    (oci-handle-free (envhp env) +oci-htype-env+))

  (define (construct-connection-string name host :key (port 1521)
				       (protocol 'TCP)
				       (use-sid #f))
    (format "~a" 
	    `(DESCRIPTION= (ADDRESS= (PROTOCOL= ,protocol)
				     (HOST= ,host)
				     (PORT= ,port))
			   (CONNECT_DATA= (,(if use-sid 
						'SID= 
						'SERVICE_NAME=) ,name)))))

  (define (connect-database env dsn user password :key (auto-commit #f))
    (let ((envhp (envhp env))
	  (errhp (errhp env))
	  (svchp (empty-pointer))
	  (srvhp (empty-pointer))
	  (authp (empty-pointer)))
      (with-check-error (connect-database (~ env 'envhp))
	(oci-handle-alloc envhp (address svchp) 
			  +oci-htype-svcctx+ 0 null-pointer)
	(oci-handle-alloc envhp (address srvhp) 
			  +oci-htype-server+ 0 null-pointer)
  	(oci-handle-alloc (~ env 'envhp) (address authp) +oci-htype-session+ 0
			  null-pointer))
      (with-check-error (connect-database errhp)
	(oci-server-attach srvhp errhp dsn (string-length dsn) +oci-default+)
	(oci-attr-set authp +oci-htype-session+ user (string-length user)
		      +oci-attr-username+ errhp)
	(oci-attr-set authp +oci-htype-session+
		      password (string-length password)
		      +oci-attr-password+ errhp)
	(oci-attr-set svchp +oci-htype-svcctx+ srvhp 0 +oci-attr-server+ errhp)
	(oci-session-begin svchp errhp authp +oci-cred-rdbms+ +oci-default+)
	(oci-attr-set svchp +oci-htype-svcctx+ authp 0 +oci-attr-session+ errhp)
	(oci-attr-set svchp +oci-htype-svcctx+ authp 0 +oci-attr-session+ errhp)
	)
      ;; TODO alter data format
      (make <oracle-connection>
	:name dsn :env env :dsn dsn
	:svchp svchp :srvhp srvhp
	:user user :session authp
	:auto-commit auto-commit)))

  (define (disconnect-database conn :optional (release-env #f))
    (let ((errhp (~ conn 'env 'errhp))
	  (svchp (~ conn 'svchp))
	  (srvhp (~ conn 'srvhp))
	  (authp (~ conn 'session)))
      (oci-session-end svchp errhp authp +oci-default+)
      (oci-server-detach srvhp errhp +oci-default+)
      (oci-handle-free authp +oci-htype-session+)
      (oci-handle-free srvhp +oci-htype-server+)
      (oci-handle-free svchp +oci-htype-svcctx+))
    (when release-env
      (release-oracle-env (~ conn 'env))))

  (define (connection-open? conn)
    (let1 status (empty-pointer)
      (oci-attr-get (~ conn 'srvhp) +oci-htype-server+
		    (address status) null-pointer +oci-attr-server-status+
		    (~ conn 'env 'errhp))
      (= (pointer->integer status) +oci-server-normal+)))

  (define (create-statement conn sql)
    (let ((stmthp (empty-pointer))
	  (type   (empty-pointer))
	  (envhp (~ conn 'env 'envhp))
	  (errhp (~ conn 'env 'errhp))
	  (svchp (~ conn 'svchp)))
      (with-check-error (create-statement errhp)
	(oci-handle-alloc envhp (address stmthp)
			  +oci-htype-stmt+ 0 null-pointer)
	(oci-stmt-prepare stmthp errhp
			  sql (string-length sql)
			  +oci-ntv-syntax+ +oci-default+)
	(oci-attr-get stmthp +oci-htype-stmt+ (address type)
		      null-pointer +oci-attr-stmt-type+ errhp))
      (register-ffi-finalizer stmthp 
			      (lambda (stmthp)
				(oci-handle-free stmthp +oci-htype-stmt+)))
      (make <oracle-statement> :connection conn :stmthp stmthp
	    :type (pointer->integer type))))

  (define (release-statement stmt)
    (oci-handle-free (~ stmt 'stmthp) +oci-htype-stmt+)
    (unregister-ffi-finalizer (~ stmt 'stmthp)))

  (define (statement-open? stmt)
    (let1 env (empty-pointer)
      (oci-attr-get (~ stmt 'stmthp) +oci-htype-stmt+
		    (address env) null-pointer +oci-attr-env+
		    (~ stmt 'connection 'env 'errhp))
      (not (null-pointer? env))))

  (define-constant +*lobs+ `(,+sqlt-blob+ ,+sqlt-clob+))

  (define (execute-query stmt . args)

    (define (write-lob info svchp errhp)
      (define (buffer&length port)
	(if (binary-port? port)
	    (let1 bv (get-bytevector-n port 1024)
	      (if (eof-object? bv)
		  (values #vu8() 0)
		  (values bv (bytevector-length bv))))
	    (let1 str (get-string-n port 1024)
	      (if (eof-object? str)
		  (values #vu8() 0)
		  (let1 bv (string->utf8 str)
		    (values bv (bytevector-length bv)))))))
      (when (memv (car info) +*lobs+)
	(let* ((vec (cdr info))
	       (locator (~ vec 1))
	       (port    (~ vec 2))
	       (amtp    (empty-pointer))
	       (dummy   (empty-pointer)))
	  (with-check-error (execute-query errhp)
	    (oci-lob-open svchp errhp locator +oci-lob-readwrite+))
	  (let loop ((first #t))
	    (let-values (((buf len) (buffer&length port)))
	      (with-check-error (execute-query errhp)
		(let1 piece (cond ((zero? len) +oci-last-piece+)
				  (first +oci-first-piece+)
				  (else +oci-next-piece+))
		  (oci-lob-write svchp errhp locator (address amtp) dummy
				 1 buf len piece
				 null-pointer null-pointer 0 
				 +sqlcs-implicit+)))
	      (unless (zero? len) (loop #f))))
	  (with-check-error (execute-query errhp)
	    ;;(oci-lob-flush-buffer svchp errhp locator 1)
	    (oci-lob-close svchp errhp locator)
	    ;;(oci-lob-free-temporary svchp errhp locator)
	    ;;(oci-descriptor-free locator +oci-dtype-lob+)
	    )
	  #;(unregister-ffi-finalizer locator)
	  )))

    #;
    (define (resolve-need-data type stmthp svchp errhp binds)
      (define (search-bind handle binds)
	(let loop ((binds binds))
	  (if (null? binds)
	      (error 'execute-query "internal error")
	      (let1 vec (cdar binds)
		(if (= (pointer->integer (~ vec 0)) (pointer->integer handle))
		    vec
		    (loop (cdr binds)))))))
      (define (store-data handle port type errhp piece)
	(define bv (make-bytevector 1024))
	;; the initial-piece must be +oci-first-piece+ but we don't check
	(let loop ((piece piece) (alen (empty-pointer)) 
		   (rcode (empty-pointer)))
	  (let1 size (get-bytevector-n! port bv 0 1024)
	    (cond ((eof-object? size)
		   (set! (~ alen 'value) 0)
		   (oci-stmt-set-piece-info handle type errhp bv 
					    (address alen)
					    +oci-last-piece+
					    null-pointer
					    (address rcode)))
		  (else
		   (set! (~ alen 'value) size)
		   (oci-stmt-set-piece-info handle type errhp bv 
					    (address alen)
					    piece null-pointer
					    (address rcode))
		   (loop +oci-next-piece+ alen rcode))))))
      (let1 status (with-check-error (execute-query errhp)
		     (oci-stmt-execute svchp stmthp errhp
				       type 0 null-pointer
				       null-pointer +oci-default+))
	(when (= status +oci-need-data+)
	  ;; do it
	  (let ((handle (empty-pointer))
		(type   (empty-pointer))
		(inout  (empty-pointer))
		(inter  (empty-pointer))
		(index  (empty-pointer))
		(piece  (empty-pointer)))
	    (oci-stmt-get-piece-info stmthp errhp
				     (address handle) (address type)
				     (address inout) (address inter)
				     (address index) (address piece))
	    (let1 value (~ (search-bind handle binds) 2)
	      (store-data handle value (pointer->integer type)
			  errhp (pointer->integer piece))))
	  (resolve-need-data type stmthp svchp errhp binds))))

    (let ((select? (= (~ stmt 'type) +oci-stmt-select+))
	  (stmthp  (~ stmt 'stmthp))
	  (svchp   (~ stmt 'connection 'svchp))
	  (errhp   (~ stmt 'connection 'env 'errhp))
	  (commit? (~ stmt 'connection 'auto-commit)))
      ;; bind if args is not null
      (unless (null? args)
	(let loop ((i 1) (args args))
	  (unless (null? args)
	    (bind-parameter stmt i (car args))
	    (loop (+ i 1) (cdr args)))))

      ;; resolve lob locator if there is
      (for-each (cut write-lob <> svchp errhp) (~ stmt 'binds))
      (with-check-error (execute-query errhp)
	(oci-stmt-execute svchp stmthp errhp
			  (if select? 0 1) 0 null-pointer
			  null-pointer 
			  (if commit? +oci-commit-on-success+ +oci-default+)))
      ;;(resolve-need-data (if select? 0 1) stmthp svchp errhp (~ stmt 'binds))
      (if select?
	  (make-cursor stmt stmthp errhp)
	  (row-count stmt))))

  (define (row-count stmt)
    (let ((stmthp (~ stmt 'stmthp))
	  (errhp   (~ stmt 'connection 'env 'errhp))
	  (count (empty-pointer)))
      (oci-attr-get stmthp +oci-htype-stmt+ (address count)
		    null-pointer +oci-attr-row-count+ errhp)
      (pointer->integer count)))

  (define (commit conn)
    (let1 errhp (~ conn 'env 'errhp)
      (with-check-error (commit errhp)
	(oci-trans-commit (~ conn 'svchp) errhp +oci-default+))))

  (define (rollback conn)
    (let1 errhp (~ conn 'env 'errhp)
      (with-check-error (commit errhp)
	(oci-trans-rollback (~ conn 'svchp) errhp +oci-default+))))

  (define-constant +fetch-row-count+ 1)

  (define (make-cursor stmt stmthp errhp)
    (define (p->s p len)
      (do ((s (make-string len)) (i 0 (+ i 1)))
	  ((= i len) s)
	(string-set! s i (integer->char (pointer-ref-c-uint8 p i)))))
    (define (handle-sql-type type param)
      (define (return type size)
	(if (negative? size)
	    (let1 lob (empty-pointer)
	      ;; use lob locator	      
	      (oci-descriptor-alloc (~ stmt 'connection 'env 'envhp)
				    (address lob) +oci-dtype-lob+ 
				    0 null-pointer)
	      ;; incase
	      (register-ffi-finalizer 
	       lob (lambda (dsc) (oci-descriptor-free dsc +oci-dtype-lob+)))
	      (values type -1 lob))
	    (values type size (allocate-pointer (* size +fetch-row-count+)))))
      (cond ((= type +sqlt-num+)
	     ;; convert either int or flt for now.
	     (let ((precision (empty-pointer))
		   (scale     (empty-pointer)))
	       (oci-attr-get param +oci-dtype-param+ (address precision)
			     null-pointer +oci-attr-precision+ errhp)
	       (oci-attr-get param +oci-dtype-param+ (address scale)
			     null-pointer +oci-attr-scale+ errhp)
	       (let ((*p (pointer->integer precision))
		     (*s (pointer->integer scale)))
		 (if (or (and (negative? *s) (zero? *p))
			 (and (zero? *s) (positive? *p)))
		     (return +sqlt-int+ size-of-int)
		     (return +sqlt-flt+ size-of-double)))))
	    ;; read as a string
	    ((= type +sqlt-dat+) (return +sqlt-str+ 32))
	    ((memv type +*lobs+) (return type -1))
	    (else 
	     ;; default string
	     (let1 column-size (empty-pointer)
	       (oci-attr-get param +oci-dtype-param+ (address column-size)
			     null-pointer +oci-attr-data-size+ errhp)
	       (return +sqlt-str+ 
		       ;; including NULL
		       (+ 1 (pointer->integer column-size)))))))
    ;; collect columns info
    (let ((param (empty-pointer))
	  (dtype (empty-pointer))
	  (column-length (empty-pointer))
	  (defnp (empty-pointer)))
      (let loop ((i 1) (r '()))
	(cond ((= (oci-param-get stmthp +oci-htype-stmt+ errhp
				 (address param) i)
		  +oci-success+)
	       (oci-attr-get param +oci-dtype-param+ (address dtype)
			     null-pointer +oci-attr-data-type+ errhp)
	       (let*-values (((type sizeof buffer)
			      (handle-sql-type (pointer->integer dtype) param))
			     ((*lob?) (negative? sizeof)))
		 ;; get column name
		 (oci-attr-get param +oci-dtype-param+ (address dtype)
			       (address column-length) +oci-attr-name+ errhp)
		 ;; name must be string (i hope it's utf8)
		 (let1 column-name (p->s dtype (pointer->integer column-length))
		   ;; reset the pointer.
		   (set! (~ dtype 'value) 0)
		   ;; TODO buffer the rows
		   (let ((retcodes (if *lob?
				       null-pointer
				       (allocate-pointer 
					(* size-of-short +fetch-row-count+))))
			 (indicators (if *lob?
					 null-pointer
					 (allocate-pointer 
					  (* size-of-short
					     +fetch-row-count+)))))
		     (oci-define-by-pos stmthp (address defnp) errhp
					i  ;; column index
					;; buffer
					(if *lob? (address buffer) buffer)
					sizeof
					type
					indicators
					null-pointer
					retcodes
					+oci-default+)
		     (loop (+ i 1)
			   ;; TODO make macro for this info
			   (cons (vector type column-name buffer
					 retcodes indicators)
				 r))))))
	      (else 
	       (make <oracle-cursor> :statement stmt :columns (reverse! r)))))))

  (define (make-oracle-blob-port conn blob-locator)
    (let ((lenp (empty-pointer))
	  (svchp (~ conn 'svchp))
	  (errhp (~ conn 'env 'errhp)))
      (oci-lob-get-length svchp errhp blob-locator (address lenp))
      (let* ((len  (pointer->integer lenp))
	     (amtp (integer->pointer len))
	     (buffer (make-bytevector 1024)))
	(define (read! bv start count)
	  (if (<= len 0)
	      0
	      ;; oci-lob-read doesn't have offset of output buffer...
	      (let* ((bufl (if (< count 1024) count 1024))
		     (ret (oci-lob-read svchp errhp blob-locator (address amtp)
					1 buffer bufl null-pointer null-pointer
					0 +sqlcs-implicit+)))
		(let1 read (pointer->integer amtp)
		  (set! len (- len read))
		  (set! (~ amtp 'value) 0)
		  (bytevector-copy! buffer 0 bv start read)
		  read))))
	;; do nothing for now
	(define (close!) )
	(make-custom-binary-input-port "oracle-blob" read! #f #f close!))))

  (define (make-oracle-clob-port conn blob-locator)
    (let ((lenp (empty-pointer))
	  (svchp (~ conn 'svchp))
	  (errhp (~ conn 'env 'errhp)))
      (oci-lob-get-length svchp errhp blob-locator (address lenp))
      (let* ((len  (pointer->integer lenp))
	     (amtp (integer->pointer len))
	     (buffer (make-bytevector 1024)))
	(define (read! str start count)
	  (if (<= len 0)
	      0
	      ;; oci-lob-read doesn't have offset of output buffer...
	      (let* ((bufl (if (< count 1024) count 1024))
		     (ret (oci-lob-read svchp errhp blob-locator (address amtp)
					1 buffer bufl null-pointer null-pointer
					0 +sqlcs-implicit+)))
		(let1 read (pointer->integer amtp)
		  (set! len (- len read))
		  (set! (~ amtp 'value) 0)
		  ;; copy string
		  (let1 s (utf8->string buffer 0 read)
		    (do ((i 0 (+ i 1)))
			((= i count))
		      (set! (~ str (+ i start)) (~ s i))))
		  read))))
	;; do nothing for now
	(define (close!) )
	(make-custom-textual-input-port "oracle-clob" read! #f #f close!))))

  (define (c->scheme conn type buffer)
    (cond ((= type +sqlt-int+)
	   (pointer-ref-c-int32 buffer 0))
	  ((= type +sqlt-flt+)
	   (pointer-ref-c-double buffer 0))
	  ((= type +sqlt-blob+)
	   (make-oracle-blob-port conn buffer))
	  ((= type +sqlt-clob+)
	   (make-oracle-clob-port conn buffer))
	  ((= type +sqlt-dat+)
	   (pointer->string buffer))
	  (else
	   (pointer->string buffer))))

  (define (query-columns query)
    (apply vector (map (cut vector-ref <> 1) (~ query 'columns ))))

  ;; If blob entry is NULL then this error will raised, however
  ;; it's incovenient if we need to re-write SQL so ignore it.
  (define-constant ORA-01405 1405)

  ;; fetch one row
  (define (fetch-row cursor :key (orientation +oci-default+)
		     (offset (~ cursor 'offset))
		     (ignores (list ORA-01405)))
    (define (db-value->scheme info-vec)
      ;; we don't do anything with indicator for now
      (let ((type (~ info-vec 0))
	    (buffer (~ info-vec 2)))
	(c->scheme (~ cursor 'statement 'connection) type buffer)))

    (define (finish)
      ;; get row
      (let1 columns (~ cursor 'columns)
	;; buffers are must be filled now
	(list->vector (map db-value->scheme columns))))

    ;; we only fetch one rwo each time for now. easy going implementation
    (let* ((stmt (~ cursor 'statement))
	   (errhp (~ stmt 'connection 'env 'errhp))
	   (status (oci-stmt-fetch (~ stmt 'stmthp) 
				   errhp
				   ;; only one row
				   +fetch-row-count+ 
				   orientation 
				   offset
				   +oci-default+)))
      (set! (~ cursor 'offset) (+ offset +fetch-row-count+))
      (cond ((memv status `(,+oci-success+ ,+oci-success-with-info+)) (finish))
	    ((= status +oci-no-data+) #f)
	    (else
	     ;; check error
	     (check-error 'fetch-row errhp status ignores)
	     ;; must be ORA-01405 
	     (finish)))))
  
  (define *scheme->c-table* (make-eq-hashtable))
  (define-syntax define-scheme->c
    (syntax-rules ()
      ;; statement is not needed
      ((_ (class var) body ...)
       (set! (~ *scheme->c-table* class) (lambda (ignore var) body ...)))
      ; for blob
      ((_ (class stmt var) body ...)
       (set! (~ *scheme->c-table* class) (lambda (stmt var) body ...)))))

  (define-scheme->c (<integer> n)
    (if (< #x-80000000 n #x7FFFFFFF)
	;; +sqlt-int+
	(values +sqlt-int+ size-of-int +oci-default+ (pack "=l" n))
	;; +sqlt-long+
	(error #f "SQLT_LONG is not supported yet" n)))

  (define-scheme->c (<real> n)
    (values +sqlt-flt+ size-of-float +oci-default+ (pack "=f" n)))

  ;; supports only utf8 string for now
  (define-scheme->c (<string> s)
    (let1 bv (string->utf8 (string-append s "\x0;"))
      ;; TODO should we use +sqlt-chr+ (varchar2)?
      (values +sqlt-str+ (bytevector-length bv) +oci-default+ bv)))

  (define-scheme->c (<bytevector> bv)
    (values +sqlt-bin+ (bytevector-length bv) +oci-default+ bv))

  ;; TODO date and time
  (define-scheme->c (<port> stmt port)
    (unless (input-port? port)
      (error 'bind-parameter "input port required" port))
    ;; use lob locator
    (let-values (((lobtype sqltype) (if (binary-port? port)
					(values +oci-temp-blob+ +sqlt-blob+)
					(values +oci-temp-clob+ +sqlt-clob+))))
      (let* ((locator (empty-pointer))
	     (env     (~ stmt 'connection 'env))
	     (errhp   (~ env 'errhp))
	     (svchp   (~ stmt 'connection 'svchp)))

	(with-check-error (bind-parameter errhp)
	  (oci-descriptor-alloc (~ env 'envhp) (address locator) 
				+oci-dtype-lob+ 0 null-pointer)
	  (oci-lob-create-temporary svchp errhp locator 
				    +oci-default+ +oci-default+ lobtype
				    #f +oci-duration-session+))
	(register-ffi-finalizer locator
				(cut oci-descriptor-free <> +oci-dtype-lob+))
	;;(values sqltype 0 +oci-data-at-exec+ null-pointer)
	(values sqltype size-of-void* +oci-default+ locator))))

  (define (bind-parameter stmt pos/name value)
    (define (lookup-sqltype value)
      (let1 class (class-of value)
	(or (and-let* ((conv (~ *scheme->c-table* class)))
	      (conv stmt value))
	    (error 'bind-parameter "given value is not supported" value))))
    (define (wrap-value type value)
      (if (memv type +*lobs+) (address value) value))
    (define (bind-rec name? ovalue)
      (let-values (((sqltype size mode value) (lookup-sqltype ovalue)))
	(let ((bind (empty-pointer))
	      (stmthp (~ stmt 'stmthp))
	      (errhp  (~ stmt 'connection 'env 'errhp)))
	  (with-check-error (bind-parameter errhp)
	    (if name?
		(let1 name (format "~s" pos/name)
		  (oci-bind-by-name stmthp (address bind) errhp
				    name (string-length name)
				    (wrap-value sqltype value)
				    size sqltype
				    null-pointer null-pointer null-pointer
				    0 null-pointer mode))
		(oci-bind-by-pos stmthp (address bind) errhp 
				 pos/name (wrap-value sqltype value)
				 size sqltype
				 null-pointer null-pointer null-pointer
				 0 null-pointer mode)))
	  ;; To avoid get GCed.
	  ;; FIXME this might causes memory leak when re-using statement.
	  (set! (~ stmt 'binds)
		(acons sqltype (vector bind value ovalue) (~ stmt 'binds))))))
    (cond ((fixnum? pos/name) 
	   (unless (positive? pos/name)
	     (error 'bind-parameter "position index must be positive number"))
	   (bind-rec #f value))
	  ((keyword? pos/name) (bind-rec #t value))
	  (else (error 'bind-parameter "invalid position" pos/name))))

)