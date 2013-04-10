;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; oracle/constant.scm - OCI constant variables
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

(library (oracle constant)
    ;; this exports define-constant but it's not so harmful
    (export :all)
    (import (only (sagittarius) define-constant))

  ;; default value for parameters and attributes
  (define-constant +oci-default+      #x00)
  ;; application is in threaded environment
  (define-constant +oci-threaded+     #x01)
  ;; the application is in object environment
  (define-constant +oci-object+       #x02)
  ;; non blocking mode of operation
  (define-constant +oci-non-blocking+ #x04)
  ;; the environment handle will not be protected by a mutex internally
  (define-constant +oci-env-no-mutex+ #x08)

  ;; Handle types
  (define-constant +oci-htype-env+    1)      ; environment handle
  (define-constant +oci-htype-error+  2)      ; error handle
  (define-constant +oci-htype-svcctx+ 3)      ; service handle
  (define-constant +oci-htype-stmt+   4)      ; statement handle
  (define-constant +oci-htype-bind+   5)      ; bind handle
  (define-constant +oci-htype-define+ 6)      ; define handle
  (define-constant +oci-htype-describe+ 7)    ; describe handle
  (define-constant +oci-htype-server+ 8)      ; server handle
  (define-constant +oci-htype-session+ 9)     ; authentication handle
  (define-constant +oci-htype-trans+  10)     ; transaction handle
  ;; complex object retrieval handle
  (define-constant +oci-htype-complexobject+ 11)
  (define-constant +oci-htype-security+ 12)   ; security handle

  ;; Descriptor types

  (define-constant +oci-dtype-lob+               50) ; lob locator
  (define-constant +oci-dtype-snap+              51) ; snapshot
  (define-constant +oci-dtype-rset+              52) ; result set
  (define-constant +oci-dtype-param+             53) ; parameter descriptor obtained from ocigparm
  (define-constant +oci-dtype-rowid+             54) ; rowid
  (define-constant +oci-dtype-complexobjectcomp+ 55) ; complex object retrieval descriptor
  (define-constant +oci-dtype-file+              56) ; File Lob locator
  (define-constant +oci-dtype-aqenq-options+     57) ; enqueue options
  (define-constant +oci-dtype-aqdeq-options+     58) ; dequeue options
  (define-constant +oci-dtype-aqmsg-properties+  59) ; message properties
  (define-constant +oci-dtype-aqagent+           60) ; aq agent

  ;; LOB types
  (define-constant +oci-temp-blob+ 1)	; LOB type BLOB
  (define-constant +oci-temp-clob+ 2)	; LOB type CLOB

  ;; lob open mode
  (define-constant +oci-lob-readonly+      1) ; readonly mode open for ILOB types
  (define-constant +oci-lob-readwrite+     2) ; read write mode open for ILOBs
  (define-constant +oci-lob-writeonly+     3) ; Writeonly mode open for ILOB types
  (define-constant +oci-lob-appendonly+    4) ; Appendonly mode open for ILOB types
  (define-constant +oci-lob-fulloverwrite+ 5) ; Completely overwrite ILOB
  (define-constant +oci-lob-fullread+      6) ; Doing a Full Read of ILOB


  ;; Error Return Values-

  (define-constant +oci-continue+          -24200) ; Continue with the body of the OCI function
  (define-constant +oci-still-executing+   -3123) ; OCI would block error
  (define-constant +oci-invalid-handle+     -2) ; maps to SQL-INVALID-HANDLE
  (define-constant +oci-error+              -1) ; maps to SQL-ERROR
  (define-constant +oci-success+             0) ; maps to SQL-SUCCESS of SAG CLI
  (define-constant +oci-success-with-info+   1) ; maps to SQL-SUCCESS-WITH-INFO
  (define-constant +oci-need-data+          99) ; maps to SQL-NEED-DATA
  (define-constant +oci-no-data+           100) ; maps to SQL-NO-DATA

  ;; Parsing Syntax Types-
  (define-constant +oci-ntv-syntax+ 1) ; Use what so ever is the native lang of server
  (define-constant +oci-v7-syntax+  2) ; V7 language
  (define-constant +oci-v8-syntax+  3) ; V8 language

  ;; Attribute types
  (define-constant +oci-attr-fncode+                  1) ; the OCI function code
  (define-constant +oci-attr-object+                  2) ; is the environment initialized in object mode
  (define-constant +oci-attr-nonblocking-mode+        3) ; non blocking mode
  (define-constant +oci-attr-sqlcode+                 4) ; the SQL verb
  (define-constant +oci-attr-env+                     5) ; the environment handle
  (define-constant +oci-attr-server+                  6) ; the server handle
  (define-constant +oci-attr-session+                 7) ; the user session handle
  (define-constant +oci-attr-trans+                   8) ; the transaction handle
  (define-constant +oci-attr-row-count+               9) ; the rows processed so far
  (define-constant +oci-attr-sqlfncode+              10) ; the SQL verb of the statement
  (define-constant +oci-attr-prefetch-rows+          11) ; sets the number of rows to prefetch
  (define-constant +oci-attr-nested-prefetch-rows+   12) ; the prefetch rows of nested table
  (define-constant +oci-attr-prefetch-memory+        13) ; memory limit for rows fetched
  (define-constant +oci-attr-nested-prefetch-memory+ 14) ; memory limit for nested rows
  (define-constant +oci-attr-char-count+             15) ; this specifies the bind and define size in characters
  (define-constant +oci-attr-pdscl+                  16) ; packed decimal scale
  (define-constant +oci-attr-pdfmt+                  17) ; packed decimal format
  (define-constant +oci-attr-param-count+            18) ; number of column in the select list
  (define-constant +oci-attr-rowid+                  19) ; the rowid
  (define-constant +oci-attr-charset+                20) ; the character set value
  (define-constant +oci-attr-nchar+                  21) ; NCHAR type
  (define-constant +oci-attr-username+               22) ; username attribute
  (define-constant +oci-attr-password+               23) ; password attribute
  (define-constant +oci-attr-stmt-type+              24) ; statement type
  (define-constant +oci-attr-internal-name+          25) ; user friendly global name
  (define-constant +oci-attr-external-name+          26) ; the internal name for global txn
  (define-constant +oci-attr-xid+                    27) ; XOPEN defined global transaction id
  (define-constant +oci-attr-trans-lock+             28) ;
  (define-constant +oci-attr-trans-name+             29) ; string to identify a global transaction
  (define-constant +oci-attr-heapalloc+              30) ; memory allocated on the heap
  (define-constant +oci-attr-charset-id+             31) ; Character Set ID
  (define-constant +oci-attr-charset-form+           32) ; Character Set Form
  (define-constant +oci-attr-maxdata-size+           33) ; Maximumsize of data on the server
  (define-constant +oci-attr-cache-opt-size+         34) ; object cache optimal size
  (define-constant +oci-attr-cache-max-size+         35) ; object cache maximum size percentage
  (define-constant +oci-attr-pinoption+              36) ; object cache default pin option
  (define-constant +oci-attr-alloc-duration+         37) ; object cache default allocation duration
  (define-constant +oci-attr-pin-duration+           38) ; object cache default pin duration
  (define-constant +oci-attr-fdo+                    39) ; Format Descriptor object attribute
  (define-constant +oci-attr-postprocessing-callback+ 40) ; Callback to process outbind data
  (define-constant +oci-attr-postprocessing-context+ 41) ; Callback context to process outbind data
  (define-constant +oci-attr-rows-returned+          42) ; Number of rows returned in current iter - for Bind handles
  (define-constant +oci-attr-focbk+                  43) ; Failover Callback attribute
  (define-constant +oci-attr-in-v8-mode+             44) ; is the server/service context in V8 mode
  (define-constant +oci-attr-lobempty+               45) ; empty lob ?
  (define-constant +oci-attr-sesslang+               46) ; session language handle

  ;; AQ Attribute Types
  ;; Enqueue Options

  (define-constant +oci-attr-visibility+         47) ; visibility
  (define-constant +oci-attr-relative-msgid+     48) ; relative message id
  (define-constant +oci-attr-sequence-deviation+ 49) ; sequence deviation

  (define-constant +oci-attr-nocache+ 87) ; Tempoaray LOB

  ;;-Credential Types-
  (define-constant +oci-cred-rdbms+ 1)   ; database username/password
  (define-constant +oci-cred-ext+ 2)     ; externally provided credentials


  ;; Describe Handle Parameter Attributes
  ;; Attributes common to Columns and Stored Procs

  (define-constant +oci-attr-data-size+ 1) ; maximum size of the data
  (define-constant +oci-attr-data-type+ 2) ; the sql type of the column/argument
  (define-constant +oci-attr-disp-size+ 3) ; the display size
  (define-constant +oci-attr-name+      4) ; the name of the column/argument
  (define-constant +oci-attr-precision+ 5) ; precision if number type
  (define-constant +oci-attr-scale+     6) ; scale if number type
  (define-constant +oci-attr-is-null+   7) ; is it null ?
  (define-constant +oci-attr-type-name+ 8)

  ;; name of the named data type or a package name for package private types

  (define-constant +oci-attr-schema-name+ 9) ; the schema name
  (define-constant +oci-attr-sub-name+ 10) ; type name if package private type
  (define-constant +oci-attr-position+ 11) ; relative position of col/arg in the list of cols/args



  ;;- OCI Statement Types -
  (define-constant +oci-stmt-select+  1) ; select statement
  (define-constant +oci-stmt-update+  2) ; update statement
  (define-constant +oci-stmt-delete+  3) ; delete statement
  (define-constant +oci-stmt-insert+  4) ; insert statement
  (define-constant +oci-stmt-create+  5) ; create statement
  (define-constant +oci-stmt-drop+    6) ; drop statement
  (define-constant +oci-stmt-alter+   7) ; alter statement
  (define-constant +oci-stmt-begin+   8) ; begin ... (pl/sql statement)
  (define-constant +oci-stmt-declare+ 9) ; declare .. (pl/sql statement )

  ;; SQL types
  (define-constant +sqlt-chr+      1) ; (ORANET TYPE) character string
  (define-constant +sqlt-num+      2) ; (ORANET TYPE) oracle numeric
  (define-constant +sqlt-int+      3) ; (ORANET TYPE) integer
  (define-constant +sqlt-flt+      4) ; (ORANET TYPE) Floating point number
  (define-constant +sqlt-str+      5) ; zero terminated string
  (define-constant +sqlt-vnu+      6) ; NUM with preceding length byte
  (define-constant +sqlt-pdn+      7) ; (ORANET TYPE) Packed Decimal Numeric
  (define-constant +sqlt-lng+      8) ; long
  (define-constant +sqlt-vcs+      9) ; Variable character string
  (define-constant +sqlt-non+      10) ; Null/empty PCC Descriptor entry
  (define-constant +sqlt-rid+      11) ; rowid
  (define-constant +sqlt-dat+      12) ; date in oracle format
  (define-constant +sqlt-vbi+      15) ; binary in VCS format
  (define-constant +sqlt-bfloat+   21) ; Native Binary float
  (define-constant +sqlt-bdouble+  22) ; NAtive binary double 
  (define-constant +sqlt-bin+      23) ; binary data(DTYBIN)
  (define-constant +sqlt-lbi+      24) ; long binary
  (define-constant +sqlt-uin+      68) ; unsigned integer
  (define-constant +sqlt-sls+      91) ; Display sign leading separate
  (define-constant +sqlt-lvc+      94) ; Longer longs (char)
  (define-constant +sqlt-lvb+      95) ; Longer long binary
  (define-constant +sqlt-afc+      96) ; Ansi fixed char
  (define-constant +sqlt-avc+      97) ; Ansi Var char
  (define-constant +sqlt-ibfloat+  100) ; binary float canonical
  (define-constant +sqlt-ibdouble+ 101) ; binary double canonical
  (define-constant +sqlt-cur+      102) ; cursor  type
  (define-constant +sqlt-rdd+      104) ; rowid descriptor
  (define-constant +sqlt-lab+      105) ; label type
  (define-constant +sqlt-osl+      106) ; oslabel type
  (define-constant +sqlt-nty+      108) ; named object type
  (define-constant +sqlt-ref+      110) ; ref type
  (define-constant +sqlt-clob+     112) ; character lob
  (define-constant +sqlt-blob+     113) ; binary lob
  (define-constant +sqlt-bfilee+   114) ; binary file lob
  (define-constant +sqlt-cfilee+   115) ; character file lob
  (define-constant +sqlt-rset+     116) ; result set type
  (define-constant +sqlt-nco+      122) ; named collection type (varray or nested table)
  (define-constant +sqlt-vst+      155) ; OCIString type
  (define-constant +sqlt-odt+      156) ; OCIDate type

  ;; datetimes and intervals
  (define-constant +sqlt-date+          184) ; ANSI Date
  (define-constant +sqlt-time+          185) ; TIME
  (define-constant +sqlt-time-tz+       186) ; TIME WITH TIME ZONE
  (define-constant +sqlt-timestamp+     187) ; TIMESTAMP
  (define-constant +sqlt-timestamp-tz+  188) ; TIMESTAMP WITH TIME ZONE
  (define-constant +sqlt-interval-ym+   189) ; INTERVAL YEAR TO MONTH
  (define-constant +sqlt-interval-ds+   190) ; INTERVAL DAY TO SECOND
  (define-constant +sqlt-timestamp-ltz+ 232) ; TIMESTAMP WITH LOCAL TZ

  (define-constant +sqlt-pnty+   241) ; pl/sql representation of named types

  ;; bind and define options
  (define-constant +oci-sb2-ind-ptr+   #x01) ; unused
  (define-constant +oci-data-at-exec+  #x02) ; data at execute time
  (define-constant +oci-dynamic-fetch+ #x02) ; fetch dynamically
  (define-constant +oci-piecewise+     #x04) ; piecewise DMLs or fetch

  ;;-Execution Modes-
  (define-constant +oci-batch-mode+ #x01) ; batch the oci statement for execution
  (define-constant +oci-exact-fetch+ #x02) ; fetch the exact rows specified
  (define-constant +oci-keep-fetch-state+ #x04)	 ; unused
  (define-constant +oci-scrollable-cursor+ #x08) ; cursor scrollable
  (define-constant +oci-describe-only+ #x10) ; only describe the statement
  (define-constant +oci-commit-on-success+ #x20) ; commit, if successful execution


  ;; CHAR/NCHAR/VARCHAR2/NVARCHAR2/CLOB/NCLOB char set "form" information
  (define-constant +sqlcs-implicit+ 1) ; for CHAR, VARCHAR2, CLOB w/o a specified set
  (define-constant +sqlcs-nchar+    2) ; for NCHAR, NCHAR VARYING, NCLOB
  (define-constant +sqlcs-explicit+ 3) ; for CHAR, etc, with "CHARACTER SET ..." syntax
  (define-constant +sqlcs-flexible+ 4) ; for PL/SQL "flexible" parameters
  (define-constant +sqlcs-lit-null+ 5) ; for typecheck of NULL and empty_clob() lits

  ;; OBJECT duration
  (define-constant +oci-duration-invalid+ #xFFFF) ; Invalid duration
  (define-constant +oci-duration-begin+   10) ; beginning sequence of duration
  (define-constant +oci-duration-null+    9)  ; null duration
  (define-constant +oci-duration-default+ 8)  ; default
  (define-constant +oci-duration-user-callback+ 7)
  (define-constant +oci-duration-next+ 6)     ; next special duration
  (define-constant +oci-duration-session+ 10) ; the end of user session
  (define-constant +oci-duration-trans+ 11) ; the end of user transaction
  (define-constant +oci-duration-statement+ 13)
                                              
  ;; flags
  (define-constant +oci-one-piece+   0)	; one piece
  (define-constant +oci-first-piece+ 1)	; the first piece
  (define-constant +oci-next-piece+  2)	; the next of many pieces
  (define-constant +oci-last-piece+  3)	; the last piece

)