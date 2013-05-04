;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; oracle/oci.scm - OCI functions
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
(library (oracle oci)
    (export oci-initialize
	    oci-env-init
	    oci-env-create
	    oci-handle-alloc
	    oci-handle-free
	    oci-logon	    ;; not using
	    oci-logoff	    ;; not using
	    oci-server-attach
	    oci-server-detach
	    oci-session-begin
	    oci-session-end
	    oci-error-get
	    oci-stmt-prepare
	    oci-stmt-execute
	    oci-stmt-fetch
	    oci-trans-commit
	    oci-trans-rollback
	    oci-param-get
	    oci-attr-get
	    oci-attr-set
	    oci-bind-by-pos
	    oci-bind-by-name
	    oci-bind-dynamic
	    oci-stmt-get-piece-info
	    oci-stmt-set-piece-info
	    oci-define-by-pos
	    oci-define-dynamic
	    oci-descriptor-alloc
	    oci-descriptor-free
	    oci-lob-open
	    oci-lob-close
	    oci-lob-is-open
	    oci-lob-flush-buffer
	    oci-lob-create-temporary
	    oci-lob-free-temporary
	    oci-lob-get-length
	    oci-lob-read
	    oci-lob-write
	    )
    (import (rnrs)
	    (sagittarius)
	    (sagittarius ffi))

  (define *oci*
    (open-shared-library
     (string-append (cond-expand 
		     (linux "libclntsh")
		     (windows "oci")
		     (else (error '(oracle oci) "not supported yet")))
		    (shared-object-suffix))))

  (define-c-typedef
    (ub2 unsigned-short)
    (sb2 short)
    (ub4 unsigned-int)
    (sb4 int))

  (define-syntax define-oci-function
    (syntax-rules ()
      ((_ (scheme-name c-name) c-return c-params)
       (define scheme-name (c-function *oci* c-return c-name c-params)))))

  ;; following 2 are deprecated so don't use.
  ;; sword OCIInitialize (ub4 mode,
  ;;                      CONST dvoid *ctxp,
  ;;                      CONST dvoid *(*malocfp)(void *, size_t),
  ;;                      CONST dvoid *(*ralocfp)(void *, void *, size_t),
  ;;                      CONST void  (*mfreefp)(void *, void *))
  ;; We don't use custom allocations so declare it as void*
  (define-oci-function (oci-initialize OCIInitialize)
    int (unsigned-int void* void* void* void*))

  ;; sword OCIEnvInit ( OCIEnv **envhpp,
  ;;                    ub4    mode,
  ;;                    size_t xtramemsz,
  ;;                    dvoid **usrmempp)
  (define-oci-function (oci-env-init OCIEnvInit)
    int (void* unsigned-int size_t void*))

  ;; new one?
  ;; sword OCIEnvCreate( OCIEnv        **envhpp,
  ;;                     ub4           mode,
  ;;                     CONST dvoid   *ctxp,
  ;;                     CONST dvoid   *(*malocfp)(dvoid *ctxp, size_t size),
  ;;                     CONST dvoid   *(*ralocfp)(dvoid *, dvoid *, size_t),
  ;;                     CONST void    (*mfreefp)(dvoid *, dvoid *)),
  ;;                     size_t        xtramemsz,
  ;;                     dvoid         **usrmempp)
  (define-oci-function (oci-env-create OCIEnvCreate)
    int (void* unsigned-int void* void* void* void* size_t void*))

  ;; sword OCIHandleAlloc ( CONST dvoid   *parenth,
  ;;                        dvoid         **hndlpp,
  ;;                        ub4           type,
  ;;                        size_t        xtramem_sz,
  ;;                        dvoid         **usrmempp )
  (define-oci-function (oci-handle-alloc OCIHandleAlloc)
    int (void* void* unsigned-int size_t void*))

  ;; sword OCIHandleFree ( dvoid     *hndlp,
  ;;                       ub4       type )
  (define-oci-function (oci-handle-free OCIHandleFree) int (void* unsigned-int))

  ;; sword OCILogon ( OCIEnv          *envhp,
  ;;                  OCIError        *errhp,
  ;;                  OCISvcCtx       **svchp,
  ;;                  CONST OraText   *username,
  ;;                  ub4             uname_len,
  ;;                  CONST OraText   *password,
  ;;                  ub4             passwd_len,
  ;;                  CONST OraText   *dbname,
  ;;                  ub4             dbname_len )
  (define-oci-function (oci-logon OCILogon)
    int (void* void* void* char*
	 unsigned-int char* unsigned-int char* unsigned-int))

  ;; sword OCILogoff ( OCISvcCtx      *svchp
  ;;                   OCIError       *errhp )
  (define-oci-function (oci-logoff OCILogoff) int (void* void*))

  ;; sword OCIServerAttach ( OCIServer     *srvhp,
  ;;                         OCIError      *errhp,
  ;;                         CONST text    *dblink,
  ;;                         sb4           dblink_len,
  ;;                         ub4           mode )
  (define-oci-function (oci-server-attach OCIServerAttach)
    int (void* void* void* int unsigned-int))

  ;; sword OCIServerDetach ( OCIServer   *srvhp,
  ;;                         OCIError    *errhp,
  ;;                         ub4         mode )
  (define-oci-function (oci-server-detach OCIServerDetach)
    int (void* void* unsigned-int))

  ;; sword OCISessionBegin ( OCISvcCtx     *svchp,
  ;;                         OCIError      *errhp,
  ;;                         OCISession    *usrhp,
  ;;                         ub4           credt,
  ;;                         ub4           mode )
  (define-oci-function (oci-session-begin OCISessionBegin)
    int (void* void* void* unsigned-int unsigned-int))

  ;; sword OCISessionEnd ( OCISvcCtx       *svchp,
  ;;                       OCIError        *errhp,
  ;;                       OCISession      *usrhp,
  ;;                       ub4             mode )
  (define-oci-function (oci-session-end OCISessionEnd)
    int (void* void* void* unsigned-int))

  ;; sword OCIErrorGet ( dvoid      *hndlp,
  ;;                     ub4        recordno,
  ;;                     text       *sqlstate,
  ;;                     sb4        *errcodep,
  ;;                     text       *bufp,
  ;;                     ub4        bufsiz,
  ;;                     ub4        type )
  (define-oci-function (oci-error-get OCIErrorGet)
    int (void* unsigned-int void* void* char* unsigned-int unsigned-int))

  ;; sword OCIStmtPrepare ( OCIStmt       *stmtp,
  ;;                        OCIError      *errhp,
  ;;                        CONST text    *stmt,
  ;;                        ub4           stmt_len,
  ;;                        ub4           language,
  ;;                        ub4           mode )
  (define-oci-function (oci-stmt-prepare OCIStmtPrepare)
    int (void* void* char* unsigned-int unsigned-int unsigned-int))

  ;; sword OCIStmtExecute ( OCISvcCtx           *svchp,
  ;;                        OCIStmt             *stmtp,
  ;;                        OCIError            *errhp,
  ;;                        ub4                 iters,
  ;;                        ub4                 rowoff,
  ;;                        CONST OCISnapshot   *snap_in,
  ;;                        OCISnapshot         *snap_out,
  ;;                        ub4                 mode )
  (define-oci-function (oci-stmt-execute OCIStmtExecute)
    int (void* void* void* unsigned-int unsigned-int void* void* unsigned-int))

  ;; sword OCIStmtFetch2 ( OCIStmt     *stmthp,
  ;;                       OCIError    *errhp, 
  ;;                       ub4         nrows,
  ;;                       ub2         orientation,
  ;;                       sb4         fetchOffset,
  ;;                       ub4         mode )
  ;; we are encouraged to use this so use it :)
  (define-oci-function (oci-stmt-fetch OCIStmtFetch2)
    int (void* void* unsigned-int unsigned-short int unsigned-int))

  ;; sword OCITransCommit ( OCISvcCtx    *svchp,
  ;;                        OCIError     *errhp,
  ;;                        ub4          flags )
  (define-oci-function (oci-trans-commit OCITransCommit) 
    int (void* void* unsigned-int))

  ;; sword OCITransRollback ( dvoid        *svchp, 
  ;;                          OCIError     *errhp,
  ;;                          ub4          flags )
  (define-oci-function (oci-trans-rollback OCITransRollback) 
    int (void* void* unsigned-int))

  ;; sword OCIParamGet ( CONST dvoid       *hndlp,
  ;;                     ub4               htype,
  ;;                     OCIError          *errhp,
  ;;                     dvoid             **parmdpp,
  ;;                     ub4               pos )
  (define-oci-function (oci-param-get OCIParamGet)
    int (void* unsigned-int void* void* unsigned-int))

  ;; sword OCIAttrGet ( CONST dvoid    *trgthndlp,
  ;;                    ub4            trghndltyp,
  ;;                    dvoid          *attributep,
  ;;                    ub4            *sizep,
  ;;                    ub4            attrtype,
  ;;                    OCIError       *errhp )
  (define-oci-function (oci-attr-get OCIAttrGet)
    int (void* unsigned-int void* void* unsigned-int void*))

  ;; sword OCIAttrSet ( dvoid       *trgthndlp,
  ;;                    ub4         trghndltyp,
  ;;                    dvoid       *attributep,
  ;;                    ub4         size,
  ;;                    ub4         attrtype,
  ;;                    OCIError    *errhp )
  (define-oci-function (oci-attr-set OCIAttrSet)
    int (void* unsigned-int void* unsigned-int unsigned-int void*))

  ;; sword OCIBindByPos ( OCIStmt      *stmtp, 
  ;;                      OCIBind      **bindpp,
  ;;                      OCIError     *errhp,
  ;;                      ub4          position,
  ;;                      dvoid        *valuep,
  ;;                      sb4          value_sz,
  ;;                      ub2          dty,
  ;;                      dvoid        *indp,
  ;;                      ub2          *alenp,
  ;;                      ub2          *rcodep,
  ;;                      ub4          maxarr_len,
  ;;                      ub4          *curelep, 
  ;;                      ub4          mode )
  (define-oci-function (oci-bind-by-pos OCIBindByPos)
    int (void* void* void* unsigned-int void* int
	 unsigned-short void* void* void* unsigned-int void* unsigned-int))

  ;; sword OCIBindByName ( OCIStmt       *stmtp, 
  ;;                       OCIBind       **bindpp,
  ;;                       OCIError      *errhp,
  ;;                       CONST text    *placeholder,
  ;;                       sb4           placeh_len,
  ;;                       dvoid         *valuep,
  ;;                       sb4           value_sz,
  ;;                       ub2           dty,
  ;;                       dvoid         *indp,
  ;;                       ub2           *alenp,
  ;;                       ub2           *rcodep,
  ;;                       ub4           maxarr_len,
  ;;                       ub4           *curelep, 
  ;;                       ub4           mode )
  (define-oci-function (oci-bind-by-name OCIBindByName)
    int (void* void* void* void* int void* int short
	 void* void* void* unsigned-int void* unsigned-int))

  ;; sword OCIBindDynamic ( OCIBind     *bindp,
  ;;                        OCIError    *errhp,
  ;;                        dvoid       *ictxp, 
  ;;                        OCICallbackInBind         (icbfp)(/*_
  ;;                                 dvoid            *ictxp,
  ;;                                 OCIBind          *bindp,
  ;;                                 ub4              iter, 
  ;;                                 ub4              index, 
  ;;                                 dvoid            **bufpp,
  ;;                                 ub4              *alenp,
  ;;                                 ub1              *piecep, 
  ;;                                 dvoid            **indpp */),
  ;;                                 dvoid            *octxp,
  ;;                        OCICallbackOutBind        (ocbfp)(/*_
  ;;                                 dvoid            *octxp,
  ;;                                 OCIBind          *bindp,
  ;;                                 ub4              iter, 
  ;;                                 ub4              index, 
  ;;                                 dvoid            **bufpp, 
  ;;                                 ub4              **alenpp,
  ;;                                 ub1              *piecep,
  ;;                                 dvoid            **indpp, 
  ;;                                 ub2              **rcodepp _*/)   )
  (define-oci-function (oci-bind-dynamic OCIBindDynamic)
    int (void* void* void* callback callback))

  ;; sword OCIStmtGetPieceInfo( CONST OCIStmt  *stmtp,
  ;;                            OCIError       *errhp,
  ;;                            dvoid          **hndlpp,
  ;;                            ub4            *typep,
  ;;                            ub1            *in_outp,
  ;;                            ub4            *iterp, 
  ;;                            ub4            *idxp,
  ;;                            ub1            *piecep )
  (define-oci-function (oci-stmt-get-piece-info OCIStmtGetPieceInfo)
    int (void* void* void* void* void* void* void* void*))

  ;; sword OCIStmtSetPieceInfo ( dvoid             *hndlp,
  ;;                             ub4               type,
  ;;                             OCIError          *errhp,
  ;;                             CONST dvoid       *bufp,
  ;;                             ub4               *alenp, 
  ;;                             ub1               piece,
  ;;                             CONST dvoid       *indp, 
  ;;                             ub2               *rcodep )
  (define-oci-function (oci-stmt-set-piece-info OCIStmtSetPieceInfo)
    int (void* unsigned-int void* void* void* char void* void*))

  ;; sword OCIDefineByPos ( OCIStmt     *stmtp, 
  ;;                        OCIDefine   **defnpp,
  ;;                        OCIError    *errhp,
  ;;                        ub4         position,
  ;;                        dvoid       *valuep,
  ;;                        sb4         value_sz,
  ;;                        ub2         dty,
  ;;                        dvoid       *indp,
  ;;                        ub2         *rlenp,
  ;;                        ub2         *rcodep,
  ;;                        ub4         mode )
  (define-oci-function (oci-define-by-pos OCIDefineByPos)
    int (void* void* void* unsigned-int void* int 
	 unsigned-short void* void* void* unsigned-int))

  ;; not using this...
  ;; sword OCIDefineDynamic ( OCIDefine   *defnp,
  ;;                          OCIError    *errhp,
  ;;                          dvoid       *octxp, 
  ;;                          OCICallbackDefine       (ocbfp)(/*_
  ;;                                   dvoid          *octxp,
  ;;                                   OCIDefine      *defnp,
  ;;                                   ub4            iter, 
  ;;                                   dvoid          **bufpp,
  ;;                                   ub4            **alenpp,
  ;;                                   ub1            *piecep,
  ;;                                   dvoid          **indpp,
  ;;                                   ub2            **rcodep _*/)  )
  (define-oci-function (oci-define-dynamic OCIDefineDynamic)
    int (void* void* void* callback))

  ;; *LOB
  ;; sword OCIDescriptorAlloc ( CONST dvoid   *parenth,
  ;;                            dvoid         **descpp, 
  ;;                            ub4           type,
  ;;                            size_t        xtramem_sz,
  ;;                            dvoid         **usrmempp)
  (define-oci-function (oci-descriptor-alloc OCIDescriptorAlloc)
    int (void* void* unsigned-int size_t void*))

  ;; sword OCIDescriptorFree ( dvoid    *descp,
  ;;                           ub4      type ) 
  (define-oci-function (oci-descriptor-free OCIDescriptorFree)
    int (void* unsigned-int))

  ;; sword OCILobOpen ( OCISvcCtx        *svchp,
  ;;                    OCIError         *errhp, 
  ;;                    OCILobLocator    *locp, 
  ;;                    ub1              mode )
  (define-oci-function (oci-lob-open OCILobOpen)
    int (void* void* void* char))

  ;; sword OCILobClose ( OCISvcCtx      *svchp,
  ;;                     OCIError       *errhp, 
  ;;                     OCILobLocator  *locp )
  (define-oci-function (oci-lob-close OCILobClose) int (void* void* void*))

  ;; sword OCILobIsOpen ( OCISvcCtx        *svchp,
  ;;                      OCIError         *errhp, 
  ;;                      OCILobLocator    *locp, 
  ;;                      boolean          *flag )
  (define-oci-function (oci-lob-is-open OCILobIsOpen)
    int (void* void* void* void*))

  ;; sword OCILobFlushBuffer ( OCISvcCtx       *svchp, 
  ;;                           OCIError        *errhp, 
  ;;                           OCILobLocator   *locp
  ;;                           ub4             flag )
  (define-oci-function (oci-lob-flush-buffer OCILobFlushBuffer)
    int (void* void* void* unsigned-int))

  ;; sword OCILobCreateTemporary(OCISvcCtx          *svchp,
  ;;                             OCIError           *errhp,
  ;;                             OCILobLocator      *locp,
  ;;                             ub2                 csid,
  ;;                             ub1                 csfrm,
  ;;                             ub1                 lobtype,
  ;;                             boolean             cache,
  ;;                             OCIDuration         duration)
  ;; OCIDuration is ub2 defined in oro.h. Hope won't change!
  ;; why boolean? if the cache parameter requires actual value?
  (define-oci-function (oci-lob-create-temporary OCILobCreateTemporary)
    int (void* void* void* unsigned-short char char bool unsigned-short))

  ;; sword OCILobFreeTemporary( OCISvcCtx          *svchp,
  ;;                            OCIError           *errhp,
  ;;                            OCILobLocator      *locp);
  (define-oci-function (oci-lob-free-temporary OCILobFreeTemporary)
    int (void* void* void*))

  ;;  sword OCILobGetLength ( OCISvcCtx      *svchp,
  ;;                          OCIError       *errhp,
  ;;                          OCILobLocator  *locp,
  ;;                          ub4            *lenp )
  (define-oci-function (oci-lob-get-length OCILobGetLength)
    int (void* void* void* void*))

  ;; we don't use callback so declare it as void*
  ;; sword OCILobRead ( OCISvcCtx          *svchp,
  ;;                    OCIError           *errhp,
  ;;                    OCILobLocator      *locp,
  ;;                    ub4                *amtp,
  ;;                    ub4                offset,
  ;;                    dvoid              *bufp,
  ;;                    ub4                bufl,
  ;;                    dvoid              *ctxp, 
  ;;                    OCICallbackLobRead (cbfp)
  ;;                                       ( dvoid         *ctxp,
  ;;                                         CONST dvoid   *bufp,
  ;;                                         ub4           len,
  ;;                                         ub1           piece )
  ;;                    ub2                csid,
  ;;                    ub1                csfrm )
  ;; This is actually deprecated but using read2 is different from
  ;; this one so for now keep it.
  (define-oci-function (oci-lob-read OCILobRead)
    int (void* void* void* void* unsigned-int void* unsigned-int void*
	 void* unsigned-short char))

  ;; sword OCILobWrite2 ( OCISvcCtx       *svchp,
  ;;                      OCIError        *errhp,
  ;;                      OCILobLocator   *locp,
  ;;                      oraub8          *byte_amtp,
  ;;                      oraub8          *char_amtp,
  ;;                      oraub8          offset,
  ;;                      void            *bufp, 
  ;;                      oraub8          buflen,
  ;;                      ub1             piece,
  ;;                      void            *ctxp, 
  ;;                      OCICallbackLobWrite2 (cbfp)
  ;;                                      (
  ;;                                        void     *ctxp,
  ;;                                        void     *bufp,
  ;;                                        oraub8   *lenp,
  ;;                                        ub1      *piecep
  ;;                                        void     **changed_bufpp,
  ;;                                        oraub8   *changed_lenp
  ;;                                      ) 
  ;;                      ub2             csid,
  ;;                      ub1             csfrm )
  ;; OCILobWrite is deprecated so use write2
  (define-oci-function (oci-lob-write OCILobWrite2)
    int (void* void* void* void* void* uint64_t
	 void* uint64_t char void* void* unsigned-short char))
)
