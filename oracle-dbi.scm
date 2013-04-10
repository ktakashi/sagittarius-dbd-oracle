(add-load-path ".")
(import (rnrs) (dbi) (sagittarius object))

(define conn (dbi-connect "dbi:oracle:database=XE"
			  :username "my_schema"
			  :password "password"))

(define delete-blob "delete from blob_test where id = ?")

(define delete-stmt (dbi-prepare conn delete-blob))
(dbi-execute! delete-stmt 1)
(dbi-commit! conn)

(define insert-blob "insert into blob_test (id, data) values (?, ?)")

(define data (open-file-input-port "test.scm"))

(define insert-stmt (dbi-prepare conn insert-blob))
(dbi-execute! insert-stmt 1 data)
(close-port data)

(define update-blob "update blob_test set data = ? where id = ?")

(define update-data (open-file-input-port "oracle.scm"))

(define update-stmt (dbi-prepare conn update-blob))
(dbi-execute! update-stmt update-data 1)

;;(dbi-rollback! conn)
(dbi-commit! conn)
(close-port update-data)

(define select "select * from blob_test")
(define stmt (dbi-prepare conn select))
(let ((count (dbi-execute! stmt)))
  (let ((row (dbi-fetch! stmt)))
    (display (vector-ref row 0))
    (let ((port (transcoded-port (vector-ref row 1) (native-transcoder))))
      (display (get-string-all port)))
    ))



(dbi-close conn)
