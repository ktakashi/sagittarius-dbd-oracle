(add-load-path ".")
(import (rnrs) (oracle) (sagittarius object))

(define env (create-oracle-env))
(define conn (connect-database env "XE" "my_schema" "password"
			       :auto-commit #t))

(define delete-blob "delete from blob_test where id = :0")

(define delete-stmt (create-statement conn delete-blob))
(execute-query delete-stmt 1)
(commit conn)

(define insert-blob "insert into blob_test (id, data) values (:0, :data)")

(define data (open-file-input-port "test.scm"))

(define insert-stmt (create-statement conn insert-blob))
(bind-parameter insert-stmt 1 1)
(bind-parameter insert-stmt :data data)
(execute-query insert-stmt)
(commit conn)
(close-port data)

(define update-blob "update blob_test set data = :0 where id = :1")

(define update-data (open-file-input-port "oracle.scm"))

(define update-stmt (create-statement conn update-blob))
(execute-query update-stmt update-data 1)

(commit conn)
(close-port update-data)

(release-statement delete-stmt)
(release-statement insert-stmt)
(release-statement update-stmt)

(define select "select * from blob_test")
(define stmt (create-statement conn select))
(let ((cursor (execute-query stmt)))
  (let ((row (fetch-row cursor)))
    (display (vector-ref row 0))
    (let ((port (transcoded-port (vector-ref row 1) (native-transcoder))))
      (display (get-string-all port)))
    ))

(release-statement stmt)
(disconnect-database conn #t)