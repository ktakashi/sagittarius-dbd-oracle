# OCI binding for Sagittarius

If you are a commercial programmer, then you probably can't avoid to use Oracle
unless you are extremely lucky.

This library provides Oracle database access procedures for DBI on Sagittarius
Scheme.

# How to use (DBI)

Following example shows the simple step to use.

    (import (dbi))
    (define conn (dbi-connect "dbi:oracle:database=XE;host=localhost"))
    (define sql "select * from foo")
    (let* ((stmt (dbi-prepare conn sql))
           (q    (dbi-execute-query! stmt)))
       (do ((row (dbi-fetch! q) (dbi-fetch! q)))
           ((not row))
         (print row)))

The DSN can accept the following options;

- database  (required)
  - Oracle SID name or TNS name (if the following options are not supplied)
- host (optional)
  - Database host name
- port (optional)
  - Port number (default 1521)
- protocol (optional)
  - Protocol (default TCP)

If the _host_ option is passed to the DSN, then DBD uses full connection
string to connect database.

The rest of DBI interface please look up the Sagittarius manual.

# Raw level APIs

TODO
