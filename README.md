# Play NSI support

Library that supports using Network Service Interface (NSI) v2.0 in Scala and Play.

More specifically it contains case classes for many NSI messages. Also
it includes SOAP handling using the Play framework.

## Tests

`sbt test` needs `GITHUB_TOKEN` to resolve `bod-nsi` from GitHub Packages, and a PostgreSQL
database for `MessageStoreSpec`:

    psql -d postgres -c "CREATE USER \"play-nsi-support-user\" PASSWORD 'password'"
    createdb -O play-nsi-support-user play-nsi-support-test

The spec recreates its tables on every run.
