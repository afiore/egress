Egress
======

A simple command line tool for managing SQL schema migrations.
Migrations are stored on the file system, and are expressed as plain old SQL.
Currently supports only PostgreSQL and Sqlite (via HDBC).

## Installation

    git clone git@github.com:afiore/egress.git && cd egress
    cabal clean && cabal configure && cabal install

The program executable will be placed in `dist/build/egress`

## Usage

    Usage: egress [options] [version|set-version|up|rollback]

    Options:
       -v n                      --schema-version=n                     Target schema version
       -m ./migrations-dir       --migration-dir=./migrations-dir       Path to the migrations folder
       -d ./dbs/example.sqlite3  --db-connection=./dbs/example.sqlite3  DB connection string
       -D sqlite|postgres        --driver=sqlite|postgres               HDBC Adapter (i.e. sqlite, postgres)
       -V                        --verbose                              Verbose mode
       -s                        --silent                               Silent mode
       -h                        --help                                 Show help
