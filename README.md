Egress
======

A minimalistic tool for managing SQL databases schema migrations.
Egress allows you to migrate you database schema upward and downwards.
Currently supports only PostgreSQL and Sqlite, migrations are expressed as plain SQL files.

## Installation

    git clone git@github.com:afiore/egress.git
    cabal clean && cabal configure && cabal install
    
The program executable will be placed in `dist/build/egress`

## Usage

   Usage: egress [options] [version|up|rollback]

   Options:
      -v n                      --schema-version=n                     Target schema version
      -m ./migrations-dir       --migration-dir=./migrations-dir       Path to the migrations folder
      -d ./dbs/example.sqlite3  --db-connection=./dbs/example.sqlite3  DB connection string
      -D sqlite|postgres        --driver=sqlite|postgres               HDBC Adapter (i.e. sqlite, postgres)
      -V                        --verbose                              Verbose mode
      -s                        --silent                               Silent mode
      -h                        --help                                 Show help
