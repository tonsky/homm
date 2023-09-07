#!/bin/bash
set -o errexit -o nounset -o pipefail
cd "`dirname $0`/.."

echo "Starting socket REPL at localhost:5555"
clj $@ -X:dev clojure.core.server/start-server :name repl :port 5555 :accept clojure.core.server/repl :server-daemon false
