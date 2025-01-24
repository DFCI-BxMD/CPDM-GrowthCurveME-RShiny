#!/bin/bash
set -e -x -o pipefail

main() {
    R -e "shiny::runApp('~/Webapp_source', host='0.0.0.0', port=443)"
}
