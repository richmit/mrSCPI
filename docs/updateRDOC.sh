#!/bin/sh

cd ../

rdoc.bat -o docs/rdoc --verbose --title=mrSCPI --no-coverage-report --main mrSCPI.rdoc src/mrSCPI.rb src/mrSCPI.rdoc

