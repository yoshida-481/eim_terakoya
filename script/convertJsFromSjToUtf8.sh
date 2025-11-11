#!/bin/sh

find . -maxdepth 1 -name "*.js" -type f -exec bash -c 'echo {} && iconv -f cp932 -t utf8 {} > {}.new && mv {}.new {}' \;