#!/usr/bin/env bash

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

find "${SCRIPT_DIR}/pages/" "${SCRIPT_DIR}/blog/" -type f -name '*.md' -print0 |
    xargs -0 grep --no-filename -i '^tags:' | # find all the tags
    sed 's|^tags: ||g' | sed 's|, | |g' | # strip non-tags
    tr ' ' '\n' | tr '[:blank:]+' '\n' | # replace spaces w/ newlines
    sort | grep -Ev '^\s*$' |
    uniq -c | sort -rn
