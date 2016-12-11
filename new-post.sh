#!/bin/sh

# Help me write blog posts by creating postnames for me.

# Yay Matt J at http://stackoverflow.com/a/192266 for the getopts
# parsing stuff.

# Reset in case getopts has been used previously in the shell.

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

OPTIND=1

postname=""
title=""
tags=""
edit_file='false'
blog_dir="blog"
verbose=0

usage() {
    echo "Usage: $0 -p <postname> -t <title> [-e]" 1>&2
    echo "Make a new blog post with <postname>" 1>&2
}

while getopts "h?p:e?t:" opt; do
    case "$opt" in
    h|\?)
        usage
        exit 1
        ;;
    t)
        title="${OPTARG}"
        ;;
    e)
        edit_file=true
        ;;
    p)  postname="${OPTARG}"
        ;;
    esac
done

shift $((OPTIND-1))
[ "$1" = "--" ] && shift

if [[ -z "${postname}" ]]; then
    >&2 echo "You must supply a post name with -p."
    exit 1
fi
if [[ -z "${title}" ]]; then
    >&2 echo "You must supply a title with -t."
    exit 1
fi

post_path="${blog_dir}/$(date --rfc-3339=date)-${postname}.md"
if [[ -f "${post_path}" ]]; then
    echo "${post_path} already exists!"
    exit 1
else
    touch "${post_path}"
    if [[ -n "${tags}" ]]; then
        cat <<EOF > "${post_path}"
----
title: ${title}
tags: ${tags}
----
EOF
    else
        cat <<EOF > "${post_path}"
----
title: ${title}
----
EOF
    fi
    if [[ "${edit_file}" -eq 'true' ]]; then
        echo "editing file"
        "${EDITOR}" "${post_path}"
    fi
fi
exit 0

### while getopts "h?:p:" o; do
###     case "${o}" in
###         s)
###             s=${OPTARG}
###             ((s == 45 || s == 90)) || usage
###             ;;
###         p)
###             p=${OPTARG}
###             ;;
###         *)
###             usage
###             ;;
###     esac
### done
### shift $((OPTIND-1))
### 
### if [ -z "${s}" ] || [ -z "${p}" ]; then
###     usage
### fi
