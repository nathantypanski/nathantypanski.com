#!/bin/bash

# don't do git checkouts - not sure I want this anymore
# # git checkout master &&\
# # git push gh master &&\
./bin/site clean &&\
./bin/site build &&\
rsync --checksum -ave 'ssh' \_site/* nathantypanski.com:/srv/http/nathantypanski.com
