#!/bin/sh

git archive \
    --format=zip -o sphs-dictionary-explorer.zip \
    --prefix=data/ --add-file=./data/dictionary.rds --prefix= \
    HEAD \
    data R app.r r-packages.txt
