#!/bin/bash

elm make Main.elm --output main.js

cp index.html main.js ../../elm-test-dist
cd ../../elm-test-dist
git add .
git commit -m "New build"
git push origin gh-pages
