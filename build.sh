#!/bin/sh

mkdir -p build
elm make src/Main.elm --output=build/Main.js
cp src/index.html build
