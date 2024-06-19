#! /bin/bash

# elm make src/Main.elm --output=elm.js
# sleep 1
# elm make src/second.elm --output=second.js


elm make src/Main.elm --output=public/main.js
# elm make src/Main.elm --output=elm.js
# elm make src/Second.elm --output=second.js

