set -xe

elm-package install -y

elm-make src/Main.elm --output=./website/js/elm-chessboard.js

cp -r src/assets website
