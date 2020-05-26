FROM codesimple/elm:0.19 AS build
WORKDIR /build
COPY . .
RUN elm make src/Main.elm --output=website/js/elm-chessboard.js

FROM nginx:alpine
COPY --from=build /build/website /usr/share/nginx/html
COPY --from=build /build/src/assets /usr/share/nginx/html/assets