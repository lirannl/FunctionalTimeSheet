FROM fpco/stack-build  as server_compilation
RUN mkdir -p /opt/build
COPY ./Server /opt/build
WORKDIR /opt/build
RUN stack install

FROM wunsh/alpine-elm as client_compilation
RUN mkdir -p /opt/build
COPY ./Client /opt/build
WORKDIR /opt/build
RUN elm make src/Main.elm

FROM ubuntu as prod
RUN mkdir -p /opt/App
WORKDIR /opt/App
COPY --from=client_compilation /opt/build/index.html .
COPY --from=server_compilation /root/.local/bin/exe /opt/App/server
RUN chmod +x ./server
CMD bash -c "cd /opt/App; stat .env > /dev/null 2> /dev/null || touch .env && DEFAULT_PAGE=index.html ./server"
