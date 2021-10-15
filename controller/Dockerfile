FROM swipl AS saved-state
COPY *.pl /build/
COPY sources /build/sources/
COPY assets /build/assets/

ENV http_port 80
ENV base_url ''

WORKDIR /build

# install custom packages, e.g. 
# RUN swipl -g  "pack_install(prolog_sax, [interactive(false), upgrade(true), url('https://github.com/milung/prolog_sax/archive/v1.0.3.zip'), inquiry(false)])" -t halt

RUN swipl -o bootfile -c run.pl

FROM swipl

COPY --from=saved-state /build/assets /app/assets/
COPY --from=saved-state /build/bootfile /app/bootfile

WORKDIR /app
# for cli mode you may need to set folowing workdir 
# WORKDIR /app/workdir
ENTRYPOINT ["swipl", "-x", "/app/bootfile"]

# remove if only cli is requested
CMD ["--server"]