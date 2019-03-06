# Build stage 0
FROM erlang:alpine

# Install Rebar3
RUN apk add --no-cache git
RUN mkdir -p /buildroot/rebar3/bin
ADD https://s3.amazonaws.com/rebar3/rebar3 /buildroot/rebar3/bin/rebar3
RUN chmod a+x /buildroot/rebar3/bin/rebar3

# Setup Environment
ENV PATH=/buildroot/rebar3/bin:$PATH

# Reset working directory
WORKDIR /buildroot

# Copy our Erlang test application
COPY . piraha

# And build the release
WORKDIR piraha
RUN rebar3 as prod release


# Build stage 1
FROM alpine

# Install some libs
RUN apk add --no-cache openssl && \
    apk add --no-cache ncurses-libs

# Install the released application
COPY --from=0 /buildroot/piraha/_build/prod/rel/piraha /piraha

# Expose relevant ports
EXPOSE 5060/udp

CMD ["/piraha/bin/piraha", "foreground"]
