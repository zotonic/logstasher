# logstasher
[![Hex.pm Version][hexpm version]][hexpm]
[![Hex.pm Downloads][hexpm downloads]][hexpm]
[![Hex.pm Documentation][hexdocs documentation]][hexdocs]
[![Build Status][gh badge]][gh]
[![Erlang Versions][erlang version badge]][gh]

Erlang Logger formatter for logstash

<!-- Badges -->
[hexpm]: https://hex.pm/packages/logstasher
[hexpm version]: https://img.shields.io/hexpm/v/logstasher.svg?style=flat-square
[hexpm downloads]: https://img.shields.io/hexpm/dt/logstasher.svg?style=flat-square
[hexdocs documentation]: https://img.shields.io/badge/hex-docs-purple.svg?style=flat-square
[hexdocs]: https://hexdocs.pm/logstasher
[gh]: https://github.com/zotonic/logstasher/actions/workflows/ci.yml
[gh badge]: https://img.shields.io/github/workflow/status/zotonic/logstasher/CI?style=flat-square
[erlang version badge]: https://img.shields.io/badge/erlang-22.0%20to%2024.2.1-blue.svg?style=flat-square


## Hex package

In `rebar.config`, add the logstasher Hex package to the deps:

```erlang
{deps, [
    {logstasher, "~> 1.0.0"}
]}.

```

## Erlang Configuration

In `sys.config`, enable `logstasher_h` as a Logger handler and configure the `logstasher`
application:


```erlang
[
     {kernel, [
         {logger, [
             {handler, logstash, logstasher_h,
                 #{
                     level => info
                 }
             }
         ]}
     ]},

     {logstasher, [
         {transport, udp},     % tcp | udp | console
         {host, "localhost"},  % inet:hostname()
         {port, 5000}          % inet:port_number()
     ]}
 ].
 ```

 After this, also add the `logstasher` application to your `.app.src` file:

 ```erlang
{applications, [
    ....
    logstasher,
    ....
]},
```

## Logstash Configuration

```ruby
input {
  udp {
    codec => json
    port => 5000
    queue_size => 10000
    workers => 10
    type => default_log_type
  }
}
output {
  stdout {}
  elasticsearch {
    protocol => http
  }
}
```

## Send data to logstash

It is possible to send other data to logstash:

```erlang
logstasher:send_message(<<"Hello world!">>, #{ some => <<"fields">> }).
```

A timestamp will be added to the message.
