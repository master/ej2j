Erlang Jabber-to-Jabber Transport is an implementation of an XMPP-to-XMPP [gateway](http://xmpp.org/extensions/xep-0100.html) with an [external component] (http://xmpp.org/extensions/xep-0114.html) interface.

INSTALL
=======
Configure XMPP server to listen for a component connection with shared secret:

``` erlang
          {listen, [
          ...
          {8888, ejabberd_service, [
                 {access, all},
                 {shaper_rule, fast},
                 {ip, {127, 0, 0, 1}},
                 {hosts, ["j2j.your.domain"], [{password, "s3cr3t"}]}
                 ]},
          ...
          }
```

Edit ej2j.config to match your configuration:

``` erlang
          [{ej2j, [
                 {component, "j2j.your.domain"},
                 {component_name, "J2J Transport"},
                 {server_host, "localhost"},
                 {server_port, 8888},
                 {server_secret, "s3cr3t"}
          ]}].
```

USAGE
=====
Start the J2J transport:

          $ ./ej2j.sh
