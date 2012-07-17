{application, ej2j,
 [
  {description, "Erlang Jabber-to-Jabber transport"},
  {vsn, "0.0.4"},
  {id, "ej2j"},
  {modules, [ej2j, ej2j_comp, ej2j_helper, ej2j_route]},
  {registered, [ej2j, ej2j_comp, ej2j_route]},
  {mod, {ej2j, []}},
  {env, []},
  {applications, [kernel, stdlib, public_key, ssl, exmpp]}
 ]
}.
