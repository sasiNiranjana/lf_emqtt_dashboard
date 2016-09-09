
emqttd_dashboard
================

Dashboard for emqttd broker.

Build
------

make && make ct

Configurtion
------------

```
{listener,
  {dashboard, 18083, [
    {acceptors, 4},
    {max_clients, 512}
  ]}
}.
```

Load Plugin
-----------

```
./bin/emqttd_ctl plugins load emqttd_dashboard
```

Login
-----

URL: http://host:18083

Username: admin

Password: public

