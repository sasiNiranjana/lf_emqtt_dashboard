PROJECT = emq_dashboard
PROJECT_DESCRIPTION = Web Dashboard for EMQ 3.0
PROJECT_VERSION = 3.0

DEPS = lager
dep_lager  = git https://github.com/basho/lager

LOCAL_DEPS = mnesia

BUILD_DEPS = emqttd cuttlefish
dep_emqttd = git https://github.com/emqtt/emqttd emq30
dep_cuttlefish = git https://github.com/basho/cuttlefish master

ERLC_OPTS += +'{parse_transform, lager_transform}'

COVER = true

include erlang.mk

app:: rebar.config

app.config::
	cuttlefish -l info -e etc/ -c etc/emq_dashboard.conf -i priv/emq_dashboard.schema -d data
