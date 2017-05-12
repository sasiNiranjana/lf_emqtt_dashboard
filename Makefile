PROJECT = emq_dashboard
PROJECT_DESCRIPTION = EMQ Web Dashboard
PROJECT_VERSION = 2.2

LOCAL_DEPS = mnesia

BUILD_DEPS = emqttd cuttlefish clique
dep_emqttd = git https://github.com/emqtt/emqttd emq22
dep_cuttlefish = git https://github.com/emqtt/cuttlefish
dep_clique = git https://github.com/turtleDeng/clique

NO_AUTOPATCH = cuttlefish

ERLC_OPTS += +'{parse_transform, lager_transform}'

COVER = true

include erlang.mk

app:: rebar.config

app.config::
	./deps/cuttlefish/cuttlefish -l info -e etc/ -c etc/emq_dashboard.conf -i priv/emq_dashboard.schema -d data
