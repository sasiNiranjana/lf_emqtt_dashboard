PROJECT = emq_dashboard
PROJECT_DESCRIPTION = EMQ Dashboard
PROJECT_VERSION = 3.0

LOCAL_DEPS = mnesia

BUILD_DEPS = emqttd
dep_emqttd = git https://github.com/emqtt/emqttd emq30

TEST_DEPS = cuttlefish
dep_cuttlefish = git https://github.com/emqtt/cuttlefish

ERLC_OPTS += +'{parse_transform, lager_transform}'

COVER = true

include erlang.mk

app:: rebar.config

app.config::
	cuttlefish -l info -e etc/ -c etc/emq_dashboard.conf -i priv/emq_dashboard.schema -d data
