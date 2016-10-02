PROJECT = emqttd_dashboard
PROJECT_DESCRIPTION = emqttd Web Dashboard
PROJECT_VERSION = 2.0

DEPS = lager gen_conf
dep_lager    = git https://github.com/basho/lager
dep_gen_conf = git https://github.com/emqtt/gen_conf

LOCAL_DEPS = mnesia

BUILD_DEPS = emqttd
dep_emqttd = git https://github.com/emqtt/emqttd master

ERLC_OPTS += +'{parse_transform, lager_transform}'

COVER = true

include erlang.mk

app:: rebar.config

