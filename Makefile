PROJECT = emqttd_dashboard
PROJECT_DESCRIPTION = emqttd dashboard
PROJECT_VERSION = 1.1.2

DEPS = emqttd lager
LOCAL_DEPS = mnesia

dep_emqttd = git https://github.com/emqtt/emqttd plus

ERLC_OPTS += +'{parse_transform, lager_transform}'

COVER = true

include erlang.mk

app:: rebar.config
