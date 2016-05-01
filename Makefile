PROJECT = emqttd_dashboard
PROJECT_DESCRIPTION = emqttd dashboard
PROJECT_VERSION = 1.1

DEPS = emqttd
LOCAL_DEPS = mnesia

dep_emqttd = git https://github.com/emqtt/emqttd plus

COVER = true

include erlang.mk

app:: rebar.config
