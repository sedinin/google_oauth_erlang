PROJECT = google_oauth
PROJECT_VERSION = $(shell head -n 1 relx.config | awk '{split($$0, a, "\""); print a[2]}')

# app:: rebar.config
LOCAL_DEPS = inets crypto public_key ssl

DEPS = lager jsx base64url

dep_lager = git https://github.com/erlang-lager/lager 3.8.0
dep_jsx = git https://github.com/talentdeficit/jsx.git v2.10.0
dep_base64url = git https://github.com/dvv/base64url.git 1.0.1

include erlang.mk

ERLC_COMPILE_OPTS= +'{parse_transform, lager_transform}'
ERLC_OPTS += $(ERLC_COMPILE_OPTS)
TEST_ERLC_OPTS += $(ERLC_COMPILE_OPTS)
