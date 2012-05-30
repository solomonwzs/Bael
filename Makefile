JQ_UI_VER=1.8.20

PREFIX:=../
DEST:=$(PREFIX)$(PROJECT)

REBAR=./rebar

all:
	@$(REBAR) get-deps compile

edoc:
	@$(REBAR) doc

test:
	@rm -rf .eunit
	@mkdir -p .eunit
	@$(REBAR) skip_deps=true eunit

clean:
	@$(REBAR) clean
	-rm -rf ./deps/*
	-rm ./priv/www/resoure/js/jquery/jquery.min.js
	-rm -rf ./priv/www/resoure/js/JSON-js
	-rm -rf ./priv/www/resoure/js/jquery-ui/

build_plt:
	@$(REBAR) build-plt

dialyzer:
	@$(REBAR) dialyze

app:
	@$(REBAR) create template=mochiwebapp dest=$(DEST) appid=$(PROJECT)

js:
	@wget -nc http://code.jquery.com/jquery.min.js \
		-P ./priv/www/resoure/js/jquery/
	-git clone git://github.com/douglascrockford/JSON-js.git \
		./priv/www/resoure/js/JSON-js
	@wget -nc http://jqueryui.com/download/jquery-ui-$(JQ_UI_VER).custom.zip\
		-P ./priv/www/resoure/js/jquery-ui/
	@unzip ./priv/www/resoure/js/jquery-ui/jquery-ui-$(JQ_UI_VER).custom.zip\
		-d ./priv/www/resoure/js/jquery-ui/
	@rm ./priv/www/resoure/js/jquery-ui/jquery-ui-$(JQ_UI_VER).custom.zip

rebuild:clean js
	@$(REBAR) get-deps compile
