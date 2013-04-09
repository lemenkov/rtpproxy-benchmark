all:
	rebar compile -v

test:
	rebar eunit -v

run:
	erl -pa ebin -config ./priv/rtpplay.config
