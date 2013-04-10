-module(rtpplay_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
	error_logger:tty(false),
	{ok, PoolSize} = application:get_env(rtpplay, poolsize),
	{ok, {RtppIp, RtppPort}} = application:get_env(rtpplay, rtppaddr),
	{ok, MainIp} = application:get_env(rtpplay, mainip),
	{ok, Ref} = supervisor:start_link({local, rtpplay_sup}, rtpplay_sup, []),
	[
		begin
				SSRC_A = random:uniform(16#ffffffff),
				SSRC_B = random:uniform(16#ffffffff),
				CallID =  list_to_binary(io_lib:format("cid-~b-~b@callid-erlrtpplay", [SSRC_A, SSRC_B])),
				TagA = <<"A-tag-erlrtpplay">>,
				TagB = <<"B-tag-erlrtpplay">>,
				supervisor:start_child(rtpplay_sup,
					{{a, CallID}, {gen_server, start_link, [rtpplay, [a, CallID, {TagA, TagB}, SSRC_A, RtppIp, RtppPort, MainIp], []]}, temporary, 5000, worker, [rtpplay]}),
				supervisor:start_child(rtpplay_sup,
					{{b, CallID}, {gen_server, start_link, [rtpplay, [b, CallID, {TagA, TagB}, SSRC_B, RtppIp, RtppPort, MainIp], []]}, temporary, 5000, worker, [rtpplay]}),
				ok
		end
		|| _X <- lists:seq(1, PoolSize)
	],
	{ok, Ref}.

stop(_State) ->
    ok.
