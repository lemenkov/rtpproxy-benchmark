-module(rtpplay).
-behaviour(gen_server).

-include_lib("rtplib/include/rtp.hrl").

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([Type, CallID, {TagF, TagT}, SSRC, RtppIp, RtppPort, MainIp]) ->
	process_flag(trap_exit, true),
	{ok, Socket} = gen_udp:open(0, [{active, false}, binary, {ip, MainIp}]),
	{ok, {SrcIp, SrcPort}} = inet:sockname(Socket),
	SIP = list_to_binary(inet_parse:ntoa(SrcIp)),
	SPort = list_to_binary(io_lib:format("~b", [SrcPort])),
	% Send RTPPROXY command and get DestIp and DestPort (use callid as cookie)
	case Type of
		a ->
			gen_udp:send(Socket, RtppIp, RtppPort, <<CallID/binary, " Uswc0,8,18,101 ", CallID/binary, " ", SIP/binary, " ", SPort/binary, " ", TagF/binary, ";1">>);
		b ->
			gen_udp:send(Socket, RtppIp, RtppPort, <<CallID/binary, " Lswc0,8,18,101 ", CallID/binary, " ", SIP/binary, " ", SPort/binary, " ", TagF/binary, ";1", " ", TagT/binary, ";1">>)
	end,
	{ok, {_, _, Reply}} = gen_udp:recv(Socket, 0, 500),
	[_, P, I] = binary_split(<< <<X>> || <<X>> <= Reply, X /= 0, X /= $\n>>, $ ),
	{ok, DestIp} = inet_parse:address(binary_to_list(I)),
	DestPort = list_to_integer(binary_to_list(P)),
	% Set socket mode back to active
	inet:setopts(Socket, [{active, true}, binary]),
	{ok, TRef} = timer:send_interval(20, generate),
	error_logger:info_msg("STARTED:: ~p~n", [{{SSRC, 1, 1}, DestIp, DestPort, Socket, TRef}]),
	{ok, {{SSRC, 1, 1}, DestIp, DestPort, Socket, TRef}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(generate, {{SSRC, Marker, Seq}, DestIp, DestPort, Socket, TRef} = State) ->
%	Rtp = #rtp{marker = Marker, payload_type = 18, sequence_number = Seq, timestamp = 100, ssrc = SSRC, payload = <<"!hello from rtpplay!">>},
%	gen_udp:send(Socket, DestIp, DestPort, rtp:encode(Rtp)),
%	gen_udp:send(Socket, DestIp, DestPort, <<128,224,68,171,0,5,25,205,0,5,109,113,<<"!hello from rtpplay!">>),
	gen_udp:send(Socket, DestIp, DestPort, <<128,18,0,100,0,0,0,100,113,142,194,241,33,104,101,108,108,111,32,102,114,111,109,32,114,116,112,112,108,97,121,33>>),
	{noreply, {{SSRC, 0, Seq + 1}, DestIp, DestPort, Socket, TRef}};

handle_info(Info, State) ->
    {noreply, State}.

terminate(_Reason, {_, _, _, Socket, TRef} = State) ->
	timer:cancel(TRef),
	gen_udp:close(Socket),
	ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

binary_split(Binary, Val) when is_binary(Binary) ->
	binary_split(<<>>, Binary, Val, []).
binary_split(Head, <<>>, _Val, Result) ->
	lists:reverse([Head | Result]);
binary_split(Head, <<Val:8, Rest/binary>>, Val, Result) ->
	binary_split(<<>>, Rest, Val, [Head | Result]);
binary_split(Head, <<OtherVal:8, Rest/binary>>, Val, Result) ->
	binary_split(<<Head/binary, OtherVal:8>>, Rest, Val, Result).

