-module(client).
-export([handle/2, initial_state/3]).

% This record defines the structure of the state of a client.
% Add whatever other fields you need.
-record(client_st, {
    gui :: atom(),
    nick :: string(),
    server :: atom()
}).

-type state() :: #client_st{}.

% Return an initial state record. This is called from GUI.
% Do not change the signature of this function.
-spec initial_state(string(), atom(), atom()) -> state().
initial_state(Nick, GUIAtom, ServerAtom) ->
    #client_st{
        gui = GUIAtom,
        nick = Nick,
        server = ServerAtom
    }.

% Join channel
handle(St, {join, Channel}) ->
    io:format("[Debug/Client]: Join requested (~p)~n", [Channel]),

    % Inform server of channel creation / joining
    case genserver:request(St#client_st.server, {join, Channel, St#client_st.nick, self()}) of
        % Everything went ok
        ok -> {reply, ok, St};

        % User was already joined
        Reply -> {reply, Reply, St}
    end;

% Leave channel
handle(St, {leave, Channel}) ->
    io:format("[Debug/Client]: Leave requested (~p)~n", [Channel]),

    % Inform server of leaving
    case genserver:request(St#client_st.server, {leave, Channel, St#client_st.nick, self()}) of
        % Everything went ok
        ok -> {reply, ok, St};

        % User wasn't joined
        Reply -> {reply, Reply, St}
    end;

% Sending message (from GUI, to channel)
handle(St, {message_send, Channel, Msg}) ->
    io:format("[Debug/Client]: Message send requested (~p), to channel: ~p~n", [Msg, Channel]),

    genserver:request(St#client_st.server, {message_send, Channel, Msg, St#client_st.nick, self()}),
    {reply, ok, St};

% This case is only relevant for the distinction assignment!
% Change nick (no check, local only)
handle(St, {nick, NewNick}) ->
    {reply, ok, St#client_st{nick = NewNick}};

% ---------------------------------------------------------------------------
% The cases below do not need to be changed...
% But you should understand how they work!

% Get current nick
handle(St, whoami) ->
    {reply, St#client_st.nick, St};

% Incoming message (from channel, to GUI)
handle(St = #client_st{gui = GUI}, {message_receive, Channel, Nick, Msg}) ->
    gen_server:call(GUI, {message_receive, Channel, Nick ++ "> " ++ Msg}),
    {reply, ok, St};

% Quit client via GUI
handle(St, quit) ->
    % Any cleanup should happen here, but this is optional
    {reply, ok, St};

% Catch-all for any unhandled requests
handle(St, Data) ->
    io:format("[Debug]: Unhandled request (~p)~n", [Data]),
    {reply, {error, not_implemented, "Client does not handle this command"}, St}.
