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
handle(#client_st{nick = Nick, server = Server} = St, {join, Channel}) ->
    io:format("[Debug/Client]: Join requested (~p)~n", [Channel]),

    % Inform server of channel creation or user joining
    case (catch genserver:request(Server, {join, Channel, Nick, self()})) of
        % Server process is down
        {'EXIT', _} ->
            {reply, {error, server_not_reached, "Server not responding"}, St};

        % User was already in the channel
        {error, user_already_joined, Msg} ->
            {reply, {error, user_already_joined, Msg}, St};

        % Everything went ok
        ok ->
            {reply, ok, St}
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
    io:format("Channel: ~p~n", [Channel]),

    case catch(genserver:request(list_to_atom(Channel), {message_send, Msg, St#client_st.nick, self()})) of
        % Channel process was down
        {'EXIT', _} ->
            {reply, {error, server_not_reached, "Channel not responding"}, St};

        % User wasn't in the channel
        {error, _, _} ->
            {reply, {error, user_not_joined, "User hasn't joined the channel yet"}, St};

        % Everything went ok
        ok ->
            {reply, ok, St}
    end;

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
