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

-spec handle(state(), {join, string()}) -> state();
            (state(), {leave, string()}) -> state();
            (state(), {message_send, string(), string()}) -> state();
            (state(), {nick, string()}) -> state();
            (state(), whoami) -> state();
            (state(), {message_receive, string(), string(), string()}) -> state();
            (state(), quit) -> state();
            (state(), any()) -> state().


% Join channel
handle(#client_st{nick = Nick, server = Server} = State, {join, Channel}) ->
    NotReached = {reply, {error, server_not_reached, "Server not responding"}, State},

    % Inform server of channel creation or user joining
    case (catch genserver:request(Server, {join, Channel, Nick, self()})) of
        % Server process is down
        {'EXIT', _} -> NotReached;

        % Server not responding
        timeout_error -> NotReached;

        % User was already in the channel
        {error, user_already_joined, Msg} ->
            {reply, {error, user_already_joined, Msg}, State};

        % Everything went ok
        ok ->
            {reply, ok, State}
    end;

% Leave channel
handle(#client_st{nick = _, server = Server} = State, {leave, Channel}) ->
    % Inform server of leaving
    case catch(genserver:request(Server, {leave, Channel, self()})) of
        % Server timeout
        timeout_error ->
            {reply, {error, server_not_reached, "Server not responding"}, State};

        % Can't leave a channel we're not apart of
        {error, user_not_joined, _} ->
            {reply, {error, user_not_joined, "User wasn't in the channel"}, State};

        % Everything went ok
        _ ->
            {reply, ok, State}
    end;

% Sending message (from GUI, to channel)
handle(State, {message_send, Channel, Msg}) ->
    NotReached = {reply, {error, server_not_reached, "Channel not responding"}, State},

    case catch(genserver:request(list_to_atom(Channel), {message_send, Msg, State#client_st.nick, self()})) of
        % Channel process was down
        {'EXIT', _} -> NotReached;

        % Channel timeout
        timeout_error -> NotReached;

        % User wasn't in the channel
        {error, _, _} ->
            {reply, {error, user_not_joined, "User hasn't joined the channel yet"}, State};

        % Everything went ok
        ok ->
            {reply, ok, State}
    end;

% This case is only relevant for the distinction assignment!
% Change nick (no check, local only)
handle(#client_st{gui = GUI, server = Server, nick = Nick} = State, {nick, NewNick}) ->
    NotReached = {reply, {error, server_not_reached, "Server not responding"}, State},
    case catch(genserver:request(Server, {new_nick, Nick, NewNick})) of
        % Server process was down
        {'EXIT', _} -> NotReached;

        % Server timeout
        timeout -> NotReached;

        ok ->
            {reply, ok, State#client_st{gui = GUI, server = Server, nick = NewNick}};

        {error, nick_taken, Reason} ->
            {reply, {error, nick_taken, Reason}, State}
    end;

% ---------------------------------------------------------------------------
% The cases below do not need to be changed...
% But you should understand how they work!

% Get current nick
handle(State, whoami) ->
    {reply, State#client_st.nick, State};

% Incoming message (from channel, to GUI)
handle(State = #client_st{gui = GUI}, {message_receive, Channel, Nick, Msg}) ->
    gen_server:call(GUI, {message_receive, Channel, Nick ++ "> " ++ Msg}),
    {reply, ok, State};

% Quit client via GUI
handle(State, quit) ->
    % Any cleanup should happen here, but this is optional
    {reply, ok, State};

% Catch-all for any unhandled requests
handle(_State, _) ->
    {'EXIT', "Fatal error: unknown command"}.
