-module(server).
-export([start/1, stop/1]).

% The server state contains a list of all channels
% These channels are created in the channel module as processes
-record(server_state, {channels = [], nicks = []}).

add_channel(#server_state{channels = Channels, nicks = Nicks}, Channel) ->
    State = #server_state{channels = [Channel | Channels], nicks = Nicks},
    io:format("[Debug/Server]: Adding channel, new state ~p~n", [State]),
    State.

% Handles new nicknames
handle(#server_state{channels = Channels, nicks = Nicks} = State, {new_nick, Nick, New}) ->
    io:format("[Debug/Server]: Nick swap requested from ~p to ~p~n", [Nick, New]),
    %io:format("[Debug/Server]: Current nicks ~p~n", [Nicks])

    case lists:member(New, Nicks) of
        % Nick wasn't taken
        false ->
            % Removes the current nickname from the list and adds the new one
            NewNicks = [New | lists:delete(Nick, Nicks)],
            {reply, ok, #server_state{channels = Channels, nicks = NewNicks}};

        % Nick already was taken
        true ->
            {reply, {error, nick_taken, "Nick was already taken"}, State}
    end;

% Handles a client's request to join a channel
handle(#server_state{channels = Channels, nicks = Nicks} = State, {join, ChannelName, Name, From}) ->
    io:format("[Debug/Server]: Join channel requested~n"),

    % Adds the nick to the list if it's not already present
    NewNicks = case lists:member(Name, State#server_state.nicks) of
        % Already exists
        true -> Nicks;

        % Nick hasn't been registered
        false -> [Name | Nicks]
    end,

    % Looks for a channel in our state, matching the join-commands name
    case lists:keyfind(ChannelName, 1, State#server_state.channels) of
        % Wasn't found
        false ->
            % Create a new channel and join it
            Channel = channel:create(ChannelName, {From, Name}),

            % Add the channel to our state
            {reply, ok, #server_state{channels = [{ChannelName, Channel} | Channels], nicks = NewNicks}};

        % Channel was found
        {_, Channel} ->
            % Join it
            case genserver:request(Channel, {join, From, Name}) of
                user_already_joined ->
                    {reply, {error, user_already_joined, "User already joined"},
                        #server_state{channels = Channels, nicks = NewNicks}};

                _ ->
                    {reply, ok, #server_state{channels = Channels, nicks = NewNicks}}
            end
    end;

% Handles a client's request to leave a channel
handle(State, {leave, ChannelName, Name, From}) ->
    io:format("[Debug/Server]: Leave channel requested~n"),

    % Looks for a channel in our state, can't leave a non-existing channel
    case lists:keyfind(ChannelName, 1, State#server_state.channels) of
        % No channel was found
        false ->
            {reply, {error, user_not_joined, "User not joined"}, State};

        % Channel found
        {_, Channel} ->
            % Leave it
            case genserver:request(Channel, {leave, From, Name}) of
                user_not_joined ->
                    {reply, {error, user_not_joined, "User not joined"}, State};

                _ ->
                    {reply, ok, State}
            end
    end;

% Handles channel closing
handle(State, {close_all, _From})   ->
    io:format("[Debug/Server]: Closing all channels~n"),

    lists:foreach(fun ({_, Channel}) ->
        io:format("[Debug/Server]: Closing channel: ~p~n", [Channel]),

        genserver:request(Channel, {close, Channel, self()})
    end, State#server_state.channels),

    {reply, ok, State};

handle(_, _) ->
    {'EXIT', "Fatal error: unknown command"}.

% Start a new server process with the given name
% Do not change the signature of this function.
start(ServerAtom) ->
    % - Spawn a new process which waits for a message, handles it, then loops infinitely
    % - Register this process to ServerAtom
    % - Return the process ID
    io:format("[Debug/Server]: Starting server!~n"),
    genserver:start(ServerAtom, #server_state{}, fun handle/2).

% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
    io:format("[Debug/Server]: Stopping server!~n"),

    % Stops all channels
    genserver:request(ServerAtom, {close_all, self()}),
    genserver:stop(ServerAtom).
