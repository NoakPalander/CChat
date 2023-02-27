-module(server).
-export([start/1, stop/1]).

% The server state contains a list of all channels
% These channels are created in the channel module as processes
-record(server_state, {channels = []}).

add_channel(#server_state{channels = Channels}, Channel) ->
    State = #server_state{channels = [Channel | Channels]},
    io:format("[Debug/Server]: Adding channel, new state ~p~n", [State]),
    State.

% Handles a client's request to join a channel
handle(State, {join, ChannelName, Name, From}) ->
    io:format("[Debug/Server]: Join channel requested~n"),


    % Looks for a channel in our state, matching the join-commands name
    case lists:keyfind(ChannelName, 1, State#server_state.channels) of
        % Wasn't found
        
        false ->
            % Create a new channel and join it
            Channel = channel:create(ChannelName, {From, Name}),

            % Add the channel to our state
            NewState = add_channel(State, {ChannelName, Channel}),
            {reply, ok, NewState};

        % Channel was found
        {_, Channel} ->
            % Join it
            case catch(genserver:request(Channel, {join, From, Name})) of
                {'EXIT', _} ->
                    {reply, {error, server_not_reached, "Server not responding"}, State};
                user_already_joined ->
                    {reply, {error, user_already_joined, "User already joined"}, State};

                timeout_error ->
                    {reply, {error, server_not_reached, "Server not responding"}, State};

                _ ->
                    {reply, ok, State}
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
                timeout_error ->
                    {reply, {error, server_not_reached, "Server not responding"}, State};

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
