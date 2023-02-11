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
    NewState = case lists:keyfind(ChannelName, 1, State#server_state.channels) of
        % Wasn't found
        false ->
            % Create a new channel and join it
            Channel = channel:create(ChannelName, {From, Name}),

            % Add the channel to our state
            add_channel(State, {ChannelName, Channel});

        % Channel was found
        {_, Channel} ->
            % Join it, if we're already joined this is effectively a no-op
            % The server state isn't altered
            genserver:request(Channel, {join, From, Name}),
            State
    end,

    io:format("[Debug/Server]: New state: ~p~n", [NewState]),

    % Return a reply with the updated state
    % TODO: This "some reply" is kinda weird, idk what to return
    {reply, "some reply", NewState};

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
    % TODO Implement function
    % Return ok
    io:format("[Debug/Server]: Stopping server!~n"),
    genserver:stop(ServerAtom).
