-module(server).
-export([start/1, stop/1]).

% The server state contains a list of all channels
% These channels are created in the channel module as processes
-record(server_state, {channels = []}).

add_channel(#server_state{channels = Channels}, Channel) ->
    #server_state{channels = [Channel | Channels]}.

% Handles a client's request to join a channel
% TODO: Only create a channel if it doesnt exist, otherwise join it.
handle(State, {join, ChannelName, Name, From}) ->
    io:format("[Debug/Server]: Join channel requested.~n"),

    % Creating a new channel
    Channel = channel:create(ChannelName, {Name, From}),

    % Updates the state
    NewState = add_channel(State, Channel),

    %NewState = #server_state{channels = Channels},
    io:format("[Debug/Server]: New state: ~p~n", [NewState]),

    % Return a reply with the updated state
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
