-module(server).
-export([start/1, stop/1]).

-record(server_state, {channels = []}).

% Handles a client's request to join a channel
handle(State, {join, Channel}) ->
    io:format("[Debug/Server]: Join channel requested."),

    % TODO: Only add channel if it's not already existing
    NewState = [Channel | State#server_state.channels],
    io:format("[Debug/Server]: New state: ~p~n", NewState),

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
