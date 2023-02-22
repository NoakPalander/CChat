-module(server).
-export([start/1, stop/1]).

% The server state contains a list of all channels
% These channels are created in the channel module as processes
-record(server_state, {channels = [], nicks = []}).

% Convenient type declaration for the state
-type state() :: #server_state{}.


% Start a new server process with the given name
% Do not change the signature of this function.
-spec start(atom()) -> pid().
start(ServerAtom) ->
    % - Spawn a new process which waits for a message, handles it, then loops infinitely
    % - Register this process to ServerAtom
    % - Return the process ID
    genserver:start(ServerAtom, #server_state{}, fun handle/2).

% Stop the server process registered to the given name,
% together with any other associated processes
-spec stop(atom()) -> ok.
stop(ServerAtom) ->
    % Stops all channels
    catch(genserver:request(ServerAtom, close_all)),

    % Stop the server
    genserver:stop(ServerAtom).


% Typespecs for the handles
-spec handle(state(), {new_nick, string(), string()}) -> state();
            (state(), {join, string(), string(), pid()}) -> state();
            (state(), {leave, string(), string(), pid()}) -> state();
            (state(), close_all) -> state();
            (state(), any()) -> state().

% Handles new nicknames
handle(#server_state{channels = Channels, nicks = Nicks} = State, {new_nick, Nick, New}) ->
    % Checks if the new nickname is already taken
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
            NotReached = {reply, {error, server_not_reached, "Channel not responding"}, State},

            % Try to join it
            case catch(genserver:request(Channel, {join, From, Name})) of
                % Channel process was down
                {'EXIT', _} -> NotReached;

                % Channel timeout
                timeout_error -> NotReached;

                % User was already in the channel
                user_already_joined ->
                    {reply, {error, user_already_joined, "User already joined"},
                        #server_state{channels = Channels, nicks = NewNicks}};

                % User wasn't in the channel
                _ ->
                    {reply, ok, #server_state{channels = Channels, nicks = NewNicks}}
            end
    end;

% Handles a client's request to leave a channel
handle(State, {leave, ChannelName, From}) ->
    % Looks for a channel in our state, can't leave a non-existing channel
    case lists:keyfind(ChannelName, 1, State#server_state.channels) of
        % No channel was found
        false ->
            {reply, {error, user_not_joined, "User not joined"}, State};

        % Channel found
        {_, Channel} ->
            NotReached = {reply, {error, server_not_reached, "Channel not responding"}, State},

            % Try to leave it
            case catch(genserver:request(Channel, {leave, From})) of
                % Channel process down
                {'EXIT', _} ->  NotReached;

                % Channel timeout
                timeout_error -> NotReached;

                % User wasn't in the channel
                user_not_joined ->
                    {reply, {error, user_not_joined, "User not joined"}, State};

                % User was in the channel
                _ ->
                    {reply, ok, State}
            end
    end;

% Handles channel closing
handle(State, close_all) ->
    % Asynchronously close all channels
    lists:foreach(fun ({_Name, Channel}) ->
        % Close the given channel
        catch(genserver:request(Channel, {close, Channel}))
    end, State#server_state.channels),

    {reply, ok, State};

% Wildcard match
handle(_State, _) ->
    {'EXIT', "Fatal error: unknown command"}.
