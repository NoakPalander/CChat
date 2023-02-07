-module(channel).
-export([create/2, delete/1]).

% Contains a list of members, members are identified as a tuple of Name's and their Pid's
-record(channel_state, {members = []}).

init_state(User) ->
    #channel_state{members = [User]}.

% TODO: Implement this
handle() ->
    undefined.

% Creates a channel process and adds the callee to the members list
% Returns the Pid of the channel
create(ChannelName, User) ->
    io:format("[Debug/Channel]: Creating a channel '~p'~n", [ChannelName]),

    % Registers the channel name as a process and runs the loop
    % register(list_to_atom(Name), spawn(fun () -> loop(InitState) end)).
    genserver:start(list_to_atom(ChannelName), init_state(User), fun handle/0).

delete(Channel) ->
    genserver:stop(Channel).

% Joins a channel
join() ->
    undefined.

% Leaves a channel
leave() ->
    undefined.
