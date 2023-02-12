-module(channel).
-export([create/2, delete/1]).

% Contains a list of members, members are identified as a tuple {Pid, Name}
-record(channel_state, {members = []}).

% Returns an initial state, containing one member
init_state(User) ->
    State = #channel_state{members = [User]},
    io:format("[Debug/Channel]: Initial state ~p~n", [State]),
    State.

% Handles user joining
handle(State, {join, From, Name}) ->
    io:format("[Debug/Channel]: User joined ~p~n", [Name]),

    Members = State#channel_state.members,
    Reply = case lists:keyfind(From, 1, Members) of
        % Already joined, do not need to update state
        {From, Name} -> {reply, user_already_joined, State};

        % Add user to the members list
        false -> {reply, ok, #channel_state{members = [{From, Name} | Members]}}
    end,

    %io:format("[Debug/Channel]: New state ~p~n", [NewState]),
    Reply;

% Handles user leaving
handle(State, {leave, From, Name}) ->
    io:format("[Debug/Channel]: User left ~p~n", [Name]),

    Members = State#channel_state.members,
    case lists:keyfind(From, 1, Members) of
        % Member wasn't found, no-op
        false -> {reply, user_not_joined, State};

        % Member found, we can now drop it from the members list
        Member -> {reply, ok, #channel_state{members = lists:delete(Member, Members)}}
    end;

handle(_State, _) ->
    undefined.

% Creates a channel process and adds the callee to the members list
% Returns the Pid of the channel
-spec create(string(), pid()) -> pid().
create(ChannelName, User) ->
    io:format("[Debug/Channel]: Creating a channel '~p'~n", [ChannelName]),

    % Registers the channel name as a process and runs the loop
    % register(list_to_atom(Name), spawn(fun () -> loop(InitState) end)).
    genserver:start(list_to_atom(ChannelName), init_state(User), fun handle/2).

delete(Channel) ->
    genserver:stop(Channel).
