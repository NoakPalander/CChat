-module(channel).
-export([create/2, delete/1]).

% Type declaration for members
-type member() :: {pid(), string()}.

% Holds our channel's state
-record(channel_state, {
    name :: string(),
    members :: list(member())
}).

% Type declaration for the state record, just syntax sugar
-type state() :: #channel_state{}.


% Returns an initial state, containing one member
-spec init_state(string(), member()) -> state().
init_state(ChannelName, User) ->
    State = #channel_state{name = ChannelName, members = [User]},
    io:format("[Debug/Channel]: Initial state ~p~n", [State]),
    State.


% Creates a channel process and adds the callee to the members list
% Returns the Pid of the channel
-spec create(string(), member()) -> pid().
create(ChannelName, User) ->
    io:format("[Debug/Channel]: Creating a channel '~p'~n", [ChannelName]),

    % Registers the channel name as a process and runs the loop
    genserver:start(list_to_atom(ChannelName), init_state(ChannelName, User), fun handle/2).


% Deletes the channel
-spec delete(pid() | atom()) -> atom().
delete(Channel) ->
    genserver:stop(Channel).


% Adds a member and returns a new state
-spec add_member(state(), member()) -> state().
add_member(#channel_state{name = Name, members = Members}, Member) ->
    #channel_state{name = Name, members = [Member | Members]}.


% Drops a member and returns a new state
-spec drop_member(state(), member()) -> state().
drop_member(#channel_state{name = Name, members = Members}, Member) ->
     #channel_state{name = Name, members = lists:delete(Member, Members)}.


% Handles user joining
handle(State, {join, From, Name}) ->
    io:format("[Debug/Channel]: User joined ~p~n", [Name]),

    Members = State#channel_state.members,
    Reply = case lists:keyfind(From, 1, Members) of
        % Already joined, do not need to update state
        {From, Name} -> {reply, user_already_joined, State};

        % Add user to the members list
        false -> {reply, ok, add_member(State, {From, Name})}
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
        Member -> {reply, ok, drop_member(State, Member)}
    end;

handle(#channel_state{name = Channel, members = Members} = State, {message_send, Msg, Name, From}) ->
    % Verifies so the user is a member of the channel
    case lists:keyfind(From, 1, Members) of
        % Member not found
        false ->
            {reply, {error, user_not_joined, "User isn't in the channel"}, State};

        Author ->
            Recipients = lists:delete(Author, Members),

            % Send message to all members asynchronously
            lists:foreach(fun ({Member, _}) ->
                spawn(fun () ->
                    genserver:request(Member, {message_receive, Channel, Name, Msg})
                end)
            end, Recipients),

            {reply, ok, State}
    end;

    %{reply, ok, State};

handle(State, {close, Channel, _From}) ->
    channel:delete(Channel),
    {reply, ok, State};

handle(State, _) ->
    {reply, {error, invalid_command, "Unknown handle"}, State}.
