-module(channel).
-export([create/2, delete/1]).

% Type declaration for members
-type member() :: {pid(), string()}.

% Holds our channel's state
-record(channel_state, {
    name :: string(),
    members :: list(member())
}).

% Type declaration for the state record, just as syntax sugar
-type state() :: #channel_state{}.

% Returns an initial state, containing one member
-spec init_state(string(), member()) -> state().
init_state(ChannelName, User) ->
    #channel_state{name = ChannelName, members = [User]}.

% Creates a channel process and adds the callee to the members list
% Returns the Pid of the channel
-spec create(string(), member()) -> pid().
create(ChannelName, User) ->
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

-spec handle(state(), {join, pid(), string()}) -> state();
            (state(), {leave, pid(), string()}) -> state();
            (state(), {message_send, string(), string(), pid()}) -> state();
            (state(), {close, pid()}) -> state();
            (state(), any()) -> state().

% Handles user joining
handle(State, {join, From, Name}) ->
    Members = State#channel_state.members,
    case lists:keyfind(From, 1, Members) of
        % Already joined, do not need to update state
        {From, Name} ->
            {reply, user_already_joined, State};

        % User wasn't joined, add the user to the members list
        false ->
            {reply, ok, add_member(State, {From, Name})}
    end;

% Handles user leaving
handle(State, {leave, From}) ->
    Members = State#channel_state.members,
    case lists:keyfind(From, 1, Members) of
        % Member wasn't found, no-op
        false -> {reply, user_not_joined, State};

        % Member found, we can now drop it from the members list
        Member -> {reply, ok, drop_member(State, Member)}
    end;

% Handles message sending, more specifically it asynchronously passes along messages to all recipients
handle(#channel_state{name = Channel, members = Members} = State, {message_send, Msg, Name, From}) ->
    % Verifies so the user is a member of the channel
    case lists:keyfind(From, 1, Members) of
        % Member not found
        false ->
            {reply, {error, user_not_joined, "User isn't in the channel"}, State};

        % Message author
        Author ->
            % Retrieves the recipients
            Recipients = lists:delete(Author, Members),

            % Send message to all recipients, asynchronously
            lists:foreach(fun ({Member, _}) ->
                spawn(fun () ->
                    catch(genserver:request(Member, {message_receive, Channel, Name, Msg}))
                end)
            end, Recipients),

            {reply, ok, State}
    end;

% Closes the channel
handle(State, {close, Channel}) ->
    channel:delete(Channel),
    {reply, ok, State};

% Wildcard match
handle(_State, _) ->
    {'EXIT', "Fatal error: unknown command"}.
