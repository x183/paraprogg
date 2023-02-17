-module(server).
-export([start/1, stop/1]).
-import(genserver, [start/3, request/2, request/3]).

% Start a new server process with the given name
% Do not change the signature of this function.
start(ServerAtom) ->
    % TODO Implement function
    % - Spawn a new process which waits for a message, handles it, then loops infinitely
    % - Register this process to ServerAtom
    % - Return the process ID
    genserver:start(ServerAtom, {[], []}, fun handle/2).

% Tries to add a user to a specific channel
handle({ChannelState, NickState}, {join, Channel, From, Nick}) ->
    % Checks
    case lists:member(Nick, [N || {N, F} <- NickState, F =/= From]) of
        false ->
            NewNickState = [{Nick, From} | NickState];
        true ->
            NewNickState = NickState
    end,
    case lists:member(Channel, ChannelState) of
        true ->
            Result = genserver:request(list_to_atom(Channel), {join, From}),
            {reply, Result, {ChannelState, NewNickState}};
        false ->
            genserver:start(list_to_atom(Channel), [From], fun channel/2),
            {reply, ok, {[Channel | ChannelState], NewNickState}}
    end;
% Stops all channels connected to this server
handle({ChannelState, NickState}, stop_channels) ->
    lists:foreach(fun(Channel) -> genserver:stop(list_to_atom(Channel)) end, ChannelState),
    {reply, ok, {[], NickState}};
% Checks if a user can change its nick
handle({ChannelState, NickState}, {new_nick, NewNick, Nick}) ->
    % Compares The users new nick to all other nicks
    case lists:member(NewNick, [N || {N, _} <- NickState]) of
        true ->
            % If the nick is already used by somebody, hinder the change
            {reply, nick_taken, {ChannelState, NickState}};
        false ->
            % If the nick is unused, change the nick of the user
            {reply, ok,
                {ChannelState, [
                    [{NewNick, F} || {N, F} <- NickState, N == Nick]
                    | [{N, F} || {N, F} <- NickState, N =/= Nick]
                ]}}
    end.

% Our channel server loop function
channel(State, {join, From}) ->
    case lists:member(From, State) of
        true ->
            {reply, user_already_joined, State};
        false ->
            {reply, ok, [From | State]}
    end;
channel(State, {leave, From}) ->
    case lists:member(From, State) of
        true ->
            NewState = lists:delete(From, State),
            {reply, ok, NewState};
        false ->
            {reply, user_not_joined, State}
    end;
channel(State, {message_send, Channel, Nick, Msg, From}) ->
    case lists:member(From, State) of
        true ->
            spawn(fun() ->
                lists:foreach(
                    fun(User) ->
                        if
                            User == From -> skip;
                            true -> genserver:request(User, {message_receive, Channel, Nick, Msg})
                        end
                    end,
                    State
                )
            end),
            {reply, ok, State};
        false ->
            {reply, user_not_joined, State}
    end.
% Channels ska fortsätta även om servern krashar, kan vara värt att skapa dem separat
% Skicka meddelanden till client, vilken i sin tur skickar vidare till TUI (terminal, yay! 😀)
% Använd genserver istället för att skicka messages
% Kolla på testerna, de är specifika

% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
    % Stop all connected channels,
    % Stop server,
    % return ok.
    genserver:request(ServerAtom, stop_channels),
    genserver:stop(ServerAtom),
    ok.

%genserver:stop(ServerAtom),
    %ok.
