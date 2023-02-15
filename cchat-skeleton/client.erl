-module(client).
-export([handle/2, initial_state/3]).
-import(genserver,[start/3,request/2,request/3]).

% This record defines the structure of the state of a client.
% Add whatever other fields you need.
-record(client_st, {
    gui, % atom of the GUI process
    nick, % nick/username of the client
    server % atom of the chat server
}).

% Return an initial state record. This is called from GUI.
% Do not change the signature of this function.
initial_state(Nick, GUIAtom, ServerAtom) ->
    #client_st{
        gui = GUIAtom,
        nick = Nick,
        server = ServerAtom
    }.

% handle/2 handles each kind of request from GUI
% Parameters:
%   - the current state of the client (St)
%   - request data from GUI
% Must return a tuple {reply, Data, NewState}, where:
%   - Data is what is sent to GUI, either the atom `ok` or a tuple {error, Atom, "Error message"}
%   - NewState is the updated state of the client

% Join channel
handle(St, {join, Channel}) ->
    case catch genserver:request(St#client_st.server, {join, Channel,self(), St#client_st.nick}) of 
        ok -> 
            {reply, ok, St};
        user_already_joined ->
            {reply,{error, user_already_joined, "You have already joined this channel"},St};
        _ ->
            {reply, {error, server_not_reached, "Server not reached"}, St}
    end;
    %St#client_st.server ! {join, Channel, self(), St#client_st.nick},
    %{reply, {error, not_implemented, "join not implemented"}, St} ;

% Leave channel
handle(St, {leave, Channel}) ->
    case catch genserver:request(list_to_atom(Channel), {leave, self()}) of 
        ok -> 
            {reply, ok, St};
        user_not_joined -> 
            {reply, {error, user_not_joined, "You have not joined this channel"}, St};
        _ ->
            {reply, {error, server_not_reached, "Server not reached"}, St}
    end;

% Sending message (from GUI, to channel)
handle(St, {message_send, Channel, Msg}) ->
    case catch genserver:request(list_to_atom(Channel), {message_send, Channel, St#client_st.nick, Msg, self()}) of
        ok ->
            {reply, ok, St} ;
        user_not_joined ->
            {reply,{error, user_not_joined, "You haven't joined that channel yet"},St};
        _ ->
            {reply, {error, server_not_reached, "Server not reached"}, St}
    end;
% This case is only relevant for the distinction assignment!
% Change nick (no check, local only)
handle(St, {nick, NewNick}) ->
    case catch genserver:request(St#client_st.server,{new_nick,NewNick,St#client_st.nick}) of
        ok ->
            {reply, ok, St#client_st{nick = NewNick}} ;
        nick_taken ->
            {reply,{error,nick_taken,"That nick is already in use"},St};
        _ -> {reply, {error, server_not_reached, "Server not reached"}, St}
    end;
% ---------------------------------------------------------------------------
% The cases below do not need to be changed...
% But you should understand how they work!

% Get current nick
handle(St, whoami) ->
    {reply, St#client_st.nick, St} ;

% Incoming message (from channel, to GUI)
handle(St = #client_st{gui = GUI}, {message_receive, Channel, Nick, Msg}) ->
    gen_server:call(GUI, {message_receive, Channel, Nick++"> "++Msg}),
    {reply, ok, St} ;

% Quit client via GUI
handle(St, quit) ->
    % Any cleanup should happen here, but this is optional
    {reply, ok, St} ;

% Catch-all for any unhandled requests
handle(St, Data) ->
    {reply, {error, not_implemented, "Client does not handle this command"}, St} .
