-module(server).
-export([start/1,stop/1]).
-import(genserver,[start/3,stop/1,request/2,request/3]).

% Start a new server process with the given name
% Do not change the signature of this function.
start(ServerAtom) ->
    % TODO Implement function
    % - Spawn a new process which waits for a message, handles it, then loops infinitely
    % - Register this process to ServerAtom
    % - Return the process ID
    Pid = genserver:start(ServerAtom,[],handleMsg),
    Pid.


    
handleMsg(ChannelNameList) ->
    not_implemented.

%    receive 
%        {join, Channel, GUI, From, Nick} -> 
%            register(Nick, From),
%            handleMsg([{Channel, Nick} | ChannelNameList]);
%            
%        {leave, Channel, GUI, Nick, Msg} -> 
%            handleMsg( [X || X <- ChannelNameList, X  =/={Channel,Nick}]);
%            
%        % Something in here is wacky, but I don't know what
%        {message_send, Channel, GUI, Nick, Msg} -> 
%            %GUI ! {message_receive, Channel, Nick, Msg},
%            foreach(fun({_,Receiver}) -> Receiver ! {message_receive, Channel, Nick, Msg} end, [X || X <- ChannelNameList, X  =={Channel,_}]),
%            handleMsg(ChannelNameList);
%    end.

channel(State) -> 
    not_implemented.

% Channels ska fortsätta även om servern krashar, kan vara värt att skapa dem separat
% Skicka meddelanden till client, vilken i sin tur skickar vidare till TUI (terminal, yay! 😀)
% Använd genserver istället för att skicka messages
% Kolla på testerna, de är specifika

% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
    % TODO Implement function
    % Return ok
    not_implemented.
    %genserver:stop(ServerAtom),
    %ok.
