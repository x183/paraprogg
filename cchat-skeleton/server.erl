-module(server).
-export([start/1,stop/1]).
-import(genserver,[start/3,request/2,request/3]).
-import(channel,[channel/1]).

% Start a new server process with the given name
% Do not change the signature of this function.
start(ServerAtom) ->
    % TODO Implement function
    % - Spawn a new process which waits for a message, handles it, then loops infinitely
    % - Register this process to ServerAtom
    % - Return the process ID
    Pid = genserver:start(ServerAtom,[],fun handleMsg/1),
    Pid.

    
handleMsg(Channels) ->
    receive 
        {join, Channel, From, Nick} -> 
            case lists:member(Channel,Channels) of
                false -> 
                    genserver:start(Channel, [], fun channel:channel/1),
                    register(Nick, From),
                    genserver:request(Channel, {join, Nick}),
                    genserver:request(self(), {reply, [Channel|Channels]});
                true -> 
                    genserver:request(Channel, {join, Nick}),
                    genserver:request(self(), {reply, [Channels]})
            end;
            %handleMsg([{Channel, Nick} | ChannelNameList]);
            
        {leave, Channel, Nick} -> 
            % handleMsg( [X || X <- ChannelNameList, X  =/={Channel,Nick}]);
            case lists:member(Channel, Channels) of
                true -> 
                    genserver:request(Channel,{leave, Nick}),
                    genserver:request(self(), {reply, [Channels]});
                false -> ok
            end;

        % Something in here is wacky, but I don't know what
        {message_send, Channel, Nick, Msg} -> 
            case lists:member(Channel, Channels) of
                true -> 
                    genserver:request(Channel, {message_send, Nick, Msg}),
                    genserver:request(self(), {reply, [Channels]}); 
                false -> ok     
            end;          
        stop -> 
            Channels
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
    Channels = genserver:request(ServerAtom,{stop}),
    lists:foreach(fun(Channel) -> genserver:stop(Channel) end, Channels),
    genserver:stop(ServerAtom),
    ok.

    %genserver:stop(ServerAtom),
    %ok.