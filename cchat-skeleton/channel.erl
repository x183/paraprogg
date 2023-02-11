-module(channel).
-export([channel/1]).

channel(Members) ->
    receive
        {join, Nick} ->
            channel([Nick|Members]),
				ok;
        {leave, Nick} ->
            channel([N || N <- Members, N =/= Nick]),
				ok;
        {message_send, Nick, Msg} ->
				%map(fun(Member) -> Member ! {message_receive, Channel, Nick, Msg} end, Members),
                map(fun(Member) -> genserver:request(Member, {message_receive, self(), Nick, Msg}) end, Members),
				channel(Members),
				ok;
			stop ->
				true
	end.

map(F, [H|T]) -> [F(H)|map(F, T)];
map(_, [])    -> [].