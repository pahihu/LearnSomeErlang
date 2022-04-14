-module(mybits).
-export([reverse_binary/1]).
-export([term_to_packet/1, packet_to_term/1]).
-export([reverse_bits/1]).
-export([test/0]).

term_to_packet(Term) ->
   BinTerm = term_to_binary(Term),
   N = byte_size(BinTerm),
   <<N:32, BinTerm/binary>>.

packet_to_term(Packet) ->
   <<N:32, BinTerm:N/binary>> = Packet,
   binary_to_term(BinTerm).

reverse_binary(Bin) ->
   list_to_binary(lists:reverse(binary_to_list(Bin))).

reverse_bits(BitStr) ->
   L = lists:reverse([ X || <<X:1>> <= BitStr ]),
   << <<X:1>> || X <- L >>.

test() ->
   <<"ahpla">> = reverse_binary(<<"alpha">>),
   <<>> = reverse_binary(<<>>),
   Packet1 = term_to_packet({42,alpha,"hello"}),
   <<N1:32, _BinTerm1:N1/binary>> = Packet1,
   {42,alpha,"hello"} = packet_to_term(Packet1),
   <<128,0,0:1>> = reverse_bits(<<1:17>>),
   <<1:1,1:1,1:1,1:1,1:1,0:1,1:1,0:1>> = reverse_bits(<<16#5f>>),
   ok.


