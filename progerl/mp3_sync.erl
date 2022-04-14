-module(mp3_sync).
-export([find_sync/2]).
-export([unpack_image_resource_directory/1]).
-export([unpack_ipv4/1]).

-define(DWORD, 32/unsigned-little-integer).
-define(LONG,  32/unsigned-little-integer).
-define(WORD,  16/unsigned-little-integer).
-define(BYTE,  8/unsigned-little-integer).

-define(IP_VERSION, 4).
-define(IP_MIN_HDR_LEN, 5).

unpack_ipv4(Dgram) ->
   DgramSize = byte_size(Dgram),
   case Dgram of
      <<?IP_VERSION:4, HLen:4, SrvcType:8, TotLen:16,
        ID:16, Flags:3, FragOff:13,
        TTL:8, Proto:8, HdrChkSum:16,
        SrcIP:32,
        DestIP:32, RestDgram/binary>> when HLen >= 5, 4*HLen =< DgramSize ->
         OptsLen = 4*(HLen - ?IP_MIN_HDR_LEN),
         <<Opts:OptsLen/binary,Data/binary>> = RestDgram;
      _Any ->
         exit(invalidDgram)
   end.

find_sync(Bin, N) ->
   case is_header(N, Bin) of
      {ok, Len1, _} ->
         case is_header(N + Len1, Bin) of
            {ok, Len2, _} ->
               case is_header(N + Len1 + Len2, Bin) of
                  {ok, _, _} ->
                     {ok, N};
                  error ->
                     find_sync(Bin, N+1)
               end;
            error ->
               find_sync(Bin, N+1)
         end;
      error ->
         find_sync(Bin, N+1)
   end.

is_header(N, Bin) ->
   unpack_header(get_word(N, Bin)).

get_word(N, Bin) ->
   {_, <<C:4/binary,_/binary>>} = split_binary(Bin, N),
   C.

unpack_header(X) ->
   try decode_header(X)
   catch
      _:_ -> error
   end.

decode_header(<<2#11111111111:11,B:2,C:2,_D:1,E:4,F:2,G:1,Bits:9>>) ->
   Vsn = case B of
            0 -> {2,5};
            1 -> exit(badVsn);
            2 -> 2;
            3 -> 1
         end,
   Layer = case C of
            0 -> exit(badLayer);
            1 -> 3;
            2 -> 2;
            3 -> 1
         end,
   %% Protection = D,
   BitRate = bitrate(Vsn, Layer, E) * 1000,
   SampleRate = samplerate(Vsn, F),
   Padding = G,
   FrameLength = framelength(Layer, BitRate, SampleRate, Padding),
   if
      FrameLength < 21 ->
         exit(frameSize);
      true ->
         {ok, FrameLength, {Layer, BitRate, SampleRate, Vsn, Bits}}
   end;
decode_header(_) ->
   exit(badHeader).


bitrate(_,_,15) -> exit(1);
bitrate(1,1,E) ->		      
    element(E+1, {free,32,64,96,128,160,192,224,256,288,
		  320,352,384,416,448});
bitrate(1,2,E) ->
    element(E+1, {free,32,48,56,64,80,96,112,128,160,
		  192,224,256,320,384});
bitrate(1,3,E) ->
    element(E+1, {free,32,40,48,56,64,80,96,112,128,160,192,
		  224,256,320});
bitrate(2,1,E) ->
    element(E+1, {free,32,48,56,64,80,96,112,128,144,160,
		  176,192,224,256});
bitrate(2,2,E) ->
    element(E+1, {free,8,16,24,32,40,48,56,64,80,96,112,
		  128,144,160});
bitrate(2,3,E) -> bitrate(2,2,E);
bitrate({2,5}, L, E) -> bitrate(2, L, E).
    
%% samplerate Vsn F
samplerate(1, 0) -> 44100;
samplerate(1, 1) -> 48000;
samplerate(1, 2) -> 32000;
samplerate(2, 0) -> 22050;
samplerate(2, 1) -> 24000;
samplerate(2, 2) -> 16000;
samplerate({2,5}, 0) -> 11025;
samplerate({2,5}, 1) -> 12000;
samplerate({2,5}, 2) -> 8000.

framelength(1, BitRate, SampleRate, Padding) ->
    ((12*BitRate div SampleRate) + Padding) * 4;
framelength(_, BitRate, SampleRate, Padding) ->
    (144 * BitRate div SampleRate) + Padding.
    

%% typedef struct _IMAGE_RESOURCE_DIRECTORY {
%%    DWORD Characteristics;
%%    DWORD TimeDateStamp;
%%    WORD MajorVersion;
%%    WORD MinorVersion;
%%    WORD NumberOfNamedEntries;
%%    WORD NumberOfIdEntries;
%% } IMAGE_RESOURCE_DIRECTORY, *PIMAGE_RESOURCE_DIRECTORY;

unpack_image_resource_directory(Dir) ->
   <<Characteristics : ?DWORD,
     TimeDateStamp   : ?DWORD,
     MajorVersion    : ?WORD,
     MinorVersion    : ?WORD,
     NumberOfNamedEntries  : ?WORD,
     NumberOfIdEntries     : ?WORD, _/binary>> = Dir,
   {Characteristics, TimeDateStamp, MajorVersion, MinorVersion,
    NumberOfNamedEntries, NumberOfIdEntries}.
