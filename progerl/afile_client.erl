-module(afile_client).
-export([ls/1, get_file/2, put_file/3]).

ls(Server) ->
   Server ! {self(), list_dir},
   receive
      {_ServerPid, FileList} ->
         FileList
   end.

get_file(Server, File) ->
   Server ! {self(), {get_file, File}},
   receive
      {_ServerPid, Content} ->
         Content
   end.

put_file(Server, File, Content) ->
   Server ! {self(), {put_file, File, Content}},
   receive
      {_ServerPid, Answer} ->
         Answer
   end.
