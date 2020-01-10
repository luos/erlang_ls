-module(els_text_synchronization).

-include("erlang_ls.hrl").

-export([ did_open/1
        , did_save/1
        , did_close/1
        ]).

-export([ generate_diagnostics/1 ]).

-spec did_open(map()) -> ok.
did_open(Params) ->
  TextDocument = maps:get(<<"textDocument">>, Params),
  Uri          = maps:get(<<"uri">>         , TextDocument),
  Text         = maps:get(<<"text">>        , TextDocument),
  Document     = els_dt_document:new(Uri, Text),
  ok           = els_indexer:index(Document),
  spawn (?MODULE, generate_diagnostics, [Uri]),
  ok.

-spec did_save(map()) -> ok.
did_save(Params) ->
  TextDocument = maps:get(<<"textDocument">>, Params),
  Uri          = maps:get(<<"uri">>         , TextDocument),
  spawn (?MODULE, generate_diagnostics, [Uri]),
  ok.

-spec did_close(map()) -> ok.
did_close(_Params) -> ok. 

%%==============================================================================
%% Internal functions  
%%==============================================================================

-spec generate_diagnostics(uri()) -> ok.
generate_diagnostics(Uri) -> %hi
  lager:error("Generatin diagnostics"),
  Method = <<"textDocument/publishDiagnostics">>,
  try
    CDiagnostics = els_compiler_diagnostics:diagnostics(Uri),
    DDiagnostics = els_dialyzer_diagnostics:diagnostics(Uri),
    EDiagnostics = els_elvis_diagnostics:diagnostics(Uri),
    GDiagnostics = els_gradualizer_diagnostics:diagnostics(Uri),
    #{ uri => Uri
                , diagnostics => CDiagnostics ++ DDiagnostics ++ EDiagnostics ++ GDiagnostics
                }
  of 
    AllDiagnostics -> els_server:send_notification(Method, AllDiagnostics)
  catch
  _:_ = Error -> 
    lager:error("Error while generating diagnostifcs: ~p for file ~p", [Error, Uri])
  end.
 