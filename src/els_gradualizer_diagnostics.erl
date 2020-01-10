-module(els_gradualizer_diagnostics).

%%==============================================================================
%% Behaviours
%%==============================================================================

-behaviour(els_diagnostics).

-export([ diagnostics/1 , source/0]). 

-include("erlang_ls.hrl").


-spec source() -> binary().
source() ->
  <<"Gradualizer">>.

-spec diagnostics(uri()) -> [diagnostic()].
diagnostics(Uri) ->
  try
  IncludeDirs = els_config:get(include_dirs),
  Errors = gradualizer:type_check_file(
    erlang:binary_to_list(els_uri:path(Uri)), [return_errors, {i, IncludeDirs}]),
  lager:info("Errors: ~p", [Errors]),
  [to_error(File, E) || {File,E} <- Errors] 
  of 
    Es ->  
      lager:info("Diagnostics result: ~p", [Es]),
      Es
  catch 
    _:_ = E -> 
      lager:error("Gradualizer error ~p", [E]),
      [] 
    end. 

 -spec to_error(any(), any()) -> any().
to_error(_File, Error) -> % blah
  % {call_undef,{21,27},els_config,get,1}
  FMsg = gradualizer_fmt:format_type_error(Error, []),

  {Ln, Col} = case Error of 
    {_, {L, C}, _, _, _} -> 
      {L, C};
    _ -> 
      {2,1}
  end,

  Range   = els_protocol:range(#{ from => {Ln, Col}
                                , to   => {Ln + 1, 1} 
                                }),
  lager:error("Error: ~p", [Error]),
  Message = list_to_binary(FMsg),
  #{ range    => Range
    , severity => ?DIAGNOSTIC_ERROR 
    % , code     => <<"Name">>
    , source   => source()
    , message  => Message
    }. 