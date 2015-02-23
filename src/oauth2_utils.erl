-module(oauth2_utils).

-export([
         scope_to_list/1,
         list_to_scope/1
        ]).

scope_to_list(Scope) when is_list(Scope)->
  case eutils:is_string(Scope) of
    true ->
      [eutils:to_binary(X) || X <- string:tokens(Scope, " ")];
    false ->
      lists:map(fun eutils:to_binary/1, Scope)
  end;
scope_to_list(Scope) ->
  scope_to_list(eutils:to_string(Scope)).

list_to_scope(List) when is_list(List) ->
  eutils:to_binary(string:join(lists:map(fun eutils:to_string/1, List), " "));
list_to_scope(Scope) ->
  list_to_scope([Scope]).
