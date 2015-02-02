-module(oauth2_config).

-export([
         get_value/1,
         backend/0,
         token_generator/0,
         expiry/1
        ]).
-define(DEFAULT_EXPIRY, 3600).

get_value(Param) ->
  application:get_env(oauth2, Param).

backend() ->
  case get_value(backend) of
    undefined -> throw({missing, backend});
    {ok, V} -> V
  end.

token_generator() ->
  case get_value(token_generator) of
    undefined -> oauth2_token;
    {ok, V} -> V
  end.

expiry(What) ->
  case get_value(expiries) of
    undefined -> ?DEFAULT_EXPIRY;
    {ok, Expiries} ->
      elists:keyfind(What, 1, Expiries, ?DEFAULT_EXPIRY)
  end.
