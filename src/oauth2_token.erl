-module(oauth2_token).

-behaviour(oauth2_token_generator).

-export([generate/1]).

-define(TOKEN_LENGTH, 32).

-define(ALLOWED_CHARS, "azertyuiopqsdfghjklmwxcvbnNBVCXWMLKJHGFDSQPOIUYTREZA1234567890").

generate(_Context) -> eutils:to_binary(estring:random(?TOKEN_LENGTH)).

