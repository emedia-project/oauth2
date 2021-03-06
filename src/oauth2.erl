-module(oauth2).

-export([
         authorize_code_request/4,
         authorize_code_request/5,
         authorize_code_grant/3,
         authorize_code_grant/4,
         implicit_grant/4,
         authorize_client_credentials/2,
         issue_code/1,
         issue_token/1,
         issue_token_and_refresh/1,
         verify_access_token/1,
         verify_refresh_token/1,
         refresh_access_token/3,
         '$handle_undefined_function'/2
        ]).

-export_type([
              context/0,
              token/0,
              client_info/0,
              client/0,
              client_id/0,
              client_secret/0,
              user_infos/0,
              user/0,
              user_login/0,
              user_password/0,
              redirect_uri/0,
              scope/0,
              state/0,
              expiry/0,
              owner/0,
              auth/0,
              response/0
             ]).

-type context() :: map().
-type token() :: binary().
-type client_info() :: any().
-type client() :: {client_id(), client_secret()}.
-type client_id() :: binary().
-type client_secret() :: binary().
-type user_infos() :: any().
-type user() :: {user_login(), user_password()}.
-type user_login() :: binary().
-type user_password() :: binary().
-type redirect_uri() :: binary().
-type scope() :: list(binary()) | binary().
-type state() :: binary().
-type expiry() :: non_neg_integer().
-type owner() :: any().
-type auth() :: map().
-type response() :: map().
-type oauth2_error() :: access_denied 
| invalid_client 
| invalid_grant 
| invalid_request 
| invalid_authorization 
| invalid_scope 
| unauthorized_client 
| unsupported_grant_type 
| unsupported_response_type 
| server_error 
| temporarily_unavailable.

-define(BACKEND, (oauth2_config:backend())).
-define(TOKEN_GENERATOR, (oauth2_config:token_generator())).

-spec authorize_code_request(client_id(), redirect_uri(), user(), scope()) -> {ok, auth()} | {error, oauth2_error()}.
authorize_code_request(ClientID, RedirectURI, User, Scope) ->
  authorize_code_request(ClientID, RedirectURI, User, Scope, <<>>).

-spec authorize_code_request(client_id(), redirect_uri(), user(), scope(), state()) -> {ok, auth()} | {error, oauth2_error()}.
authorize_code_request(ClientID, RedirectURI, User, Scope, State) -> 
  case ?BACKEND:validate_redirect_uri(ClientID, RedirectURI, State) of
    {ok, State1} -> 
      case ?BACKEND:authenticate_user(User, State1) of
        {ok, {State2, UserInfos, Owner}} -> 
          case ?BACKEND:verify_scope(UserInfos, Owner, Scope, State2) of
            {ok, {State3, Scope1}} -> 
              {ok, #{
                 type => user,
                 user => UserInfos,
                 client_id => ClientID,
                 owner => Owner,
                 scope => oauth2_utils:list_to_scope(Scope1),
                 state => State3,
                 redirect_uri => RedirectURI
                }};
            {error, _} ->
              {error, invalid_scope}
          end;
        {error, _} ->
          {error, invalid_request}
      end;
    {error, _} -> 
      {error, unauthorized_client}
  end.

-spec authorize_code_grant(oauth2:client(), oauth2:redirect_uri(), oauth2:token()) -> {ok, auth()} | {error, oauth2_error()}.
authorize_code_grant(Client, RedirectURI, Code) ->
  authorize_code_grant(Client, RedirectURI, Code, <<>>).
  
-spec authorize_code_grant(oauth2:client(), oauth2:redirect_uri(), oauth2:token(), oauth2:state()) -> {ok, auth()} | {error, oauth2_error()}.
authorize_code_grant({ClientID, _} = Client, RedirectURI, Code, State) ->
  case ?BACKEND:authenticate_client(Client, State) of
    {ok, State1} ->
      case ?BACKEND:validate_redirect_uri(ClientID, RedirectURI, State1) of
        {ok, State2} ->
          Result = ?BACKEND:verify_access_code(Code),
          _ = ?BACKEND:revoke_access_code(Code),
          case Result of
            {ok, #{client_id := ClientID1, redirect_uri := RedirectURI1, state := State3, associate_token_type := AssociateTokenType}} ->
              case {eutils:compare_as_binary(ClientID, ClientID1),
                    eutils:compare_as_binary(RedirectURI, RedirectURI1),
                    eutils:compare_as_binary(State2, State3),
                    eutils:compare_as_binary(AssociateTokenType, <<"code">>)} of
                {0, 0, 0, 0} ->
                  Result;
                _ ->
                  {error, invalid_request}
              end;
            _ ->
              {error, invalid_request}
          end;
        {error, _} ->
          _ = ?BACKEND:revoke_access_code(Code),
          {error, unauthorized_client}
      end;
    {error, _} ->
      _ = ?BACKEND:revoke_access_code(Code),
      {error, unauthorized_client}
  end.

-spec implicit_grant(client(), redirect_uri(), user(), scope()) -> {ok, auth()} | {error, oauth2_error()}.
implicit_grant({ClientID, _} = Client, RedirectURI, User, Scope) -> 
  case ?BACKEND:authenticate_client(Client, <<>>) of
    {ok, _} ->
      case ?BACKEND:validate_redirect_uri(ClientID, RedirectURI, <<>>) of
        {ok, _} ->
          case ?BACKEND:authenticate_user(User, <<>>) of
            {ok, {_, UserInfos, Owner}} ->
              case ?BACKEND:verify_scope(UserInfos, Owner, Scope, <<>>) of
                {ok, {_, Scope1}} ->
                  {ok, #{
                     type => user,
                     user => UserInfos,
                     client_id => ClientID,
                     owner => Owner,
                     scope => oauth2_utils:list_to_scope(Scope1),
                     state => <<>>,
                     redirect_uri => RedirectURI
                    }};
                {error, _} ->
                  {error, invalid_scope}
              end;
            {error, _} ->
              {error, invalid_request}
          end;
        {error, _} ->
          {error, unauthorized_client}
      end;
    {error, _} ->
      {error, unauthorized_client}
  end.

-spec authorize_client_credentials(client(), scope()) -> {ok, auth()} | {error, oauth2_error()}.
authorize_client_credentials({ClientID, _} = Client, Scope) ->
  case ?BACKEND:authenticate_client(Client, <<>>) of
    {ok, _} ->
      case ?BACKEND:verify_client_scope(ClientID, Scope) of
        {ok, Scope1} ->
          {ok, #{
             type => client,
             client_id => ClientID,
             scope => oauth2_utils:list_to_scope(Scope1),
             state => <<>>
            }};
        {error, _} ->
          {error, invalid_scope}
      end;
    {error, _} ->
      {error, unauthorized_client}
  end.

-spec issue_code(auth()) -> {ok, response()} | {error, oauth2_error()}.
issue_code(#{state := State} = Auth) ->
  Auth1 = maps:put(associate_token_type, code, Auth),
  case ?BACKEND:associate_access_code(
          ?TOKEN_GENERATOR:generate(Auth1),
          oauth2_config:expiry(code_grant), Auth1) of
    {ok, AccessCode} ->
      {ok, #{code => AccessCode, 
             state => State}};
    E -> E
  end.

-spec issue_token(auth()) -> {ok, response()} | {error, oauth2_error()}.
issue_token(#{state := State} = Auth) -> 
  Auth1 = maps:put(associate_token_type, access_token, Auth),
  case ?BACKEND:associate_access_token(
          ?TOKEN_GENERATOR:generate(Auth1),
          oauth2_config:expiry(access_token), Auth1) of
    {ok, AccessToken} ->
      {ok, #{access_token => AccessToken,
             expires_in => oauth2_config:expiry(access_token),
             token_type => <<"bearer">>,
             state => State}};
    E -> E
  end.

-spec issue_token_and_refresh(auth()) -> {ok, response()} | {error, oauth2_error()}.
issue_token_and_refresh(#{state := State} = Auth) -> 
  Auth1 = maps:put(associate_token_type, access_token, Auth),
  case ?BACKEND:associate_access_token(
          ?TOKEN_GENERATOR:generate(Auth1),
          oauth2_config:expiry(access_token), Auth1) of
    {ok, AccessToken} ->
      Auth2 = maps:put(associate_token_type, refresh_token, Auth),
      case ?BACKEND:associate_refresh_token(
              ?TOKEN_GENERATOR:generate(Auth2),
              oauth2_config:expiry(refresh_token), Auth2) of
        {ok, RefreshToken} ->
          {ok, #{access_token => AccessToken,
                 expires_in => oauth2_config:expiry(access_token),
                 refresh_token => RefreshToken,
                 refresh_token_expires_in => oauth2_config:expiry(refresh_token),
                 token_type => <<"bearer">>,
                 state => State}};
        E -> E
      end;
    E -> E
  end.

-spec verify_access_token(token()) -> {ok, auth()} | {error, oauth2_error()}.
verify_access_token(Token) -> 
  case ?BACKEND:verify_access_token(Token) of
    {error, E} ->
      _ = ?BACKEND:revoke_access_token(Token),
      {error, E};
    {ok, #{associate_token_type := AssociateTokenType}} = OK -> 
      case eutils:compare_as_binary(AssociateTokenType, <<"access_token">>) of
        0 -> OK;
        _ -> {error, invalid_request}
      end
  end.

-spec verify_refresh_token(token()) -> {ok, auth()} | {error, oauth2_error()}.
verify_refresh_token(Token) ->
  case ?BACKEND:verify_refresh_token(Token) of
    {error, E} ->
      _ = ?BACKEND:revoke_refresh_token(Token),
      {error, E};
    {ok, #{associate_token_type := AssociateTokenType}} = OK -> 
      case eutils:compare_as_binary(AssociateTokenType, <<"refresh_token">>) of
        0 -> OK;
        _ -> {error, invalid_request}
      end
  end.

-spec refresh_access_token(client(), token(), scope()) -> {ok, auth()} | {error, oauth2_error()}.
refresh_access_token({ClientID, _} = Client, RefreshToken, Scope) ->
  case verify_refresh_token(RefreshToken) of
    {error, _} ->
      {error, invalid_request};
    {ok, #{client_id := ClientID1, scope := InitialScope} = UserAuth} -> 
      case eutils:compare_as_binary(ClientID1, ClientID) of
        0 ->
          Scope1 = if
                     Scope =:= [] -> oauth2_utils:scope_to_list(InitialScope);
                     true -> Scope
                   end,
          case authorize_client_credentials(Client, Scope1) of
            {ok, #{client_id := ClientID, scope := Scope2}} ->
              _ = ?BACKEND:revoke_refresh_token(RefreshToken),
              UserAuth1 = maps:put(scope, Scope2, UserAuth),
              UserAuth2 = maps:put(state, <<>>, UserAuth1),
              {ok, UserAuth2};
            _ ->
              {error, unauthorized_client}
          end;
        _ ->
          {error, unauthorized_client}
      end;
    _ ->
      {error, unauthorized_client}
  end.

% @hidden
'$handle_undefined_function'(Fun, Args) ->
  case code:ensure_loaded(?BACKEND) of
    {module, Module} ->
      case erlang:function_exported(Module, Fun, length(Args)) of
        false ->
          crash({?MODULE, Fun, length(Args)});
        true ->
          erlang:apply(Module, Fun, Args)
      end;
    {error, _} ->
      crash({?MODULE, Fun, length(Args)})
  end.

% Private

crash(Tuple) ->
  try erlang:error(undef)
  catch
    error:undef ->
      Stk = [Tuple|tl(erlang:get_stacktrace())],
      erlang:raise(error, undef, Stk)
  end.


