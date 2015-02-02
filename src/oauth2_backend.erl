-module(oauth2_backend).

%% Verifies that RedirectionUri is a valid redirection URI for the
%% client identified by Identity.
-callback validate_redirect_uri(oauth2:client_id(), oauth2:redirect_uri(), oauth2:state()) ->
  {ok, oauth2:state()}
  | {error, any()}.

%% Authenticates a combination of username and password.
%% Returns the user infirmations and resource owner identity 
%% if the credentials are valid.
-callback authenticate_user(oauth2:user(), oauth2:state()) ->
  {ok, {oauth2:state(), oauth2:user_infos(), oauth2:owner()}}
  | {error, any()}.

%% Verifies that scope() is a valid scope for the user and owner
-callback verify_scope(oauth2:user_infos(), oauth2:owner(), oauth2:scope(), oauth2:state()) ->
  {ok, {oauth2:state(), oauth2:scope()}}
  | {error, any()}.

%% Verifies that scope() is a valid scope for the client identified
%% by Identity.
-callback verify_client_scope(oauth2:client_id(), oauth2:scope()) ->
  {ok, oauth2:scope()}
  | {error, any()}.

%% Associate the access code to the user authentication
-callback associate_access_code(oauth2:token(), oauth2:expiry(), oauth2:auth()) -> 
  {ok, oauth2:token()} 
  | {error, invalid_request}.

%% Associate the access token to the user authentication
-callback associate_access_token(oauth2:token(), oauth2:expiry(), oauth2:auth()) -> 
  {ok, oauth2:token()} 
  | {error, invalid_request}.

%% Associate the refresh token to the user authentication
-callback associate_refresh_token(oauth2:token(), oauth2:expiry(), oauth2:auth()) -> 
  {ok, oauth2:token()} 
  | {error, invalid_request}.

%% Authenticates a combination of client id and client secret.
-callback authenticate_client(oauth2:client(), oauth2:state()) ->
  {ok, oauth2:state()}
  | {error, any()}.

%% Revoke an access code
-callback revoke_access_code(oauth2:token()) -> ok.

%% Revoke an access token
-callback revoke_access_token(oauth2:token()) -> ok.

%% Revoke an refresh token
-callback revoke_refresh_token(oauth2:token()) -> ok.

%% Validate that the given access code is valid.
%% Return the associated authentication if the code is valid.
-callback verify_access_code(oauth2:token()) ->
  {ok, oauth2:auth()}
  | {error, any()}.

%% Validate that the given access token is valid.
%% Return the associated authentication if the code is valid.
-callback verify_access_token(oauth2:token()) -> 
  {ok, oauth2:auth()} 
  | {error, any()}.

%% Validate that the given refresh token is valid.
%% Return the associated authentication if the code is valid.
-callback verify_refresh_token(oauth2:token()) -> 
  {ok, oauth2:auth()}
  | {error, any()}.
