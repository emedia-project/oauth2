

# Module oauth2 #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)



<a name="types"></a>

## Data Types ##




### <a name="type-auth">auth()</a> ###



<pre><code>
auth() = #{}
</code></pre>





### <a name="type-client">client()</a> ###



<pre><code>
client() = {<a href="#type-client_id">client_id()</a>, <a href="#type-client_secret">client_secret()</a>}
</code></pre>





### <a name="type-client_id">client_id()</a> ###



<pre><code>
client_id() = binary()
</code></pre>





### <a name="type-client_info">client_info()</a> ###



<pre><code>
client_info() = any()
</code></pre>





### <a name="type-client_secret">client_secret()</a> ###



<pre><code>
client_secret() = binary()
</code></pre>





### <a name="type-context">context()</a> ###



<pre><code>
context() = #{}
</code></pre>





### <a name="type-expiry">expiry()</a> ###



<pre><code>
expiry() = non_neg_integer()
</code></pre>





### <a name="type-owner">owner()</a> ###



<pre><code>
owner() = any()
</code></pre>





### <a name="type-redirect_uri">redirect_uri()</a> ###



<pre><code>
redirect_uri() = binary()
</code></pre>





### <a name="type-response">response()</a> ###



<pre><code>
response() = #{}
</code></pre>





### <a name="type-scope">scope()</a> ###



<pre><code>
scope() = [binary()] | binary()
</code></pre>





### <a name="type-state">state()</a> ###



<pre><code>
state() = binary()
</code></pre>





### <a name="type-token">token()</a> ###



<pre><code>
token() = binary()
</code></pre>





### <a name="type-user">user()</a> ###



<pre><code>
user() = {<a href="#type-user_login">user_login()</a>, <a href="#type-user_password">user_password()</a>}
</code></pre>





### <a name="type-user_infos">user_infos()</a> ###



<pre><code>
user_infos() = any()
</code></pre>





### <a name="type-user_login">user_login()</a> ###



<pre><code>
user_login() = binary()
</code></pre>





### <a name="type-user_password">user_password()</a> ###



<pre><code>
user_password() = binary()
</code></pre>


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#authorize_code_grant-3">authorize_code_grant/3</a></td><td></td></tr><tr><td valign="top"><a href="#authorize_code_grant-4">authorize_code_grant/4</a></td><td></td></tr><tr><td valign="top"><a href="#authorize_code_request-4">authorize_code_request/4</a></td><td></td></tr><tr><td valign="top"><a href="#authorize_code_request-5">authorize_code_request/5</a></td><td></td></tr><tr><td valign="top"><a href="#client_credential_grant-2">client_credential_grant/2</a></td><td></td></tr><tr><td valign="top"><a href="#implicit_grant-4">implicit_grant/4</a></td><td></td></tr><tr><td valign="top"><a href="#issue_code-1">issue_code/1</a></td><td></td></tr><tr><td valign="top"><a href="#issue_token-1">issue_token/1</a></td><td></td></tr><tr><td valign="top"><a href="#issue_token_and_refresh-1">issue_token_and_refresh/1</a></td><td></td></tr><tr><td valign="top"><a href="#refresh_access_token-3">refresh_access_token/3</a></td><td></td></tr><tr><td valign="top"><a href="#verify_access_token-1">verify_access_token/1</a></td><td></td></tr><tr><td valign="top"><a href="#verify_refresh_token-1">verify_refresh_token/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="authorize_code_grant-3"></a>

### authorize_code_grant/3 ###


<pre><code>
authorize_code_grant(Client::<a href="oauth2.md#type-client">oauth2:client()</a>, RedirectURI::<a href="oauth2.md#type-redirect_uri">oauth2:redirect_uri()</a>, Code::<a href="oauth2.md#type-token">oauth2:token()</a>) -&gt; {ok, <a href="#type-auth">auth()</a>} | {error, any()}
</code></pre>
<br />


<a name="authorize_code_grant-4"></a>

### authorize_code_grant/4 ###


<pre><code>
authorize_code_grant(Client::<a href="oauth2.md#type-client">oauth2:client()</a>, RedirectURI::<a href="oauth2.md#type-redirect_uri">oauth2:redirect_uri()</a>, Code::<a href="oauth2.md#type-token">oauth2:token()</a>, State::<a href="oauth2.md#type-state">oauth2:state()</a>) -&gt; {ok, <a href="#type-auth">auth()</a>} | {error, any()}
</code></pre>
<br />


<a name="authorize_code_request-4"></a>

### authorize_code_request/4 ###


<pre><code>
authorize_code_request(ClientID::<a href="#type-client_id">client_id()</a>, RedirectURI::<a href="#type-redirect_uri">redirect_uri()</a>, User::<a href="#type-user">user()</a>, Scope::<a href="#type-scope">scope()</a>) -&gt; {ok, <a href="#type-auth">auth()</a>} | {error, any()}
</code></pre>
<br />


<a name="authorize_code_request-5"></a>

### authorize_code_request/5 ###


<pre><code>
authorize_code_request(ClientID::<a href="#type-client_id">client_id()</a>, RedirectURI::<a href="#type-redirect_uri">redirect_uri()</a>, User::<a href="#type-user">user()</a>, Scope::<a href="#type-scope">scope()</a>, State::<a href="#type-state">state()</a>) -&gt; {ok, <a href="#type-auth">auth()</a>} | {error, any()}
</code></pre>
<br />


<a name="client_credential_grant-2"></a>

### client_credential_grant/2 ###


<pre><code>
client_credential_grant(Client::<a href="#type-client">client()</a>, Scope::<a href="#type-scope">scope()</a>) -&gt; {ok, <a href="#type-auth">auth()</a>} | {error, any()}
</code></pre>
<br />


<a name="implicit_grant-4"></a>

### implicit_grant/4 ###


<pre><code>
implicit_grant(Client::<a href="#type-client">client()</a>, RedirectURI::<a href="#type-redirect_uri">redirect_uri()</a>, User::<a href="#type-user">user()</a>, Scope::<a href="#type-scope">scope()</a>) -&gt; {ok, <a href="#type-auth">auth()</a>} | {error, any()}
</code></pre>
<br />


<a name="issue_code-1"></a>

### issue_code/1 ###


<pre><code>
issue_code(Auth::<a href="#type-auth">auth()</a>) -&gt; {ok, <a href="#type-response">response()</a>} | {error, any()}
</code></pre>
<br />


<a name="issue_token-1"></a>

### issue_token/1 ###


<pre><code>
issue_token(Auth::<a href="#type-auth">auth()</a>) -&gt; {ok, <a href="#type-response">response()</a>} | {error, any()}
</code></pre>
<br />


<a name="issue_token_and_refresh-1"></a>

### issue_token_and_refresh/1 ###


<pre><code>
issue_token_and_refresh(Auth::<a href="#type-auth">auth()</a>) -&gt; {ok, <a href="#type-response">response()</a>} | {error, any()}
</code></pre>
<br />


<a name="refresh_access_token-3"></a>

### refresh_access_token/3 ###


<pre><code>
refresh_access_token(Token::<a href="#type-token">token()</a>, RefreshToken::<a href="#type-token">token()</a>, Scope::<a href="#type-scope">scope()</a>) -&gt; {ok, <a href="#type-auth">auth()</a>} | {error, any()}
</code></pre>
<br />


<a name="verify_access_token-1"></a>

### verify_access_token/1 ###


<pre><code>
verify_access_token(Token::<a href="#type-token">token()</a>) -&gt; {ok, {user, <a href="#type-user_login">user_login()</a>} | {client, <a href="#type-client_info">client_info()</a>}} | {error, any()}
</code></pre>
<br />


<a name="verify_refresh_token-1"></a>

### verify_refresh_token/1 ###


<pre><code>
verify_refresh_token(Token::<a href="#type-token">token()</a>) -&gt; {ok, {user, <a href="#type-user_login">user_login()</a>} | {client, <a href="#type-client_info">client_info()</a>}} | {error, any()}
</code></pre>
<br />


