-module(oauth2ems_authorize).

-export([execute/1]).

-include("../include/ems_schema.hrl").



execute(Request = #request{type = Type}) -> 
	TypeAuth = case Type of
		"GET" -> ems_request:get_querystring(<<"response_type">>, "", Request);
		"POST" -> ems_request:get_querystring(<<"grant_type">>, "", Request)
	end,
    Result = case TypeAuth of
            "password" -> 
				password_grant(Request);
            "client_credentials" ->
				client_credentials_grant(Request);
			"token" ->
				authorization_request(Request);
			"code" ->
				authorization_request(Request);	
			"authorization_code" ->
				access_token_request(Request);	
			"code2" ->
				% Apenas para simulação
				code_request(Request);				
             _ -> {error, invalid_request}
	end,  
	case Result of
		{ok, ResponseData} ->
			ResponseData2 = ems_schema:prop_list_to_json(ResponseData),
			{ok, Request#request{code = 200, 
								 response_data = ResponseData2}
			};
		{redirect, ClientId, RedirectUri} ->
			%LocationPath = lists:concat(["http://127.0.0.1:2301/authorize?response_type=code2&client_id=", ClientId, "&redirect_uri=", RedirectUri]),
			LocationPath = lists:concat(["http://127.0.0.1:2301/portal/index.html?response_type=code2&client_id=", ClientId, "&redirect_uri=", RedirectUri]),
			{ok, Request#request{code = 302, 
									 response_data = <<"{}">>,
									 response_header = #{
															<<"location">> => LocationPath
														}
									}
			};
		Error ->   {ok, Request#request{code = 400, 
								 response_data = Error}
					}
	end.

	
%%%===================================================================
%%% Funções internas
%%%===================================================================


%% Cliente Credencial Grant- seção 4.4.1 do RFC 6749. 
%% URL de teste: POST http://127.0.0.1:2301/authorize?grant_type=client_credentials&client_id=s6BhdRkqt3&secret=qwer
client_credentials_grant(Request) ->
	ClientId = ems_request:get_querystring(<<"client_id">>, "", Request),
	Secret = ems_request:get_querystring(<<"secret">>, "", Request),
	Scope = ems_request:get_querystring(<<"scope">>, "", Request),	
    Authorization = oauth2:authorize_client_credentials({ClientId, Secret}, Scope, []),
	issue_token(Authorization).

%% Resource Owner Password Credentials Grant - seção 4.3.1 do RFC 6749.
%% URL de teste: POST http://127.0.0.1:2301/authorize?grant_type=password&username=johndoe&password=A3ddj3w
password_grant(Request) -> 
	Username = ems_request:get_querystring(<<"username">>, "", Request),
	Password = ems_request:get_querystring(<<"password">>, "", Request),
	Scope = ems_request:get_querystring(<<"scope">>, "", Request),	
	Authorization = oauth2:authorize_password({Username,Password}, Scope, []),
	issue_token(Authorization).
	
%% Verifica a URI do Cliente e redireciona para a página de autorização - Implicit Grant e Authorization Code Grant
%% URL de teste: GET http://127.0.0.1:2301/authorize?response_type=code&client_id=s6BhdRkqt3&state=xyz%20&redirect_uri=http%3A%2F%2Flocalhost%3A2301%2Fportal%2Findex.html
authorization_request(Request) ->
    %State       = ems_request:get_querystring(<<"state">>, [],Request),
    %Scope       = ems_request:get_querystring(<<"scope">>, [],Request),
    ClientId    = ems_request:get_querystring(<<"client_id">>, [],Request),
    RedirectUri = ems_request:get_querystring(<<"redirect_uri">>, [],Request),
    Resposta = case oauth2ems_backend:verify_redirection_uri(ClientId, RedirectUri, []) of
		ok -> {redirect, ClientId, RedirectUri};
		Error -> Error
	end,
    Resposta.

%% Requisita o código de autorização - seções 4.1.1 e 4.1.2 do RFC 6749.
%% URL de teste: GET http://127.0.0.1:2301/authorize?response_type=code2&client_id=s6BhdRkqt3&state=xyz%20&redirect_uri=http%3A%2F%2Flocalhost%3A2301%2Fportal%2Findex.html&username=johndoe&password=A3ddj3w
code_request(Request) ->
    ClientId    = ems_request:get_querystring(<<"client_id">>, [],Request),
    RedirectUri = ems_request:get_querystring(<<"redirect_uri">>, [],Request),
    Username    = ems_request:get_querystring(<<"username">>, [],Request),
    Password    = ems_request:get_querystring(<<"password">>, [],Request),
    %State      = ems_request:get_querystring(<<"state">>, [],Request),
    Scope       = ems_request:get_querystring(<<"scope">>, [],Request),
    Authorization = oauth2:authorize_code_request({Username,Password}, ClientId, RedirectUri, Scope, []),
   	issue_code(Authorization).

%% Requisita o token de acesso com o código de autorização - seções  4.1.3. e  4.1.4 do RFC 6749.
%% URL de teste: GET http://127.0.0.1:2301/authorize?grant_type=authorization_code&client_id=s6BhdRkqt3&state=xyz%20&redirect_uri=http%3A%2F%2Flocalhost%3A2301%2Fportal%2Findex.html&username=johndoe&password=A3ddj3w&secret=qwer&code=dxUlCWj2JYxnGp59nthGfXFFtn3hJTqx
access_token_request(Request) ->
	Code = list_to_binary(ems_request:get_querystring(<<"code">>, [],Request)),
	ClientId    = ems_request:get_querystring(<<"client_id">>, [],Request),
    RedirectUri = ems_request:get_querystring(<<"redirect_uri">>, [],Request),
    ClientSecret = ems_request:get_querystring(<<"secret">>, [],Request),
    Authorization = oauth2:authorize_code_grant({ClientId, ClientSecret}, Code, RedirectUri, []),
    issue_token(Authorization).  
		

issue_token({ok, {_, Auth}}) ->
	{ok, {_, Response}} = oauth2:issue_token(Auth, []),
	{ok, oauth2_response:to_proplist(Response)};
issue_token(Error) ->
    Error.
    
issue_code({ok, {_, Auth}}) ->
	{ok, {_, Response}} = oauth2:issue_code(Auth, []),
	{ok, oauth2_response:to_proplist(Response)};
issue_code(Error) ->
    Error.

