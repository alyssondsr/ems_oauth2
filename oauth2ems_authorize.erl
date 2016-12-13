-module(oauth2ems_authorize).

-export([execute/1]).

execute(Request) -> 
	ResponseType = ems_request:get_querystring(<<"response_type">>, "", Request),
	GrantType = ems_request:get_querystring(<<"grant_type">>, "", Request),
	Type = erlang:max(ResponseType,GrantType),
        Resposta = case Type of
            "password" -> 
				password_grant(Request);
            "client_credentials" ->
				client_credentials_grant(Request);
            "token" ->
				implicit_grant(Request);
             _ ->
				<<"\{error: invalid_request\}">>
			end,  
	Resposta.
	
% Funções Internas %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
client_credentials_grant(Request) ->
	ClientId = ems_request:get_querystring(<<"client_id">>, "", Request),
	Secret = ems_request:get_querystring(<<"secret">>, "", Request),
	Scope = ems_request:get_querystring(<<"scope">>, "", Request),	
    Auth = oauth2:authorize_client_credentials(ClientId, Secret, Scope, []),
	issue_token(Auth).
    
password_grant(Request) -> 
	Username = ems_request:get_querystring(<<"username">>, "", Request),
	Password = ems_request:get_querystring(<<"password">>, "", Request),
	Scope = ems_request:get_querystring(<<"scope">>, "", Request),	
    Auth = oauth2:authorize_password(Username, Password, Scope, []),
	issue_token(Auth).

implicit_grant(Request) ->
    State       = ems_request:get_querystring(<<"state">>, [],Request),
    Scope       = ems_request:get_querystring(<<"scope">>, [],Request),
    ClientId    = ems_request:get_querystring(<<"client_id">>, [],Request),
    RedirectUri = ems_request:get_querystring(<<"redirect_uri">>, [],Request),
    Resposta = case ems_oauth2_backend:verify_redirection_uri(ClientId, RedirectUri, []) of
		{ok,Uri} -> 
			[{ <<"uri">>, Uri}];
		{error, Reason} ->
			[{ <<"error">>, Reason}]                         
	end,			
    Resposta.

implicit_grant2(Request) ->
    ClientId    = ems_request:get_querystring(<<"client_id">>, [],Request),
    RedirectUri = ems_request:get_querystring(<<"redirect_uri">>, [],Request),
    Username    = ems_request:get_querystring(<<"username">>, [],Request),
    Password    = ems_request:get_querystring(<<"password">>, [],Request),
    State       = ems_request:get_querystring(<<"state">>, [],Request),
    Scope       = ems_request:get_querystring(<<"scope">>, [],Request),
    Resposta 	= case oauth2:verify_redirection_uri(ClientId, RedirectUri) of
        ok ->
            case oauth2:authorize_password(Username, Password, Scope) of
                {ok, Response} ->
                    [{<<"state">>, State} | oauth2_response:to_proplist(Response)];
                {error, Reason} ->
					[{ <<"error">>, Reason}]
			end; 
        {error, Reason} ->
			[{ <<"error">>, Reason}]                         
	end,			
    Resposta.
	%<<"\{ok,ok\}">>.


issue_token({ok, Auth}) ->
	Response = oauth2:issue_token(Auth, []),
	    oauth2_response:to_proplist(Response);
issue_token(Error) ->
    Error.
