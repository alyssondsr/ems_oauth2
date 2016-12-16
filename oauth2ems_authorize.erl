-module(oauth2ems_authorize).

-export([execute/1]).

-include("../include/ems_schema.hrl").


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
				authorization_request(Request);
			"code" ->
				authorization_request(Request);	
			"authorization_code" ->
				access_token_request(Request);	
			"code2" ->
			% Apenas para simulação
				authorization_request2(Request);				
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

authorization_request(Request) ->
    %State       = ems_request:get_querystring(<<"state">>, [],Request),
    %Scope       = ems_request:get_querystring(<<"scope">>, [],Request),
    ClientId    = ems_request:get_querystring(<<"client_id">>, [],Request),
    RedirectUri = ems_request:get_querystring(<<"redirect_uri">>, [],Request),
    Resposta = case ems_oauth2_backend:verify_redirection_uri(ClientId, RedirectUri, []) of
		{ok,Uri} -> 
			[{ <<"uri">>, Uri}];
		{error, Reason} ->
			[{ <<"error">>, Reason}]                         
	end,			
    Resposta.

authorization_request2(Request) ->
    ClientId    = ems_request:get_querystring(<<"client_id">>, [],Request),
    RedirectUri = ems_request:get_querystring(<<"redirect_uri">>, [],Request),
    Username    = ems_request:get_querystring(<<"username">>, [],Request),
    Password    = ems_request:get_querystring(<<"password">>, [],Request),
    %State       = ems_request:get_querystring(<<"state">>, [],Request),
    Scope       = ems_request:get_querystring(<<"scope">>, [],Request),
    io:format("\n====================\nClientId: ~p\n====================\n", [ClientId]),
	io:format("\n====================\nAuth: ~p\n====================\n", [oauth2:authorize_password(Username, Password, Scope,[])]),

    Resposta 	= case ems_oauth2_backend:verify_redirection_uri(ClientId, RedirectUri, [])  of
        {ok, _} ->
            case oauth2:authorize_password(Username, Password, Scope, []) of
                {ok, Auth} ->
                   	issue_code({ok, Auth});
                    %[{<<"state">>, State} | oauth2_response:to_proplist(Response)];
                {error, Reason} ->
					[{ <<"error">>, Reason}]
			end; 
        {error, Reason} ->
			[{ <<"error">>, Reason}]                         
	end,			
    Resposta.

access_token_request(Request) ->
	Code = maps:get(<<"code">>, Request#request.querystring_map, []),
	ClientId    = ems_request:get_querystring(<<"client_id">>, [],Request),
    RedirectUri = ems_request:get_querystring(<<"redirect_uri">>, [],Request),
    Resposta 	= case ems_oauth2_backend:verify_redirection_uri(ClientId, RedirectUri, [])  of
        {ok, Auth} ->
			case oauth2:verify_access_code(Code, []) of
				{ok, _} ->
					io:format("\n====================\nAuth: ~p\n====================\n", [issue_token({ok, Auth})]),
                   	issue_token({ok, Auth});
                {error, Reason} ->
					[{ <<"error">>, Reason}]
			end; 
		{error, Reason} ->
			[{ <<"error">>, Reason}]
		end,
	Resposta. 
		

issue_token({ok, Auth}) ->
	Response = oauth2:issue_token(Auth, []),
	oauth2_response:to_proplist(Response);
issue_token(Error) ->
    Error.
    
issue_code({ok, Auth}) ->
	Response = oauth2:issue_code(Auth, []),
	oauth2_response:to_proplist(Response);
issue_code(Error) ->
    Error.

