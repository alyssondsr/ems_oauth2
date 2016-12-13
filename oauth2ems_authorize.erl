-module(oauth2ems_authorize).

-export([execute/1]).

execute(Request) -> 
	ResponseType = ems_request:get_querystring(<<"response_type">>, "", Request),
	GrantType = ems_request:get_querystring(<<"grant_type">>, "", Request),
	Type = erlang:max(ResponseType,GrantType),
        Resposta = case Type of
            "password" -> 
				process_password_grant(Request);
            "client_credentials" ->
				process_client_credentials_grant(Request);
            "token" ->
				process_implicit_grant(Request);
             _ ->
				<<"\{error: invalid_request\}">>
			end,  
		%io:format("..............\n Reply: ~p \n...............\n", [Teste] ),
    %<<"\{ok,ok\}">>. 
	Resposta.
	
process_client_credentials_grant(Request) ->
	ClientId = ems_request:get_querystring(<<"client_id">>, "", Request),
	Secret = ems_request:get_querystring(<<"secret">>, "", Request),
	Scope = ems_request:get_querystring(<<"scope">>, "", Request),	
    Auth = oauth2:authorize_client_credentials(ClientId, Secret, Scope, []),
	issue_token(Auth).
    
process_password_grant(Request) -> 
	Username = ems_request:get_querystring(<<"username">>, "", Request),
	Password = ems_request:get_querystring(<<"password">>, "", Request),
	Scope = ems_request:get_querystring(<<"scope">>, "", Request),	
    Auth = oauth2:authorize_password(Username, Password, Scope, []),
	issue_token(Auth).

process_implicit_grant(Request) ->
    State       = ems_request:get_querystring(<<"state">>, [],Request),
    Scope       = ems_request:get_querystring(<<"scope">>, [],Request),
    ClientId    = ems_request:get_querystring(<<"client_id">>, [],Request),
    RedirectUri = ems_request:get_querystring(<<"redirect_uri">>, [],Request),
    io:format("..............\n redirect_uri: ~p \n...............\n", [redirect_uri] ),
    Auth = case ems_oauth2_backend:verify_redirection_uri(ClientId, RedirectUri, []) of
		{ok,Uri} -> 
			list_to_binary(Uri);
		{error, Reason} ->
			io:format("..............\n Reason: ~p \n...............\n", [Reason] ),
			<<"\{error:\}">>
	end,			
    io:format("..............\n Auth: ~p \n...............\n", [Auth] ),
    Auth.

process_implicit_grant_stage2(Request) ->
    ClientId    = ems_request:get_querystring(<<"client_id">>, [],Request),
    RedirectUri = ems_request:get_querystring(<<"redirect_uri">>, [],Request),
    Username    = ems_request:get_querystring(<<"username">>, [],Request),
    Password    = ems_request:get_querystring(<<"password">>, [],Request),
    State       = ems_request:get_querystring(<<"state">>, [],Request),
    Scope       = ems_request:get_querystring(<<"scope">>, [],Request),
    %case oauth2:verify_redirection_uri(ClientId, RedirectUri) of
     %  ok ->
     %      case oauth2:authorize_password(Username, Password, Scope) of
     %          
     %      end;
       
	<<"\{ok,ok\}">>.


issue_token({ok, Auth}) ->
	Response = oauth2:issue_token(Auth, []),
   	%io:format("\n#################\nToken = ~p\n#################\n", [oauth2_response:to_proplist(Response)]),
	    oauth2_response:to_proplist(Response);
issue_token(Error) ->
    Error.
