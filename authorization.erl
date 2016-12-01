%%********************************************************************
%% @title Módulo helloworld_service
%% @version 1.0.0
%% @doc Módulo de serviço para o famoso hello world!!!
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************
-module(authorization).

-export([execute/1]).

execute(Request) -> 
	%ResponseType = ems_request:get_querystring(<<"response_type">>, "", Request),
	GrantType = ems_request:get_querystring(<<"grant_type">>, "", Request),
	{ok, Reply} =
        case GrantType of
            "password" -> 
				process_password_grant(Request);
            "client_credentials" ->
				process_client_credentials_grant(Request);
            "token" ->
				process_client_credentials_grant(Request);
             _ ->
				process_password_grant(Request)
			end,  
		io:format("..............\n Reply: ~p \n...............\n", [Reply] ),
     
	Reply.
	
process_client_credentials_grant(Request) ->
	ClientId = ems_request:get_querystring(<<"client_id">>, "", Request),
	Secret = ems_request:get_querystring(<<"secret">>, "", Request),
	Scope = ems_request:get_querystring(<<"scope">>, "", Request),	
    Auth = oauth2:authorize_client_credentials(ClientId, Secret, Scope, []),
    io:format("..............\n Auth: ~p \n...............\n", [Auth] ),
	issue_token(Auth).
    
process_password_grant(Request) -> 
	Username = ems_request:get_querystring(<<"username">>, "", Request),
	Password = ems_request:get_querystring(<<"password">>, "", Request),
	Scope = ems_request:get_querystring(<<"scope">>, "", Request),	
    io:format("..............\n User: ~p \n...............\n", [Username] ),
    Auth = oauth2:authorize_password(Username, Password, Scope, []),
    io:format("..............\n Auth: ~p \n...............\n", [Auth] ),
	issue_token(Auth).
	
issue_token({ok, Auth}) ->
	Response = oauth2:issue_token(Auth, []),
	%io:format("\n#################\nToken = ~p\n#################\n", [Response]),
    oauth2_response:to_proplist(Response);
issue_token(Error) ->
    Error.
