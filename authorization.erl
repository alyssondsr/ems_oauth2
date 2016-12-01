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
	%RedirectUri = ems_request:get_querystring(<<"redirect_uri">>, "", Request),
	ClientId = ems_request:get_querystring(<<"client_id">>, "", Request),
	Secret = ems_request:get_querystring(<<"secret">>, "", Request),
	%State = ems_request:get_querystring(<<"state">>, "", Request),
	Scope = ems_request:get_querystring(<<"scope">>, "", Request),	
	
	process_client_credentials_grant(ClientId, Secret, Scope).
	
process_client_credentials_grant(ClientId, Secret, Scope) ->
    Auth = oauth2:authorize_client_credentials(ClientId, Secret, Scope, []),
    io:format("..............\n Auth: ~p \n...............", [Auth] ),
	issue_token(Auth).
    
process_password_grant(ClientId, Secret, Scope) ->
	Auth = oauth2:authorize_password(ClientId, Secret, Scope, []),
	issue_token(Auth).
	
issue_token({ok, Auth}) ->
	Response = oauth2:issue_token(Auth, []),
	%io:format("\n#################\nToken = ~p\n#################\n", [Response]),
    oauth2_response:to_proplist(Response);
issue_token(Error) ->
    Error.
