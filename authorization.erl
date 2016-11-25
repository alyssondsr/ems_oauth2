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
	ResponseType = ems_request:get_querystring(<<"response_type">>, "", Request),
	RedirectUri = ems_request:get_querystring(<<"redirect_uri">>, "", Request),
	ClientId = ems_request:get_querystring(<<"client_id">>, "", Request),
	State = ems_request:get_querystring(<<"state">>, "", Request),
	Scope = ems_request:get_querystring(<<"scope">>, "", Request),
	%Resposta = oauth2:authorize_code_request(ClientId, RedirectUri, "", "", Scope, ""),

	io:format("\nresponse_type = ~p\nclient_id = ~p \nredirect_uri = ~p \nstate = ~p \nscope = ~p\n_____________\n", [ResponseType, ClientId, RedirectUri, State, Scope]),
	
	%io:format("Requisição ~p",[Request]),
	<<"{\"ok\": \"ok\"}">>.
	

