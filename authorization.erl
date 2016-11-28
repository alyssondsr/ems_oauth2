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
	Secret = ems_request:get_querystring(<<"secret">>, "", Request),
	State = ems_request:get_querystring(<<"state">>, "", Request),
	Scope = ems_request:get_querystring(<<"scope">>, "", Request),

	io:format("\nresponse_type = ~p\nclient_id = ~p \nredirect_uri = ~p \nstate = ~p \nSecret = ~p\n_____________\n", [ResponseType, ClientId, RedirectUri, State, Secret]),
	Resposta = ems_oauth2_backend:authenticate_client(ClientId,Secret,""),
	Unio:format("\nAutenticação = ~p\n_____________\n", [Resposta]),

	%io:format("Requisição ~p",[Request]),
	<<"{\"ok\": \"ok\"}">>.
	

