%%********************************************************************
%% @title Módulo helloworld_service
%% @version 1.0.0
%% @doc Módulo de serviço para o famoso hello world!!!
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************
-module(authorization).

-export([execute/1]).
-export([issue_token/2]).


execute(Request) -> 
	ResponseType = ems_request:get_querystring(<<"response_type">>, "", Request),
	RedirectUri = ems_request:get_querystring(<<"redirect_uri">>, "", Request),
	ClientId = ems_request:get_querystring(<<"client_id">>, "", Request),
	Secret = ems_request:get_querystring(<<"secret">>, "", Request),
	State = ems_request:get_querystring(<<"state">>, "", Request),
	Scope = ems_request:get_querystring(<<"scope">>, "", Request),

	%io:format("\nresponse_type = ~p\nclient_id = ~p \nredirect_uri = ~p \nstate = ~p \nSecret = ~p\n_____________\n", [ResponseType, ClientId, RedirectUri, State, Secret]),
	
	Resposta = process_client_credentials_grant(ClientId, Secret, Scope, Request),
	%io:format("\nAutenticação = ~p\n_____________\n", [Resposta2]),

	%io:format("Requisição ~p",[Request]),
	Resposta.
	
process_client_credentials_grant(ClientId, Secret, Scope, Request) ->
    Auth = oauth2:authorize_client_credentials(ClientId, Secret, Scope, []),
	issue_token(Auth, Request).
    
process_password_grant(ClientId, Secret, Scope, Request) ->
	%io:format(".............\n Username: ~p \n Passwd: ~p \n Scope: ~p \n...............\n", [Username,Password,Scope] ),
	Auth = oauth2:authorize_password(ClientId, Secret, Scope, []),
	%io:format("..............\n Auth: ~p \n...............", [Auth] ),
	issue_token(Auth, Request).
	
issue_token({ok, Auth}, Request) ->
	Response = oauth2:issue_token(Auth, []),
	%io:format("\n#################\nToken = ~p\n#################\n", [Response]),
    oauth2_response:to_proplist(Response);
issue_token(Error, Request) ->
    emit_response(Error, Request).

emit_response(AuthResult, Req) ->
    {Code, JSON} =
        case AuthResult of
            {error, Reason} ->
                {400, jsx:encode([{error, to_binary(Reason)}])};
            Response ->
                {200, jsx:encode(to_json_term(oauth2_response:to_proplist(Response), []))}
        end,
    cowboy_req:reply(Code, [], JSON, Req).
to_binary(Atom) when is_atom(Atom) ->
    list_to_binary(atom_to_list(Atom)).
    
to_json_term([], Acc) ->
    Acc;
to_json_term([{H, {HK, HV}} |  T], Acc) ->
    to_json_term(T, [{H, <<"{",HK/binary,",",HV/binary,"}">>} | Acc]);
to_json_term([H | T], Acc) ->
    to_json_term(T, [H | Acc]).

