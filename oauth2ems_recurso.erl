-module(oauth2ems_recurso).

-export([execute/1]).

-include("../include/ems_schema.hrl").


execute(Request) -> 
	Token = maps:get(<<"token">>, Request#request.querystring_map, []),
	Auth = oauth2:verify_access_token(Token, []),
	Response = oauth2_response:to_proplist(Auth),
	{ok, Request#request{code = 200,  response_data = Response} }.
