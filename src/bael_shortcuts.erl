-module(bael_shortcuts).

-export([render_ok/3]).

render_ok(Req, TemplateModule, Params)->
	{ok, Output}=TemplateModule:render(Params),
	Req:ok({"text/html", Output}).
