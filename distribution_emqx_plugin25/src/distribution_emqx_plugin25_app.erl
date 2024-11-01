-module(distribution_emqx_plugin25_app).

-behaviour(application).

-emqx_plugin(?MODULE).

-export([ start/2
        , stop/1
        ]).

start(_StartType, _StartArgs) ->
    {ok, Sup} = distribution_emqx_plugin25_sup:start_link(),
    distribution_emqx_plugin25:load(application:get_all_env()),

    emqx_ctl:register_command(distribution_emqx_plugin25, {distribution_emqx_plugin25_cli, cmd}),
    {ok, Sup}.

stop(_State) ->
    emqx_ctl:unregister_command(distribution_emqx_plugin25),
    distribution_emqx_plugin25:unload().
