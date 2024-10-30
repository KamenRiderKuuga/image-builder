-module(share_instance_plugin_app).

-behaviour(application).

-emqx_plugin(?MODULE).

-export([ start/2
        , stop/1
        ]).

start(_StartType, _StartArgs) ->
    {ok, Sup} = share_instance_plugin_sup:start_link(),
    share_instance_plugin:load(application:get_all_env()),

    emqx_ctl:register_command(share_instance_plugin, {share_instance_plugin_cli, cmd}),
    {ok, Sup}.

stop(_State) ->
    emqx_ctl:unregister_command(share_instance_plugin),
    share_instance_plugin:unload().
