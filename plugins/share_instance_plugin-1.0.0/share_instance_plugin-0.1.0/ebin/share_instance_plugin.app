{application,share_instance_plugin,
             [{description,"Another amazing EMQX plugin."},
              {vsn,"0.1.0"},
              {modules,[share_instance_plugin,share_instance_plugin_app,
                        share_instance_plugin_cli,share_instance_plugin_sup]},
              {registered,[share_instance_plugin_sup]},
              {applications,[kernel,stdlib,map_sets]},
              {mod,{share_instance_plugin_app,[]}},
              {env,[]},
              {licenses,["Apache-2.0"]},
              {maintainers,["Anonymous <anonymous@example.org>"]}]}.