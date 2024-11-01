-module(share_instance_plugin).

%% for #message{} record
%% no need for this include if we call emqx_message:to_map/1 to convert it to a map
-include_lib("emqx/include/emqx.hrl").
-include_lib("emqx/include/emqx_hooks.hrl").

%% for logging
-include_lib("emqx/include/logger.hrl").

-export([
    load/1,
    unload/0
]).

%% Client Lifecycle Hooks
-export([
    on_client_subscribe/4,
    on_client_unsubscribe/4
]).

%% Message Pubsub Hooks
-export([
    on_message_publish/2,
    on_message_delivered/3
]).

%% Called when the plugin application start
load(Env) ->
    hook('client.subscribe', {?MODULE, on_client_subscribe, [Env]}),
    hook('client.unsubscribe', {?MODULE, on_client_unsubscribe, [Env]}),
    hook('message.publish', {?MODULE, on_message_publish, [Env]}),
    hook('message.delivered', {?MODULE, on_message_delivered, [Env]}).

%%--------------------------------------------------------------------
%% Client Lifecycle Hooks
%%--------------------------------------------------------------------

on_client_subscribe(#{clientid := ClientId}, _Properties, TopicFilters, _Env) ->
    ModifiedTopicFilters = modify_topics(ClientId, TopicFilters),
    {ok, ModifiedTopicFilters}.

on_client_unsubscribe(#{clientid := ClientId}, _Properties, TopicFilters, _Env) ->
    ModifiedTopicFilters = modify_topics(ClientId, TopicFilters),
    {ok, ModifiedTopicFilters}.

%%--------------------------------------------------------------------
%% Message PubSub Hooks
%%--------------------------------------------------------------------

%% Transform message and return

on_message_publish(Message, _Env) ->
    ClientId = emqx_message:from(Message),
    OldTopic = emqx_message:topic(Message),
    case get_instance_param(to_string(ClientId)) of
        {ok, Instance} ->
            NewTopic = add_instance_to_topic(Instance, OldTopic),
            ModifiedMessage = Message#message{topic = NewTopic},
            {ok, ModifiedMessage};
        error ->
            {ok, Message}
    end.

to_string(Value) ->
    if
        is_atom(Value) -> atom_to_list(Value);
        is_binary(Value) -> binary_to_list(Value)
    end.

on_message_delivered(_ClientInfo = #{clientid := ClientId}, Message, _Env) ->
    OldTopic = emqx_message:topic(Message),
    case get_instance_param(to_string(ClientId)) of
        {ok, Instance} ->
            NewTopic = sub_instance_to_topic(Instance, OldTopic),
            ModifiedMessage = Message#message{topic = NewTopic},
            {ok, ModifiedMessage};
        error ->
            {ok, Message}
    end.

sub_instance_to_topic(Instance, Topic) ->
    OldTopic = binary_to_list(Topic),
    case string:str(OldTopic, Instance) of
        0 ->
            error;
        Index ->
            if
                Index == 1 ->
                    NewTopic = string:substr(OldTopic, string:len(Instance) + 2),
                    list_to_binary(NewTopic);
                true ->
                    Topic
            end
    end.

%% Called when the plugin application stop
unload() ->
    unhook('client.subscribe', {?MODULE, on_client_subscribe}),
    unhook('client.unsubscribe', {?MODULE, on_client_unsubscribe}),
    unhook('message.publish', {?MODULE, on_message_publish}),
    unhook('message.delivered', {?MODULE, on_message_delivered}).

hook(HookPoint, MFA) ->
    %% use highest hook priority so this module's callbacks
    %% are evaluated before the default hooks in EMQX
    emqx_hooks:add(HookPoint, MFA, _Property = ?HP_HIGHEST).

unhook(HookPoint, MFA) ->
    emqx_hooks:del(HookPoint, MFA).

modify_topics(ClientId, TopicFilters) ->
    case get_instance_param(binary_to_list(ClientId)) of
        {ok, Instance} ->
            add_instance_to_topics(Instance, TopicFilters);
        error ->
            TopicFilters
    end.

get_instance_param(Substring) ->
    case string:str(Substring, "instance=") of
        0 ->
            error;
        Index ->
            Sub1 = string:substr(Substring, Index),
            case string:str(Sub1, ",") of
                0 ->
                    case string:str(Sub1, "|") of
                        0 -> {ok, string:substr(Sub1, 10)};
                        Index2 -> {ok, string:substr(Sub1, 10, Index2 - 10)}
                    end;
                Index2 ->
                    {ok, string:substr(Sub1, 10, Index2 - 10)}
            end
    end.

add_instance_to_topics(Instance, TopicFilters) ->
    lists:map(
        fun({Topic, _QoS}) -> {add_instance_to_topic(Instance, Topic), _QoS} end, TopicFilters
    ).

add_instance_to_topic(Instance, Topic) ->
    list_to_binary(Instance ++ "/" ++ binary_to_list(Topic)).
