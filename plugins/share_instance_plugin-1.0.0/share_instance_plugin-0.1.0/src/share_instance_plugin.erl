-module(share_instance_plugin).

%% for #message{} record
%% no need for this include if we call emqx_message:to_map/1 to convert it to a map
-include_lib("emqx/include/emqx.hrl").
-include_lib("emqx/include/emqx_hooks.hrl").


%% for logging
-include_lib("emqx/include/logger.hrl").

-export([load/1
  , unload/0
]).

%% Client Lifecycle Hooks
-export([on_client_subscribe/4
  , on_client_unsubscribe/4
]).

%% Session Lifecycle Hooks
-export([on_session_subscribed/4
  , on_session_unsubscribed/4
]).

%% Message Pubsub Hooks
-export([on_message_publish/2
  , on_message_delivered/3
  , on_message_acked/3
  , on_message_dropped/4
]).

%% Called when the plugin application start
load(Env) ->
  hook('client.subscribe', {?MODULE, on_client_subscribe, [Env]}),
  hook('client.unsubscribe', {?MODULE, on_client_unsubscribe, [Env]}),
  hook('session.subscribed', {?MODULE, on_session_subscribed, [Env]}),
  hook('session.unsubscribed', {?MODULE, on_session_unsubscribed, [Env]}),
  hook('message.publish', {?MODULE, on_message_publish, [Env]}),
  hook('message.delivered', {?MODULE, on_message_delivered, [Env]}),
  hook('message.acked', {?MODULE, on_message_acked, [Env]}),
  hook('message.dropped', {?MODULE, on_message_dropped, [Env]}).

%%--------------------------------------------------------------------
%% Client Lifecycle Hooks
%%--------------------------------------------------------------------


on_client_subscribe(#{clientid := ClientId}, _Properties, TopicFilters, _Env) ->
  io:format("Client(~s) will subscribe(share instance): ~p~n", [ClientId, TopicFilters]),
  ModifiedTopicFilters = modify_topics(ClientId, TopicFilters),
  io:format("update subscribe(share instance): ~p~n", [ModifiedTopicFilters]),
  {ok, ModifiedTopicFilters}.

on_client_unsubscribe(#{clientid := ClientId}, _Properties, TopicFilters, _Env) ->
  io:format("Client(~s) will unsubscribe(share instance) ~p~n", [ClientId, TopicFilters]),
  ModifiedTopicFilters = modify_topics(ClientId, TopicFilters),
  io:format("update subscribe(share instance): ~p~n", [ModifiedTopicFilters]),
  {ok, ModifiedTopicFilters}.

%%--------------------------------------------------------------------
%% Session Lifecycle Hooks
%%--------------------------------------------------------------------


on_session_subscribed(#{clientid := ClientId}, Topic, SubOpts, _Env) ->
  io:format("Session(~s) subscribed (share instance) ~s with subopts: ~p~n", [ClientId, Topic, SubOpts]).

on_session_unsubscribed(#{clientid := ClientId}, Topic, Opts, _Env) ->
  io:format("Session(~s) unsubscribed (share instance) ~s with opts: ~p~n", [ClientId, Topic, Opts]).

%%--------------------------------------------------------------------
%% Message PubSub Hooks
%%--------------------------------------------------------------------

%% Transform message and return

on_message_publish(Message, _Env) ->
  io:format("Publish (share instance): ~p~n", [emqx_message:to_map(Message)]),
  ClientId = emqx_message:from(Message),
  OldTopic = emqx_message:topic(Message),
  case get_instance_param(to_string(ClientId)) of
    {ok, Instance} ->
      NewTopic = add_instance_to_topic(Instance, OldTopic),
      io:format("Publish (share instance) update topic: ~p~n", [NewTopic]),
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
  io:format("Message delivered to client(~s):~n~p~n", [ClientId, emqx_message:to_map(Message)]),
  OldTopic = emqx_message:topic(Message),
  case get_instance_param(to_string(ClientId)) of
    {ok, Instance} ->
      NewTopic = sub_instance_to_topic(Instance, OldTopic),
      io:format("Delivered (share instance) update topic: ~p~n", [NewTopic]),
      ModifiedMessage = Message#message{topic = NewTopic},
      {ok, ModifiedMessage};
    error ->
      {ok, Message}
  end.

sub_instance_to_topic(Instance, Topic) ->
  OldTopic = binary_to_list(Topic),
  io:format("sub topic (share instance): ~s, ~s~n", [OldTopic, Instance]),
  case string:str(OldTopic, Instance) of
    0 -> error;
    Index ->
      if
        Index == 1 ->
          NewTopic = string:substr(OldTopic, string:len(Instance) + 2),
          list_to_binary(NewTopic);
        true ->
          Topic
      end
  end.




on_message_acked(_ClientInfo = #{clientid := ClientId}, Message, _Env) ->
  io:format("Message acked by client(~s):~n~p~n",
    [ClientId, emqx_message:to_map(Message)]).

on_message_dropped(Message, _By = #{node := Node}, Reason, _Env) ->
  io:format("Message dropped by node ~p due to ~p:~n~p~n",
    [Node, Reason, emqx_message:to_map(Message)]).


%% Called when the plugin application stop
unload() ->
  unhook('client.subscribe', {?MODULE, on_client_subscribe}),
  unhook('client.unsubscribe', {?MODULE, on_client_unsubscribe}),

  unhook('session.subscribed', {?MODULE, on_session_subscribed}),
  unhook('session.unsubscribed', {?MODULE, on_session_unsubscribed}),

  unhook('message.publish', {?MODULE, on_message_publish}).

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
      io:format("instance not match (share instance)"),
      TopicFilters
  end.


%%get_instance_param(ClientId) ->
%%  Substring = binary_to_list(ClientId),
%%  case string:str(Substring, "instance=") of
%%    0 -> error;
%%    Index ->
%%      case string:str(Substring, "|") of
%%        0 -> error;
%%        Index2 -> string:substr(Substring, Index+9, Index2-Index-9)
%%      end
%%  end.
get_instance_param(Substring) ->
  case string:str(Substring, "instance=") of
    0 -> error;
    Index ->
      Sub1 = string:substr(Substring, Index),
      case string:str(Sub1, ",") of
        0 ->
          case string:str(Sub1, "|") of
            0 -> {ok, string:substr(Sub1, 10)};
            Index2 ->
              {ok, string:substr(Sub1, 10, Index2 - 10)}
          end;
        Index2 ->
          {ok, string:substr(Sub1, 10, Index2 - 10)}
      end
  end.



add_instance_to_topics(Instance, TopicFilters) ->
  lists:map(fun({Topic, _QoS}) -> {add_instance_to_topic(Instance, Topic), _QoS} end, TopicFilters).

add_instance_to_topic(Instance, Topic) ->
  io:format("add topic (share instance): ~s, ~s~n", [binary_to_list(Topic), Instance]),
  list_to_binary(Instance ++ "/" ++ binary_to_list(Topic)).
