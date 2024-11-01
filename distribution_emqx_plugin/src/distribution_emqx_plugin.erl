-module(distribution_emqx_plugin).

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

%% Message Pubsub Hooks
-export([on_message_delivered/3]).

%% Called when the plugin application start
load(Env) ->
    hook('message.delivered', {?MODULE, on_message_delivered, [Env]}).

%%--------------------------------------------------------------------
%% Message PubSub Hooks
%%--------------------------------------------------------------------
on_message_delivered(
    _ClientInfo = #{clientid := RawClientId, username := RawUsername}, Message, _Env
) ->
    % 将 RawClientId 和 RawUsername 转换为字符串并去掉前置空格
    ClientId = string:trim(to_string(RawClientId), leading, " "),
    Username = string:trim(to_string(RawUsername), leading, " "),

    Payload = emqx_message:payload(Message),

    % 处理 Payload
    BinData =
        case Payload of
            _ when is_binary(Payload) -> Payload;
            _ when is_list(Payload) -> iolist_to_binary(Payload);
            % 如果 Payload 为空，返回空二进制
            _ -> <<>>
        end,

    % 将 binary 转换为列表
    MessageList = binary_to_list(BinData),
    % 检查魔数并提取设备ID列表
    case check_magic_and_extract_ids(MessageList) of
        {ok, DeviceIDs} ->
            % 发布消息给对应的客户端
            case check_conditions(DeviceIDs, ClientId, Username) of
                true ->
                    % 移除魔数标识
                    StrippedPayload = list_to_binary(strip_magic_prefix(MessageList)),
                    ModifiedMessage = Message#message{payload = StrippedPayload},
                    {ok, ModifiedMessage};
                false ->
                    ModifiedMessage = Message#message{payload = <<>>, topic = "n"},
                    {ok, ModifiedMessage}
            end;
        error ->
            ModifiedMessage = Message#message{payload = <<>>, topic = "n"},
            {ok, ModifiedMessage}
    end.

to_string(Value) ->
    if
        is_atom(Value) -> atom_to_list(Value);
        is_binary(Value) -> binary_to_list(Value)
    end.

check_conditions([], _ClientId, _Username) ->
    if
        _Username =:= "das" ->
            true;
        true ->
            false
    end;
check_conditions([DeviceID | Rest], ClientId, Username) ->
    if
        Username =:= "das" ->
            true;
        true ->
            case {string:prefix(ClientId, DeviceID), string:prefix(Username, DeviceID)} of
                {nomatch, nomatch} ->
                    check_conditions(Rest, ClientId, Username);
                {PrefixClientId, _} when PrefixClientId =/= nomatch ->
                    % 38 是字符 '&' 的 ASCII 值
                    case lists:member(38, Username) of
                        true ->
                            false;
                        false ->
                            true
                    end;
                _ ->
                    true
            end
    end.

% 检查魔数并提取设备ID列表的递归函数
-spec check_magic_and_extract_ids(list()) -> {ok, list(string())} | error.
check_magic_and_extract_ids([
    239, 188, 171, 239, 189, 129, 239, 189, 141, 239, 189, 133, 239, 189, 142, 124 | Rest
]) ->
    % 提取到 '@' 之前的内容并分割设备ID
    case extract_device_ids(Rest, [[]]) of
        {ok, DeviceIDs} -> {ok, DeviceIDs};
        error -> error
    end;
check_magic_and_extract_ids(_) ->
    {ok, []}.

% 提取设备ID的递归函数
-spec extract_device_ids(list(), list(string())) -> {ok, list(string())} | error.
extract_device_ids([64 | _], Acc) ->
    {ok, [lists:reverse([38 | ID]) || ID <- Acc]};
extract_device_ids([124 | Rest], Acc) ->
    extract_device_ids(Rest, [[] | Acc]);
extract_device_ids([Char | Rest], [CurrentID | IDs]) ->
    extract_device_ids(Rest, [[Char | CurrentID] | IDs]);
extract_device_ids([], Acc) ->
    % 处理结束
    FinalIDs = [lists:reverse([38 | ID]) || ID <- Acc],
    {ok, FinalIDs};
extract_device_ids(_, _) ->
    error.

% 去掉魔数前缀的函数
-spec strip_magic_prefix(list()) -> list().
strip_magic_prefix(List) ->
    case lists:splitwith(fun(Char) -> Char =/= 64 end, List) of
        {_, [64 | Rest]} -> Rest;
        _ -> List
    end.

%% Called when the plugin application stop
unload() ->
    unhook('message.delivered', {?MODULE, on_message_delivered}).

hook(HookPoint, MFA) ->
    %% use highest hook priority so this module's callbacks
    %% are evaluated before the default hooks in EMQX
    emqx_hooks:add(HookPoint, MFA, _Property = ?HP_HIGHEST).

unhook(HookPoint, MFA) ->
    emqx_hooks:del(HookPoint, MFA).
