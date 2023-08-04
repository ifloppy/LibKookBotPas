unit kookstructureutils;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fpjson;

const
  //https://developer.kookapp.cn/doc/event/channel
  kcetAddReaction = 'add_reaction';
  kcetDeleteReaction = 'deleted_reaction';
  kcetUpdateMessage = 'updated_message';
  kcetDeleteMessage = 'deleted_message';
  kcetAddChannel = 'added_channel';
  kcetUpdateChannel = 'updated_channel';
  kcetDeleteChannel = 'deleted_channel';
  kcetPinMessage = 'pinned_message';
  kcetUnpinMessage = 'unpinned_message';

  //https://developer.kookapp.cn/doc/event/direct-message
  kpetUpdate = 'updated_private_message';
  kpetDelete = 'deleted_private_message';
  kpetAddReaction = 'private_added_reaction';
  kpetDeleteReaction = 'private_deleted_reaction';

  //https://developer.kookapp.cn/doc/event/guild-member
  kmetJoin = 'joined_guild';
  kmetExit = 'exited_guild';
  kmetUpdate = 'updated_guild_member';
  kmetOnline = 'guild_member_online';
  kmetOffline = 'guild_member_offline';

  //https://developer.kookapp.cn/doc/event/guild-role
  kretAdd = 'added_role';
  kretDelete = 'deleted_role';
  kretUpdate = 'updated_role';

  //https://developer.kookapp.cn/doc/event/guild
  kgetUpdate = 'updated_guild';
  kgetDelete = 'deleted_guild';
  kgetAddBlock = 'added_block_list';
  kgetDeleteBlock = 'deleted_block_list';
  kgetAddEmoji = 'added_emoji';
  kgetRemoveEmoji = 'removed_emoji';
  kgetUpdateEmoji = 'updated_emoji';

  //https://developer.kookapp.cn/doc/event/message
  kuteJoinVoiceChannel = 'joined_channel';
  kuteExitVoiceChannel = 'exited_channel';
  kuteUserUpdate = 'user_updated';
  kuteSelfJoinGuild = 'self_joined_guild';
  kuteSelfExitGuild = 'self_exited_guild';
  kuteCardButtonClick = 'message_btn_click';

type
  TKookRole = record
    role_id: integer;
    Name: string;
    color: integer;
    position: integer;
    hoist: integer;
    mentionable: integer;
    permissions: integer;
  end;

  TKookGuildNotifyType = (kntDefault, kntAll, kntAtOnly, kntNo);
  TKookChannelType = (kctGroup = 0, kctText = 1, kctVoice = 2);
  TKookMessageType = (kmtText = 1, kmtImage = 2, kmtVideo = 3, kmtFile = 4, kmtAudio = 8,
    kmtKMarkdown = 9, kmtCard = 10, kmtSystem = 255);



  TKookChanel = record
    id: string;
    Name: string;
    user_id: string;
    guild_id: string;
    topic: string;
    is_category: boolean;
    parent_id: string;
    level: integer;
    slow_mode: integer;
    channel_type: TKookChannelType;
    permission_sync: integer;
    has_password: boolean;
    isDetailed: boolean;
    //TODO:more information
  end;

  TKookGuild = record
    id: string;
    Name: string;
    topic: string;
    user_id: string;
    icon: string;
    notify_type: TKookGuildNotifyType;
    region: string;
    enable_open: boolean;
    default_channel_id: string;
    welcome_channel_id: string;
    roles: array of TKookRole;
    channels: array of TKookChanel;
    isDetailed: boolean;
    boost_num: integer;
    level: integer;
  end;

  TKookUser = record
    id: string;
    username: string;
    nickname: string;
    identify_num: string;
    online: boolean;
    bot: boolean;
    status: integer;
    avatar: string;
    vip_avatar: string;
    mobile_verified: boolean;
    roles: array of integer;
    isDetailed: boolean;
  end;

  TKookQuote = record
    id: string;
    QuotedMessageType: integer;
    Content: string;
    Create_At: integer;
    author: TKookUser;
  end;

  TKookAttachments = record
    attachmentType: string;
    url: string;
    Name: string;
    size: integer;

  end;

  TKookMessage = record
    channel_type: string;
    message_type: TKookMessageType;
    target_id: string;
    author_id: string;
    content: string;
    msg_id: string;
    msg_timestamp: integer;
    nonce: string;
    extra: TJSONObject;//source data
    nonsys_guild_id: string;
    nonsys_channel_name: string;
    nonsys_mention: array of integer;
    nonsys_mention_all: boolean;
    nonsys_mention_roles: array of integer;
    nonsys_mention_here: boolean;
    nonsys_author: TKookUser;
    //TODO:System message
  end;

  TKEventMessage = procedure(Sender: TObject; Message: TKookMessage);

  TKookUserArray = array of TKookUser;
  TKookChannelArray = array of TKookChanel;

function GetKookAttachments(Data: TJSONObject): TKookAttachments;
function GetKookChannel(Data: TJSONObject): TKookChanel;
function GetKookGuild(Data: TJSONObject): TKookGuild;
function GetKookMessage(Data: TJSONObject): TKookMessage;
function GetKookQuote(Data: TJSONObject): TKookQuote;
function GetKookRole(Data: TJSONObject): TKookRole;
function GetKookUser(Data: TJSONObject): TKookUser;

implementation

function GetKookChannel(Data: TJSONObject): TKookChanel;
begin
  Result.channel_type := TKookChannelType(Data.Integers['type']);
  Result.guild_id := Data.Strings['guild_id'];
  Result.has_password := Data.Booleans['has_password'];
  Result.id := Data.Strings['id'];
  Result.is_category := Data.Booleans['is_category'];
  Result.level := Data.Integers['level'];
  Result.Name := Data.Strings['name'];
  Result.parent_id := Data.Strings['parent_id'];
  Result.permission_sync := Data.Integers['permission_sync'];
  Result.slow_mode := Data.Integers['slow_mode'];
  Result.topic := Data.Strings['topic'];
  Result.user_id := Data.Strings['user_id'];
end;

function GetKookRole(Data: TJSONObject): TKookRole;
begin
  Result.color := Data.Integers['color'];
  Result.hoist := Data.Integers['hoist'];
  Result.mentionable := Data.Integers['mentionable'];
  Result.Name := Data.Strings['name'];
  Result.permissions := Data.Integers['permissions'];
  Result.position := Data.Integers['position'];
  Result.role_id := Data.Integers['role_id'];
end;

function GetKookGuild(Data: TJSONObject): TKookGuild;
var
  ca: array of TKookChanel;
  ra: array of TKookRole;
  channels, roles: TJSONArray;
  i: integer;
begin
  if Data.Find('channels') = nil then
  begin
    Result.isDetailed := False;

    if Data.Find('boost_num') <> nil then Result.boost_num := Data.Integers['boost_num'];
    if Data.Find('level') <> nil then Result.boost_num := Data.Integers['level'];

  end
  else
  begin
    Result.isDetailed := True;

    channels := Data.Arrays['channels'];
    SetLength(ca, channels.Count);
    for i := 0 to Pred(channels.Count) do
    begin
      ca[i] := GetKookChannel(channels.Objects[i]);
    end;
    Result.channels := ca;


    roles := Data.Arrays['roles'];
    SetLength(ra, roles.Count);
    for i := 0 to Pred(roles.Count) do
    begin
      ra[i] := GetKookRole(roles.Objects[i]);
    end;
    Result.roles := ra;
  end;


  Result.default_channel_id := Data.Strings['default_channel_id'];
  Result.enable_open := Data.Booleans['enable_open'];
  Result.icon := Data.Strings['icon'];
  Result.id := Data.Strings['id'];
  Result.Name := Data.Strings['name'];
  Result.notify_type := TKookGuildNotifyType(Data.Integers['notify_type']);
  Result.region := Data.Strings['region'];



  Result.topic := Data.Strings['topic'];
  Result.user_id := Data.Strings['user_id'];
  Result.welcome_channel_id := Data.Strings['welcome_channel_id'];

  channels.Free;
  roles.Free;
end;

function GetKookUser(Data: TJSONObject): TKookUser;
var
  i: integer;
begin
  if Data.Find('mobile_verified') = nil then
  begin
    Result.isDetailed := False;
  end
  else
  begin
    Result.isDetailed := True;
    Result.mobile_verified := Data.Booleans['mobile_verified'];
  end;

  Result.id := Data.Strings['id'];
  Result.username := Data.Strings['username'];
  Result.nickname := Data.Strings['nickname'];
  Result.identify_num := Data.Strings['identify_num'];
  Result.online := Data.Booleans['online'];
  Result.bot := Data.Booleans['bot'];
  Result.status := Data.Integers['status'];
  Result.avatar := Data.Strings['avatar'];
  Result.vip_avatar := Data.Strings['vip_avatar'];


  SetLength(Result.roles, Data.Arrays['roles'].Count);
  for i := 0 to Pred(Data.Arrays['roles'].Count) do
  begin
    Result.roles[i] := Data.Arrays['roles'].Integers[i];
  end;

end;

function GetKookAttachments(Data: TJSONObject): TKookAttachments;
begin
  Result.attachmentType := Data.Strings['type'];
  Result.url := Data.Strings['url'];
  Result.Name := Data.Strings['name'];
  Result.size := Data.Integers['size'];
end;

function GetKookQuote(Data: TJSONObject): TKookQuote;
begin
  Result.id := Data.Strings['id'];
  Result.QuotedMessageType := Data.Integers['type'];
  Result.Content := Data.Strings['content'];
  Result.Create_At := Data.Integers['create_at'];
  Result.author := GetKookUser(Data.Objects['author']);
end;

function GetKookMessage(Data: TJSONObject): TKookMessage;
begin
  Result.channel_type := Data.Strings['channel_type'];
  Result.message_type := TKookMessageType(Data.Integers['type']);
  Result.target_id := Data.Strings['target_id'];
  Result.author_id := Data.Strings['author_id'];
  Result.content := Data.Strings['content'];
  Result.msg_id := Data.Strings['msg_id'];
  Result.msg_timestamp := Data.Integers['msg_timestamp'];
  Result.nonce := Data.Strings['nonce'];
  Result.extra := Data.Objects['extra'];
end;

end.
