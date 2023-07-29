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
  TKookChannelType = (kctText = 1, kctVoice = 2);
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
    channeltype: TKookChannelType;
    permission_sync: integer;
    has_password: boolean;
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

function GetKookAttachments(Data: TJSONObject): TKookAttachments;
function GetKookChannel(Data: TJSONObject): TKookChanel;
function GetKookGuild(Data: TJSONObject): TKookGuild;
function GetKookMessage(Data: TJSONObject): TKookMessage;
function GetKookQuote(Data: TJSONObject): TKookQuote;
function GetKookRole(Data: TJSONObject): TKookRole;
function GetKookUser(Data: TJSONObject): TKookUser;

implementation

function GetKookChannel(Data: TJSONObject): TKookChanel;
var
  kc: TKookChanel;
begin
  kc.channeltype := TKookChannelType(Data.Integers['type']);
  kc.guild_id := Data.Strings['guild_id'];
  kc.has_password := Data.Booleans['has_password'];
  kc.id := Data.Strings['id'];
  kc.is_category := Data.Booleans['is_category'];
  kc.level := Data.Integers['level'];
  kc.Name := Data.Strings['name'];
  kc.parent_id := Data.Strings['parent_id'];
  kc.permission_sync := Data.Integers['permission_sync'];
  kc.slow_mode := Data.Integers['slow_mode'];
  kc.topic := Data.Strings['topic'];
  kc.user_id := Data.Strings['user_id'];
  exit(kc);
end;

function GetKookRole(Data: TJSONObject): TKookRole;
var
  r: TKookRole;
begin
  r.color := Data.Integers['color'];
  r.hoist := Data.Integers['hoist'];
  r.mentionable := Data.Integers['mentionable'];
  r.Name := Data.Strings['name'];
  r.permissions := Data.Integers['permissions'];
  r.position := Data.Integers['position'];
  r.role_id := Data.Integers['role_id'];
  Exit(r);
end;

function GetKookGuild(Data: TJSONObject): TKookGuild;
var
  g: TKookGuild;
  ca: array of TKookChanel;
  ra: array of TKookRole;
  channels, roles: TJSONArray;
  i: integer;
begin
  if Data.Find('channels') = nil then
  begin
    g.isDetailed:=false;

    if Data.Find('boost_num') <> nil then g.boost_num:=Data.Integers['boost_num'];
    if Data.Find('level') <> nil then g.boost_num:=Data.Integers['level'];


  end else begin
    g.isDetailed:=true;

    channels := Data.Arrays['channels'];
    SetLength(ca, channels.Count);
    for i := 0 to Pred(channels.Count) do
    begin
      ca[i] := GetKookChannel(channels.Objects[i]);
    end;
    g.channels := ca;


    roles := Data.Arrays['roles'];
    SetLength(ra, roles.Count);
    for i := 0 to Pred(roles.Count) do
    begin
      ra[i] := GetKookRole(roles.Objects[i]);
    end;
    g.roles := ra;
  end;


  g.default_channel_id := Data.Strings['default_channel_id'];
  g.enable_open := Data.Booleans['enable_open'];
  g.icon := Data.Strings['icon'];
  g.id := Data.Strings['id'];
  g.Name := Data.Strings['name'];
  g.notify_type := TKookGuildNotifyType(Data.Integers['notify_type']);
  g.region := Data.Strings['region'];



  g.topic := Data.Strings['topic'];
  g.user_id := Data.Strings['user_id'];
  g.welcome_channel_id := Data.Strings['welcome_channel_id'];
  exit(g);
end;

function GetKookUser(Data: TJSONObject): TKookUser;
var
  u: TKookUser;
  i: integer;
begin
  if Data.Find('mobile_verified') = nil then begin
    u.isDetailed:=false;
  end else begin
    u.isDetailed:=true;
    u.mobile_verified := Data.Booleans['mobile_verified'];
  end;

  u.id := Data.Strings['id'];
  u.username := Data.Strings['username'];
  u.nickname := Data.Strings['nickname'];
  u.identify_num := Data.Strings['identify_num'];
  u.online := Data.Booleans['online'];
  u.bot := Data.Booleans['bot'];
  u.status := Data.Integers['status'];
  u.avatar := Data.Strings['avatar'];
  u.vip_avatar := Data.Strings['vip_avatar'];


  SetLength(u.roles, Data.Arrays['roles'].Count);
  for i := 0 to Pred(Data.Arrays['roles'].Count) do
  begin
    u.roles[i] := Data.Arrays['roles'].Integers[i];
  end;

  exit(u);

end;

function GetKookAttachments(Data: TJSONObject): TKookAttachments;
var
  a: TKookAttachments;
begin
  a.attachmentType := Data.Strings['type'];
  a.url := Data.Strings['url'];
  a.Name := Data.Strings['name'];
  a.size := Data.Integers['size'];
  exit(a);
end;

function GetKookQuote(Data: TJSONObject): TKookQuote;
var
  q: TKookQuote;
begin
  q.id := Data.Strings['id'];
  q.QuotedMessageType := Data.Integers['type'];
  q.Content := Data.Strings['content'];
  q.Create_At := Data.Integers['create_at'];
  q.author := GetKookUser(Data.Objects['author']);
  exit(q);
end;

function GetKookMessage(Data: TJSONObject): TKookMessage;
var
  m: TKookMessage;
begin
  m.channel_type := Data.Strings['channel_type'];
  m.message_type := TKookMessageType(Data.Integers['type']);
  m.target_id := Data.Strings['target_id'];
  m.author_id := Data.Strings['author_id'];
  m.content := Data.Strings['content'];
  m.msg_id := Data.Strings['msg_id'];
  m.msg_timestamp := Data.Integers['msg_timestamp'];
  m.nonce := Data.Strings['nonce'];
  m.extra := Data.Objects['extra'];
  exit(m);
end;

end.
