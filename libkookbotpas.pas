unit libKookBotPas;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, WebsocketsClient, fphttpclient, jsonparser, fpjson, URIParser, opensslsockets, wsstream, openssl, wsmessages, kookstructureutils;

const
  KookHTTPBaseURL = 'https://www.kookapp.cn/api/v3';
  isDebug = True;

type
  TObjProcedure = procedure of object;

  THeartBeatThread = class(TThread)
  private
    instance: TObject;
  public
    constructor Create(Sender: TObject);
    procedure Execute; override;
    destructor Destroy; override;
  end;


  TKookBot = class(TObject)
  private
    wsClient: TWebsocketClient;
    httpClient: TFPHTTPClient;
    latestSN: integer;
    wsCommunicator: TWebsocketCommunicator;
    OpenSSLSocketHandler: TOpenSSLSocketHandler;
    GatewayURL: string;
    isConnectionOK, isGotPong: boolean;
    statusCodeHello: integer;
    HeartBeatThreadObj: THeartBeatThread;
    function GetGateway(IsResume: boolean = False): boolean;


    procedure wsHandlerHandshakeSuccess(Sender: TObject; const Data: TResponseData);
    procedure wsHandlerHandshakeFailure(Sender: TObject; const Data: TResponseData);
    procedure wsReceiveMessageHandler(Sender: TObject);
  public
    onKookMessage: TKEventMessage;
    onDisconnect: TNotifyEvent;

    BotJoinedGuilds: array of TKookGuild;

    constructor Create(BotToken: string; LatestSNCode: integer = 0);
    destructor Destroy; override;

    procedure Connect();//Connect to websocket

    procedure GetBotJoinedGuilds();
    function GetGuildDetail(GuildID: string): TKookGuild;
    function GetGuildUsers(GuildID: string; ChannelID: string = ''): TKookUserArray;
    function ChangeGuildNickName(GuildID: string; nickname: string = ''; user_id: string = ''): boolean;
    function LeaveGuild(GuildID: string): boolean;
    function KickGuild(GuildID: string; UserID: string): boolean;
    function GetChannelList(GuildID: string): TKookChannelArray;
    function GetChannelDetail(ChannelID: string): TKookChanel;
    //TODO:Create,Edit,Delete channel
    //TODO:List,View channel Message
    function SendTextMessage(ChannelID: string; content: string; QuoteID: string = ''; TempTargetID: string = ''; FinalMessageID: TStringStream = nil): boolean;
    function UpdateTextMessage(MessageID: string; content: string): boolean;
    function DeleteTextMessage(MessageID: string): boolean;
    //TODO:Get reaction of message
    function AddMessageReaction(MessageID: string; emoji: string): boolean;
    function RemoveMessageReaction(MessageID: string; emoji: string; UserID: string = ''): boolean;

    //PM
    //TODO: All pm function

    //TODO: User api

    //麻了你Kook功能真鸡巴多，待我慢慢来写

  end;




implementation

constructor TKookBot.Create(BotToken: string; LatestSNCode: integer = 0);
begin
  InitSSLInterface; //OpenSSL library required

  //Prepare for websocket
  wsClient := TWebsocketClient.Create('', 443);
  wsClient.OnHandshakeSuccess := @wsHandlerHandshakeSuccess;
  wsClient.OnHandshakeFailure := @wsHandlerHandshakeFailure;
  OpenSSLSocketHandler := TOpenSSLSocketHandler.Create;
  isConnectionOK := False;

  //Prepare for http client
  httpClient := TFPHTTPClient.Create(nil);
  httpClient.AddHeader('Accept', 'application/json');
  httpClient.AddHeader('Content-Type', 'application/json');
  httpClient.AddHeader('User-Agent', 'libKookBotPas');
  httpClient.AddHeader('Authorization', 'Bot ' + BotToken);

  //For auto resuming session
  latestSN := LatestSNCode;


  GetGateway();

  //if isDebug then WriteLn('Gateway:' + GatewayURL);

  Connect();
end;

destructor TKookBot.Destroy();
begin
  //Free objects to prevent mem leak
  isConnectionOK := False;
  HeartBeatThreadObj.Free;


  wsCommunicator.StopReceiveMessageThread;
  wsCommunicator.WriteMessage(wmtClose).Free;
  while wsCommunicator.ReceiveMessageThreadRunning do
    Sleep(10);
  wsCommunicator.Free;
  wsClient.Free;

  httpClient.Free;
  OpenSSLSocketHandler.Free;

end;

function TKookBot.GetGateway(IsResume: boolean = False): boolean;
  //Save full gateway url to var GatewayURL
var
  resp: string;
  respJSON: TJSONObject;
  ResumeParam: string;
  URI: TURI;
begin
  if IsResume then ResumeParam := '&resume=1&sn=' + IntToStr(latestSN);
  try
    resp := httpClient.Get(KookHTTPBaseURL + '/gateway/index?compress=0' + ResumeParam);
  except
    exit(False);
  end;
  respJSON := GetJSON(resp) as TJSONObject;
  GatewayURL := respJSON.Objects['data'].Strings['url'];

  URI := ParseURI(GatewayURL, 'wss', 443, True);
  wsClient.Host := URI.Host;
  wsClient.Path := URI.Path + URI.Document + '?' + URI.Params;
  if isDebug then
  begin
    WriteLn('ws host:' + wsClient.Host);
    WriteLn('ws path:' + wsClient.Path);
    WriteLn('ws port:' + IntToStr(wsClient.Port));
  end;

  respJSON.Free;
  exit(True);
end;

procedure TKookBot.Connect();
//Try to connect to ws server
begin
  wsCommunicator.Free;

  isConnectionOK := False;
  statusCodeHello := 0;

  wsCommunicator := wsClient.Connect(OpenSSLSocketHandler);
  wsCommunicator.OnReceiveMessage := @wsReceiveMessageHandler;
  wsCommunicator.StartReceiveMessageThread;

  HeartBeatThreadObj := THeartBeatThread.Create(Self);

end;

procedure TKookBot.wsHandlerHandshakeSuccess(Sender: TObject; const Data: TResponseData);
begin
  if isDebug then
  begin
    WriteLn('WS Handshake success:' + Data.Content);
    WriteLn('Status:' + IntToStr(Data.StatusCode));
  end;
  isConnectionOK := True;
end;

procedure TKookBot.wsHandlerHandshakeFailure(Sender: TObject; const Data: TResponseData);
begin
  if isDebug then
  begin
    WriteLn('WS Handshake failure:' + Data.Content);
    WriteLn('Status:' + IntToStr(Data.StatusCode));
  end;
  isConnectionOK := False;
end;

procedure TKookBot.wsReceiveMessageHandler(Sender: TObject);
var
  msgList: TWebsocketMessageOwnerList;
  m: TWebsocketMessage;//processing message
  JSONObject: TJSONObject;
  EventTypeID: integer;
  MessageReceived: TKookMessage;
  buffer: string;
begin
  MsgList := TWebsocketMessageOwnerList.Create(True);
  wsCommunicator.GetUnprocessedMessages(MsgList);
  for m in msgList do
  begin
    if isDebug then
    begin
      Str(m.MessageType, buffer);
      WriteLn('Received message, type:' + buffer);
      if m is TWebsocketStringMessage then
      begin
        WriteLn(TWebsocketStringMessage(m).Data);
      end;
      if m is TWebsocketPongMessage then
      begin
        WriteLn(TWebsocketPongMessage(m).Data);
      end;
    end;

    if m is TWebsocketStringMessage then
    begin

      JSONObject := GetJSON(TWebsocketStringMessage(m).Data) as TJSONObject;
      EventTypeID := JSONObject.Integers['s'];
      case EventTypeID of
        1: begin//Hello message
          if JSONObject.Objects['d'].Integers['code'] = 0 then
          begin
            isConnectionOK := True;
          end
          else
          begin
            statusCodeHello := JSONObject.Objects['d'].Integers['code'];
          end;
        end;
        0: begin//event
          MessageReceived := kookstructureutils.GetKookMessage(JSONObject.Objects['d']);
          if isDebug then
          begin
            Str(MessageReceived.message_type, buffer);
            WriteLn('Message type:' + buffer);
            WriteLn('Message content:' + MessageReceived.content);
          end;

          if onKookMessage <> nil then onKookMessage(Self, MessageReceived);

          //update SN
          latestSN := JSONObject.Integers['sn'];
        end;
        3: begin//pong message
          isGotPong := True;
          if isDebug then WriteLn('Received pong message');
        end;
        5: begin//reconnect message
          if isDebug then
          begin
            WriteLn('Reconnect command received from Kook');
          end;
          isConnectionOK := False;
          GetGateway(True);
          latestSN := 0;
        end;
        6: begin//resume ack message
          if isDebug then
          begin
            WriteLn('RESUME ACK');
          end;
        end;
        else
          begin
            if isDebug then begin
              Write('Unknown message type:', JSONObject.Integers['s']);
            end;
          end;
      end;

      JSONObject.Free;
    end;
  end;
  msgList.Free;
end;



//TODO:PAGES
procedure TKookBot.GetBotJoinedGuilds();
var
  respStr: string;
  respObj: TJSONObject;
  i: integer;
begin

  respStr := httpClient.Get(KookHTTPBaseURL + '/guild/list');
  if isDebug then WriteLn(respStr);
  respObj := GetJSON(respStr) as TJSONObject;

  SetLength(BotJoinedGuilds, 0);

  with respObj.Objects['data'].Arrays['items'] do
  begin
    SetLength(BotJoinedGuilds, Count);
    for i := 0 to Pred(Count) do
    begin
      BotJoinedGuilds[i] := GetKookGuild(Objects[i]);
    end;

  end;

  respObj.Free;
end;

function TKookBot.GetGuildDetail(GuildID: string): TKookGuild;
var
  rs: string;
  ro: TJSONObject;
begin
  rs := httpClient.Get(KookHTTPBaseURL + '/guild/view?guild_id=' + GuildID);
  ro := GetJSON(rs) as TJSONObject;
  Result := GetKookGuild(ro.Objects['data']);
  ro.Free;
end;

//TODO:PAGES
function TKookBot.GetGuildUsers(GuildID: string; ChannelID: string = ''): TKookUserArray;
var
  rs: string;
  ro: TJSONObject;
  channel_id_param: string;
  ua: TKookUserArray;
  i: integer;
begin
  if ChannelID <> '' then channel_id_param := '&channel_id=' + ChannelID;
  rs := httpClient.Get(KookHTTPBaseURL + '/guild/user-list?guild_id=' + GuildID + channel_id_param);
  ro := GetJSON(rs) as TJSONObject;
  with ro.Objects['data'].Arrays['items'] do
  begin
    SetLength(ua, Count);
    for i := 0 to Pred(Count) do
    begin
      ua[i] := GetKookUser(Objects[i]);
    end;
  end;
  Result := ua;
  ro.Free;
end;

function TKookBot.ChangeGuildNickName(GuildID: string; nickname: string = ''; user_id: string = ''): boolean;
var
  rs: string;
  ro, bo: TJSONObject;
  bodyStream: TStringStream;
begin

  if (Length(nickname) < 2) and (nickname <> '') then Exit(False);
  if Length(nickname) > 64 then Exit(False);

  bo := TJSONObject.Create();
  bo.Add('guild_id', GuildID);
  if nickname <> '' then bo.Add('nickname', nickname);
  if user_id <> '' then bo.Add('user_id', user_id);

  bodyStream := TStringStream.Create(bo.AsJSON);

  httpClient.RequestBody := bodyStream;

  try
    rs := httpClient.Post(KookHTTPBaseURL + '/guild/nickname');
  except

  end;
  httpClient.RequestBody := nil;
  ro := GetJSON(rs) as TJSONObject;
  if isDebug then WriteLn('Change guild nick name status:', ro.Integers['code']);
  Result := (ro.Integers['code'] = 0);
  bodyStream.Free;
  ro.Free;
  bo.Free;

end;


function TKookBot.LeaveGuild(GuildID: string): boolean;
var
  rs: string;
  ro, bo: TJSONObject;
  bs: TStringStream;
begin
  bo := TJSONObject.Create();
  bo.Add('guild_id', GuildID);
  bs := TStringStream.Create(bo.AsJSON);
  httpClient.RequestBody := bs;
  try
    rs := httpClient.Post(KookHTTPBaseURL + '/guild/leave');
  except

  end;
  httpClient.RequestBody := nil;
  ro := GetJSON(rs) as TJSONObject;
  Result := (ro.Integers['code'] = 0);
  bs.Free;
  ro.Free;
  bo.Free;
end;

function TKookBot.KickGuild(GuildID: string; UserID: string): boolean;
var
  rs: string;
  ro, bo: TJSONObject;
  bs: TStringStream;
begin
  bo := TJSONObject.Create();
  bo.Add('guild_id', GuildID);
  bo.Add('target_id', UserID);
  bs := TStringStream.Create(bo.AsJSON);
  httpClient.RequestBody := bs;
  try
    rs := httpClient.Post(KookHTTPBaseURL + '/guild/kickout');
  except

  end;
  httpClient.RequestBody := nil;
  ro := GetJSON(rs) as TJSONObject;
  Result := (ro.Integers['code'] = 0);
  bs.Free;
  ro.Free;
  bo.Free;
end;

function TKookBot.GetChannelList(GuildID: string): TKookChannelArray;
var
  respStr: string;
  respObj: TJSONObject;
  i: integer;
  KookChannelArray: TKookChannelArray;
begin

  respStr := httpClient.Get(KookHTTPBaseURL + '/channel/list?guild_id=' + GuildID);
  if isDebug then WriteLn(respStr);
  respObj := GetJSON(respStr) as TJSONObject;

  SetLength(KookChannelArray, 0);

  with respObj.Objects['data'].Arrays['items'] do
  begin
    SetLength(KookChannelArray, Count);
    for i := 0 to Pred(Count) do
    begin
      KookChannelArray[i] := GetKookChannel(Objects[i]);
    end;

  end;
  Result:=KookChannelArray;
  respObj.Free;
end;

function TKookBot.GetChannelDetail(ChannelID: string): TKookChanel;
var
  rs: string;
  ro: TJSONObject;
begin
  rs := httpClient.Get(KookHTTPBaseURL + '/channel/view?guild_id=' + ChannelID);
  ro := GetJSON(rs) as TJSONObject;
  Result := GetKookChannel(ro.Objects['data']);
  ro.Free;
end;

function TKookBot.SendTextMessage(ChannelID: string; content: string; QuoteID: string = ''; TempTargetID: string = ''; FinalMessageID: TStringStream = nil): boolean;
var
  rs: string;
  ro, bo: TJSONObject;
  bs: TStringStream;
begin
  bo := TJSONObject.Create();
  bo.Add('target_id', ChannelID);
  bo.Add('content', content);
  if QuoteID <> '' then bo.Add('quote', QuoteID);
  if TempTargetID <> '' then bo.Add('temp_target_id', TempTargetID);
  //Final message id: save message id. REMEMBER TO FREE THIS OBJECT
  bs := TStringStream.Create(bo.AsJSON);
  httpClient.RequestBody := bs;
  try
    rs := httpClient.Post(KookHTTPBaseURL + '/message/create');
  except

  end;
  httpClient.RequestBody := nil;
  ro := GetJSON(rs) as TJSONObject;
  Result := (ro.Integers['code'] = 0);
  if Assigned(FinalMessageID) then FinalMessageID.WriteString(ro.Objects['data'].Strings['msg_id']);
  bs.Free;
  ro.Free;
  bo.Free;

end;

function TKookBot.UpdateTextMessage(MessageID: string; content: string): boolean;
var
  rs: string;
  ro, bo: TJSONObject;
  bs: TStringStream;
begin
  bo := TJSONObject.Create();
  bo.Add('msg_id', MessageID);
  bo.Add('content', content);
  bs := TStringStream.Create(bo.AsJSON);
  httpClient.RequestBody := bs;
  try
    rs := httpClient.Post(KookHTTPBaseURL + '/message/update');
  except

  end;
  httpClient.RequestBody := nil;
  ro := GetJSON(rs) as TJSONObject;
  Result := (ro.Integers['code'] = 0);
  bs.Free;
  ro.Free;
  bo.Free;
end;

function TKookBot.DeleteTextMessage(MessageID: string): boolean;
var
  rs: string;
  ro, bo: TJSONObject;
  bs: TStringStream;
begin
  bo := TJSONObject.Create();
  bo.Add('msg_id', MessageID);
  bs := TStringStream.Create(bo.AsJSON);
  httpClient.RequestBody := bs;
  try
    rs := httpClient.Post(KookHTTPBaseURL + '/message/delete');
  except

  end;
  httpClient.RequestBody := nil;
  ro := GetJSON(rs) as TJSONObject;
  Result := (ro.Integers['code'] = 0);
  bs.Free;
  ro.Free;
  bo.Free;
end;

function TKookBot.AddMessageReaction(MessageID: string; emoji: string): boolean;
var
  rs: string;
  ro, bo: TJSONObject;
  bs: TStringStream;
begin
  bo := TJSONObject.Create();
  bo.Add('msg_id', MessageID);
  bo.Add('emoji', emoji);
  bs := TStringStream.Create(bo.AsJSON);
  httpClient.RequestBody := bs;
  try
    rs := httpClient.Post(KookHTTPBaseURL + '/message/add-reaction');
  except

  end;
  httpClient.RequestBody := nil;
  ro := GetJSON(rs) as TJSONObject;
  Result := (ro.Integers['code'] = 0);
  bs.Free;
  ro.Free;
  bo.Free;
end;

function TKookBot.RemoveMessageReaction(MessageID: string; emoji: string; UserID: string = ''): boolean;
var
  rs: string;
  ro, bo: TJSONObject;
  bs: TStringStream;
begin
  bo := TJSONObject.Create();
  bo.Add('msg_id', MessageID);
  bo.Add('emoji', emoji);
  if UserID <> '' then bo.Add('user_id', UserID);
  bs := TStringStream.Create(bo.AsJSON);
  httpClient.RequestBody := bs;
  try
    rs := httpClient.Post(KookHTTPBaseURL + '/message/delete-reaction');
  except

  end;
  httpClient.RequestBody := nil;
  ro := GetJSON(rs) as TJSONObject;
  Result := (ro.Integers['code'] = 0);
  bs.Free;
  ro.Free;
  bo.Free;
end;

constructor THeartBeatThread.Create(Sender: TObject);
begin
  inherited Create(False);
  Self.FreeOnTerminate := True;
  instance := Sender;
end;

procedure THeartBeatThread.Execute();
var
  s: string;
  i: integer;
begin
  with instance as TKookBot do
  begin
    while isConnectionOK do
    begin
      for i := 0 to 30 do
      begin
        if not isConnectionOK then exit;
        Sleep(1000);
      end;

      if not isConnectionOK then exit;
      s := '{"s": 2,"sn": ' + IntToStr(latestSN) + '}';
      wsCommunicator.WriteStringMessage(s);
      if isDebug then WriteLn('Send ping message:', s);
    end;
  end;

end;

destructor THeartBeatThread.Destroy;
begin
  instance := nil;
end;

end.
