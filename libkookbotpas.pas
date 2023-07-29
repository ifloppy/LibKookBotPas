unit libKookBotPas;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, WebsocketsClient, fphttpclient, jsonparser, fpjson, URIParser, opensslsockets, wsstream, openssl, wsmessages, TypInfo, kookstructureutils;

const
  KookHTTPBaseURL = 'https://www.kookapp.cn/api/v3';
  isDebug = True;

type
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
    function GetGateway(IsResume: boolean = False): boolean;
    procedure sendPingMessage();
    procedure wsHeartBeatThread();

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
  end;

  TObjProcedure = procedure of object;

  TWaitAsyncExecute = class(TThread)
  private
    TargetProc: TObjProcedure;
    ExecWaitTime: integer;
  public
    constructor Create(WaitTime: integer; TaskProc: TObjProcedure);
    procedure Execute; override;
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
  wsClient.Free;
  httpClient.Free;
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
var
  HeartBeatThread: TWaitAsyncExecute;
begin
  wsCommunicator.Free;

  isConnectionOK := False;
  statusCodeHello := 0;

  wsCommunicator := wsClient.Connect(OpenSSLSocketHandler);
  wsCommunicator.OnReceiveMessage := @wsReceiveMessageHandler;
  wsCommunicator.StartReceiveMessageThread;

  TWaitAsyncExecute.Create(36000, @wsHeartBeatThread);

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
  EventTypeID: Integer;
  MessageReceived: TKookMessage;
  buffer: string;
begin
  MsgList := TWebsocketMessageOwnerList.Create(True);
  wsCommunicator.GetUnprocessedMessages(MsgList);
  for m in msgList do
  begin
    if isDebug then
    begin
      WriteLn('Received message, type:' + GetEnumName(TypeInfo(TWebsocketMessageType), Ord(m.MessageType)));
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
      EventTypeID:=JSONObject.Integers['s'];
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
          MessageReceived:=kookstructureutils.GetKookMessage(JSONObject.Objects['d']);
          if isDebug then begin
            Str(MessageReceived.message_type, buffer);
            WriteLn('Message type:'+buffer);
            WriteLn('Message content:'+MessageReceived.content)
          end;

          if onKookMessage <> nil then onKookMessage(Self, MessageReceived);
        end;
        3: begin//pong message
          isGotPong:=true;
          if isDebug then WriteLn('Received pong message');
        end;
        5: begin//reconnect message
          if isDebug then begin
            WriteLn('Reconnect command received from Kook');
          end;
          isConnectionOK:=false;
          GetGateway(true);
          latestSN:=0;
        end;
        6: begin//resume ack message
          if isDebug then begin
            WriteLn('RESUME ACK');
          end;
        end;
      end;

      JSONObject.Free;
    end;
  end;
  msgList.Free;
end;

procedure TkookBot.sendPingMessage();
var
  s: String;
begin
  s:='{"s": 2,"sn": '+IntToStr(latestSN)+'}';
  wsCommunicator.WriteStringMessage(s);
  if isDebug then WriteLn('Send ping message');
end;

procedure TKookBot.wsHeartBeatThread();
begin
  while isConnectionOK do begin
    sendPingMessage();
    Sleep(6000);
    if not isGotPong then begin
      isConnectionOK:=false;
      if onDisconnect <> nil then onDisconnect(Self);
    end;
    isGotPong:=false;
    Sleep(30000);
  end;
end;

//TODO:PAGES
procedure TKookBot.GetBotJoinedGuilds();
var
  respStr: string;
  respObj: TJSONObject;
  i: integer;
begin

  respStr:=httpClient.Get(KookHTTPBaseURL+'/guild/list');
  if isDebug then WriteLn(respStr);
  respObj:=GetJSON(respStr) as TJSONObject;

  SetLength(BotJoinedGuilds, 0);

  with respObj.Objects['data'].Arrays['items'] do begin
    SetLength(BotJoinedGuilds, Count);
    for i := 0 to Pred(Count) do begin
      BotJoinedGuilds[i]:=GetKookGuild(Objects[i]);
    end;

  end;

  respObj.Free;
end;

function TKookBot.GetGuildDetail(GuildID: string): TKookGuild;
var
  rs: string;
  ro: TJSONObject;
begin
  rs := httpClient.Get(KookHTTPBaseURL+'/guild/view?guild_id='+GuildID);
  ro := GetJSON(rs) as TJSONObject;
  Result:=GetKookGuild(ro.Objects['data']);
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
  if ChannelID <> '' then channel_id_param:='&channel_id='+ChannelID;
  rs := httpClient.Get(KookHTTPBaseURL+'/guild/user-list?guild_id='+GuildID+channel_id_param);
  ro := GetJSON(rs) as TJSONObject;
  with ro.Objects['data'].Arrays['items'] do begin
    SetLength(ua, Count);
    for i := 0 to Pred(Count) do
    begin
      ua[i] := GetKookUser(Objects[i]);
    end;
  end;
  Result:=ua;
  ro.Free;
end;

constructor TWaitAsyncExecute.Create(WaitTime: integer; TaskProc: TObjProcedure);
begin
  inherited Create(False);
  ExecWaitTime := WaitTime;
  TargetProc := TaskProc;
  Self.FreeOnTerminate := True;
end;

procedure TWaitAsyncExecute.Execute();
begin
  Sleep(ExecWaitTime);
  TargetProc();

end;

end.
