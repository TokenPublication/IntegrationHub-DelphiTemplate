unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, System.IOUtils,
  System.Generics.Collections, System.JSON, System.Math, System.Threading;

const
  DLL_NAME = 'IntegrationHubCpp.dll';
  REQUIRED_DLLS: array[0..3] of string = (
    'libcrypto-3.dll',
    'libusb-1.0.dll',
    'zlib1.dll',
    'IntegrationHubCpp.dll'
  );

type
  // Forward declaration for ConnectionWrapper pointer
  PConnectionWrapper = type Pointer;
  
  // Callback types
  TSerialInCallbackFunc = procedure(TypeCode: Integer; Value: PWideChar); stdcall;
  TDeviceStateCallbackFunc = procedure(IsConnected: Boolean; Id: PWideChar); stdcall;

  // Forward declare TForm1 for the callbacks
  TForm1 = class;

  // DLL function types
  TCreateCommunication = function(CompanyName: PWideChar): PConnectionWrapper; stdcall;
  TDeleteCommunication = procedure(Ptr: PConnectionWrapper); stdcall;
  TReconnect = procedure(Ptr: PConnectionWrapper); stdcall;
  TGetActiveDeviceIndex = function(Ptr: PConnectionWrapper): Integer; stdcall;
  TSendBasket = function(Ptr: PConnectionWrapper; JsonData: PWideChar): Integer; stdcall;
  TSendPayment = function(Ptr: PConnectionWrapper; JsonData: PWideChar): Integer; stdcall;
  TGetFiscalInfo = function(Ptr: PConnectionWrapper): PWideChar; stdcall;
  TSetSerialInCallback = procedure(Ptr: PConnectionWrapper; Callback: TSerialInCallbackFunc); stdcall;
  TSetDeviceStateCallback = procedure(Ptr: PConnectionWrapper; Callback: TDeviceStateCallbackFunc); stdcall;

  TPOSCommunication = class
  private
    class var
      FInstance: TPOSCommunication;
      FForm: TForm1;
      FSerialInEvent: TNotifyEvent;
      FDeviceStateEvent: TNotifyEvent;
  private
    FDllHandle: THandle;
    FDllHandles: TDictionary<string, THandle>;
    FConnection: PConnectionWrapper;
    FCompanyName: string;
    FTask: ITask;
    FConnecting: Boolean;
    
    // DLL function pointers
    FCreateCommunication: TCreateCommunication;
    FDeleteCommunication: TDeleteCommunication;
    FReconnect: TReconnect;
    FGetActiveDeviceIndex: TGetActiveDeviceIndex;
    FSendBasket: TSendBasket;
    FSendPayment: TSendPayment;
    FGetFiscalInfo: TGetFiscalInfo;
    FSetSerialInCallback: TSetSerialInCallback;
    FSetDeviceStateCallback: TSetDeviceStateCallback;
    
    procedure LoadDll;
    procedure UnloadDll;
    procedure LoadRequiredDlls;
    procedure UnloadAllDlls;
    procedure InitializeFunctions;
    class procedure SerialInCallback(TypeCode: Integer; Value: PWideChar); stdcall; static;
    class procedure DeviceStateCallback(IsConnected: Boolean; Id: PWideChar); stdcall; static;
    procedure DoConnect;
  public
    constructor Create(AForm: TForm1; const ACompanyName: string);
    destructor Destroy; override;
    class property Instance: TPOSCommunication read FInstance;
    
    procedure Connect;
    procedure Disconnect;
    procedure Reconnect;
    function GetActiveDeviceIndex: Integer;
    function SendBasket(const JsonData: string): Integer;
    function SendPayment(const JsonData: string): Integer;
    function GetFiscalInfo: string;
    procedure SetCallbacks(const ASerialInEvent, ADeviceStateEvent: TNotifyEvent);
    property Connecting: Boolean read FConnecting;
  end;

  TForm1 = class(TForm)
    btnSendBasket: TButton;
    btnSendPayment: TButton;
    btnGetFiscalInfo: TButton;
    memLog: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnSendBasketClick(Sender: TObject);
    procedure btnSendPaymentClick(Sender: TObject);
    procedure btnGetFiscalInfoClick(Sender: TObject);
  private
    FPOSComm: TPOSCommunication;
    FLastSerialValue: string;
    FLastDeviceId: string;
    FLastTypeCode: Integer;
    FIsConnected: Boolean;
    procedure Log(const Msg: string);
    procedure OnSerialIn(Sender: TObject);
    procedure OnDeviceState(Sender: TObject);
    procedure UpdateButtons;
  public
    property LastSerialValue: string read FLastSerialValue write FLastSerialValue;
    property LastDeviceId: string read FLastDeviceId write FLastDeviceId;
    property LastTypeCode: Integer read FLastTypeCode write FLastTypeCode;
    property IsConnected: Boolean read FIsConnected write FIsConnected;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

{ TPOSCommunication }

constructor TPOSCommunication.Create(AForm: TForm1; const ACompanyName: string);
begin
  inherited Create;
  if FInstance <> nil then
    raise Exception.Create('Only one instance of TPOSCommunication is allowed');
    
  FDllHandles := TDictionary<string, THandle>.Create;
  FInstance := Self;
  FForm := AForm;
  FCompanyName := ACompanyName;
  FConnection := nil;
  FConnecting := False;
  
  // Load DLLs first
  if Assigned(FForm) then
    FForm.Log('Loading required DLLs...');
  LoadRequiredDlls;
  
  // Initialize main DLL functions
  if Assigned(FForm) then
    FForm.Log('Initializing DLL functions...');
  LoadDll;
  
  if Assigned(FForm) then
    FForm.Log('Initialization complete');
end;

destructor TPOSCommunication.Destroy;
begin
  Disconnect;
  UnloadDll;
  UnloadAllDlls;
  FDllHandles.Free;
  if FInstance = Self then
    FInstance := nil;
  inherited;
end;

procedure TPOSCommunication.LoadRequiredDlls;
var
  DllName: string;
  DllHandle: THandle;
  DllPath: string;
  SearchPaths: TArray<string>;
  Path: string;
  LoadedCount: Integer;
begin
  // Define search paths - current directory and Win32\Debug
  SearchPaths := [
    ExtractFilePath(Application.ExeName),
    TPath.Combine(ExtractFilePath(Application.ExeName), 'Win32\Debug')
  ];

  LoadedCount := 0;
  for DllName in REQUIRED_DLLS do
  begin
    DllHandle := 0;
    
    // Try loading from each search path
    for Path in SearchPaths do
    begin
      DllPath := TPath.Combine(Path, DllName);
      if FileExists(DllPath) then
      begin
        if Assigned(FForm) then
          FForm.Log(Format('Loading %s from %s...', [DllName, Path]));
          
        DllHandle := LoadLibrary(PChar(DllPath));
        if DllHandle <> 0 then
        begin
          FDllHandles.Add(DllName, DllHandle);
          Inc(LoadedCount);
          if Assigned(FForm) then
            FForm.Log(Format('Successfully loaded %s', [DllName]));
          Break;
        end;
      end;
    end;

    if DllHandle = 0 then
    begin
      if Assigned(FForm) then
        FForm.Log(Format('Failed to load %s. Error: %s', [DllName, SysErrorMessage(GetLastError)]));
      raise Exception.CreateFmt('Failed to load required DLL: %s', [DllName]);
    end;
  end;

  if Assigned(FForm) then
    FForm.Log(Format('Successfully loaded %d DLLs', [LoadedCount]));
end;

procedure TPOSCommunication.UnloadAllDlls;
var
  Pair: TPair<string, THandle>;
begin
  for Pair in FDllHandles do
  begin
    if Pair.Value <> 0 then
    begin
      FreeLibrary(Pair.Value);
      if Assigned(FForm) then
        FForm.Log(Format('Unloaded %s', [Pair.Key]));
    end;
  end;
  FDllHandles.Clear;
end;

procedure TPOSCommunication.LoadDll;
begin
  // Main DLL is already loaded in LoadRequiredDlls, just get its handle
  if not FDllHandles.TryGetValue(DLL_NAME, FDllHandle) then
  begin
    raise Exception.CreateFmt('Failed to get handle for %s', [DLL_NAME]);
  end;
  InitializeFunctions;
end;

procedure TPOSCommunication.UnloadDll;
begin
  FDllHandle := 0; // Will be freed in UnloadAllDlls
end;

procedure TPOSCommunication.InitializeFunctions;
begin
  @FCreateCommunication := GetProcAddress(FDllHandle, 'createCommunication');
  @FDeleteCommunication := GetProcAddress(FDllHandle, 'deleteCommunication');
  @FReconnect := GetProcAddress(FDllHandle, 'reconnect');
  @FGetActiveDeviceIndex := GetProcAddress(FDllHandle, 'getActiveDeviceIndex');
  @FSendBasket := GetProcAddress(FDllHandle, 'sendBasket');
  @FSendPayment := GetProcAddress(FDllHandle, 'sendPayment');
  @FGetFiscalInfo := GetProcAddress(FDllHandle, 'getFiscalInfo');
  @FSetSerialInCallback := GetProcAddress(FDllHandle, 'setSerialInCallback');
  @FSetDeviceStateCallback := GetProcAddress(FDllHandle, 'setDeviceStateCallback');

  if not Assigned(FCreateCommunication) or
     not Assigned(FDeleteCommunication) or
     not Assigned(FReconnect) or
     not Assigned(FGetActiveDeviceIndex) or
     not Assigned(FSendBasket) or
     not Assigned(FSendPayment) or
     not Assigned(FGetFiscalInfo) or
     not Assigned(FSetSerialInCallback) or
     not Assigned(FSetDeviceStateCallback) then
  begin
    raise Exception.Create('Failed to initialize one or more DLL functions');
  end;
end;

class procedure TPOSCommunication.SerialInCallback(TypeCode: Integer; Value: PWideChar); stdcall;
begin
  if Assigned(FInstance) and Assigned(FForm) and Assigned(FSerialInEvent) then
  begin
    FForm.LastTypeCode := TypeCode;
    FForm.LastSerialValue := Value;
    TThread.Queue(nil,
      procedure
      begin
        FSerialInEvent(FInstance);
      end);
  end;
end;

class procedure TPOSCommunication.DeviceStateCallback(IsConnected: Boolean; Id: PWideChar); stdcall;
begin
  if Assigned(FInstance) and Assigned(FForm) and Assigned(FDeviceStateEvent) then
  begin
    FForm.IsConnected := IsConnected;
    FForm.LastDeviceId := Id;
    TThread.Queue(nil,
      procedure
      begin
        FDeviceStateEvent(FInstance);
      end);
  end;
end;

procedure TPOSCommunication.Connect;
begin
  if FConnection <> nil then
    Exit;
    
  if FConnecting then
  begin
    if Assigned(FForm) then
      FForm.Log('Connection attempt already in progress...');
    Exit;
  end;

  FConnecting := True;
  if Assigned(FForm) then
  begin
    FForm.Log('Connecting...');
    TThread.Queue(nil, FForm.UpdateButtons);
  end;

  FTask := TTask.Create(procedure
    begin
      try
        DoConnect;
        if Assigned(FForm) then
        begin
          TThread.Queue(nil, procedure
            begin
              FForm.Log('Connected successfully');
              FConnecting := False;
              FForm.UpdateButtons;
            end);
        end;
      except
        on E: Exception do
        begin
          if Assigned(FForm) then
          begin
            TThread.Queue(nil, procedure
              begin
                FForm.Log('Error connecting: ' + E.Message);
                FConnecting := False;
                FForm.UpdateButtons;
              end);
          end;
        end;
      end;
    end);
  FTask.Start;
end;

procedure TPOSCommunication.DoConnect;
begin
  if Assigned(FForm) then
    TThread.Queue(nil, procedure
    begin
      FForm.Log('Creating communication instance...');
    end);
    
  FConnection := FCreateCommunication(PWideChar(WideString(FCompanyName)));
  if FConnection = nil then
    raise Exception.Create('Failed to create communication');

  if Assigned(FForm) then
    TThread.Queue(nil, procedure
    begin
      FForm.Log('Setting up callbacks...');
    end);
    
  FSetSerialInCallback(FConnection, SerialInCallback);
  FSetDeviceStateCallback(FConnection, DeviceStateCallback);
  
  if Assigned(FForm) then
    TThread.Queue(nil, procedure
    begin
      FForm.Log('Connection setup complete');
    end);
end;

procedure TPOSCommunication.Disconnect;
begin
  if FConnection <> nil then
  begin
    FDeleteCommunication(FConnection);
    FConnection := nil;
  end;
end;

procedure TPOSCommunication.Reconnect;
begin
  if FConnection = nil then
    Connect
  else
    FReconnect(FConnection);
end;

function TPOSCommunication.GetActiveDeviceIndex: Integer;
begin
  if FConnection = nil then
    raise Exception.Create('Not connected');
  Result := FGetActiveDeviceIndex(FConnection);
end;

function TPOSCommunication.SendBasket(const JsonData: string): Integer;
begin
  if FConnection = nil then
    raise Exception.Create('Not connected');
  Result := FSendBasket(FConnection, PWideChar(WideString(JsonData)));
end;

function TPOSCommunication.SendPayment(const JsonData: string): Integer;
begin
  if FConnection = nil then
    raise Exception.Create('Not connected');
  Result := FSendPayment(FConnection, PWideChar(WideString(JsonData)));
end;

function TPOSCommunication.GetFiscalInfo: string;
begin
  if FConnection = nil then
    raise Exception.Create('Not connected');
  Result := string(WideString(FGetFiscalInfo(FConnection)));
end;

procedure TPOSCommunication.SetCallbacks(const ASerialInEvent, ADeviceStateEvent: TNotifyEvent);
begin
  FSerialInEvent := ASerialInEvent;
  FDeviceStateEvent := ADeviceStateEvent;
end;

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  // Disable buttons initially
  btnSendBasket.Enabled := False;
  btnSendPayment.Enabled := False;
  btnGetFiscalInfo.Enabled := False;

  memLog.Clear;
  try
    Log('Initializing POS Communication...');
    FPOSComm := TPOSCommunication.Create(Self, 'YourCompanyName');
    FPOSComm.SetCallbacks(OnSerialIn, OnDeviceState);
    
    // Attempt initial connection
    Log('Attempting initial connection...');
    FPOSComm.Connect;
  except
    on E: Exception do
    begin
      Log('Error initializing: ' + E.Message);
      Application.Terminate;
    end;
  end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FPOSComm.Free;
end;

procedure TForm1.Log(const Msg: string);
begin
  memLog.Lines.Add(Format('[%s] %s', [FormatDateTime('yyyy-mm-dd hh:nn:ss', Now), Msg]));
end;

procedure TForm1.OnSerialIn(Sender: TObject);
begin
  Log(Format('Serial In - Type: %d, Value: %s', [FLastTypeCode, FLastSerialValue]));
end;

procedure TForm1.OnDeviceState(Sender: TObject);
begin
  Log(Format('Device State - Connected: %s, ID: %s', [BoolToStr(FIsConnected, True), FLastDeviceId]));
  UpdateButtons;
  
  // Auto-reconnect if disconnected
  if not FIsConnected and not FPOSComm.Connecting then
  begin
    Log('Connection lost. Attempting to reconnect...');
    FPOSComm.Connect;
  end;
end;

procedure TForm1.UpdateButtons;
begin
  btnSendBasket.Enabled := FIsConnected and not FPOSComm.Connecting;
  btnSendPayment.Enabled := FIsConnected and not FPOSComm.Connecting;
  btnGetFiscalInfo.Enabled := FIsConnected and not FPOSComm.Connecting;
  
  if FIsConnected then
    Caption := 'POS Communication Demo - Connected'
  else if FPOSComm.Connecting then
    Caption := 'POS Communication Demo - Connecting...'
  else
    Caption := 'POS Communication Demo - Disconnected';
end;

procedure TForm1.btnSendBasketClick(Sender: TObject);
const
  SampleBasket = '{"documentType":9008,"taxFreeAmount":5000,"customerInfo":{"taxID":"11111111111"},"paymentItems":[{"amount":5000,"description":"Nakit","type":1}]}';
begin
  try
    var Response := FPOSComm.SendBasket(SampleBasket);
    Log(Format('Basket sent successfully. Response: %d', [Response]));
  except
    on E: Exception do
      Log('Error sending basket: ' + E.Message);
  end;
end;

procedure TForm1.btnSendPaymentClick(Sender: TObject);
const
  SamplePayment = '{"amount":10.99,"type":"credit"}';
begin
  try
    var Response := FPOSComm.SendPayment(SamplePayment);
    Log(Format('Payment sent successfully. Response: %d', [Response]));
  except
    on E: Exception do
      Log('Error sending payment: ' + E.Message);
  end;
end;

procedure TForm1.btnGetFiscalInfoClick(Sender: TObject);
begin
  try
    var Info := FPOSComm.GetFiscalInfo;
    Log('Fiscal Info: ' + Info);
  except
    on E: Exception do
      Log('Error getting fiscal info: ' + E.Message);
  end;
end;

end.
