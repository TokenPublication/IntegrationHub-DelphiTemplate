{*******************************************************************************
* TokenX Connect (Wired) / Integration Hub - Delphi Interface
*
* Description:
*   This unit implements a comprehensive Delphi wrapper for the TokenX Connect
*   (also known as Integration Hub) library. It facilitates the integration of
*   point-of-sale (POS) devices with Delphi applications using the native C++
*   DLL provided by the TokenX platform.
*
* Platform/Language:
*   - Platform: Microsoft Windows
*   - Language: Object Pascal (Delphi)
*   - Implementation: VCL (Visual Component Library) application
* 
* Main Features:
*   - Dynamic loading of required DLLs at runtime
*   - Asynchronous connection to POS terminals
*   - Event-driven architecture with callback-based device state monitoring
*   - JSON-based transaction and payment processing
*   - Fiscal information retrieval from POS devices
*   - Automatic reconnection on connection loss
*   - Thread-safe communication with the UI
*
* Technical Implementation:
*   - Singleton pattern for POS communication manager
*   - DLL function pointer resolution via GetProcAddress
*   - Asynchronous operations using System.Threading
*   - Callback routing from C++ to Delphi event model
*   - Comprehensive error handling and logging
*
* Dependencies:
*   - IntegrationHubCpp.dll - Main integration library
*   - libcrypto-3.dll - Cryptography support library
*   - libusb-1.0.dll - USB communication library
*   - zlib1.dll - Compression library
*
* Usage:
*   This wrapper simplifies the complex process of POS integration by providing
*   a clean Object Pascal interface to the native C++ DLL. It handles memory
*   management, thread synchronization, and error handling automatically.
*******************************************************************************}

unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, System.IOUtils,
  System.Generics.Collections, System.JSON, System.Math, System.Threading;

const
  // Name of the main DLL to be loaded
  DLL_NAME = 'IntegrationHubCpp.dll';
  
  // List of all required DLLs that must be loaded for the integration to work
  REQUIRED_DLLS: array[0..3] of string = (
    'libcrypto-3.dll',    // Cryptography support library
    'libusb-1.0.dll',     // USB communication library
    'zlib1.dll',          // Compression library
    'IntegrationHubCpp.dll' // Main integration DLL
  );

type
  // Forward declaration for ConnectionWrapper pointer
  // This represents a handle to the connection established with the POS device
  PConnectionWrapper = type Pointer;
  
  // Callback types for receiving notifications from the POS device
  // SerialIn callback is triggered when data is received from the device
  TSerialInCallbackFunc = procedure(TypeCode: Integer; Value: PWideChar); stdcall;
  
  // DeviceState callback is triggered when the connection state with the device changes
  TDeviceStateCallbackFunc = procedure(IsConnected: Boolean; Id: PWideChar); stdcall;

  // Forward declare TForm1 for the callbacks
  TForm1 = class;

  // DLL function type declarations - these match the exported functions in the DLL
  // Each function is declared with its parameter types and calling convention
  TCreateCommunication = function(CompanyName: PWideChar): PConnectionWrapper; stdcall;
  TDeleteCommunication = procedure(Ptr: PConnectionWrapper); stdcall;
  TReconnect = procedure(Ptr: PConnectionWrapper); stdcall;
  TGetActiveDeviceIndex = function(Ptr: PConnectionWrapper): Integer; stdcall;
  TSendBasket = function(Ptr: PConnectionWrapper; JsonData: PWideChar): Integer; stdcall;
  TSendPayment = function(Ptr: PConnectionWrapper; JsonData: PWideChar): Integer; stdcall;
  TGetFiscalInfo = function(Ptr: PConnectionWrapper): PWideChar; stdcall;
  TSetSerialInCallback = procedure(Ptr: PConnectionWrapper; Callback: TSerialInCallbackFunc); stdcall;
  TSetDeviceStateCallback = procedure(Ptr: PConnectionWrapper; Callback: TDeviceStateCallbackFunc); stdcall;

  {*
   * TPOSCommunication
   * 
   * A class that handles communication with POS devices through the IntegrationHubCpp DLL.
   * This class implements a singleton pattern - only one instance can exist.
   * It manages DLL loading/unloading, connection establishment, and transaction processing.
   *}
  TPOSCommunication = class
  private
    class var
      FInstance: TPOSCommunication;  // Singleton instance
      FForm: TForm1;                 // Reference to the form for UI updates
      FSerialInEvent: TNotifyEvent;  // Event handler for serial data received
      FDeviceStateEvent: TNotifyEvent; // Event handler for device state changes
  private
    FDllHandle: THandle;             // Handle to the main DLL
    FDllHandles: TDictionary<string, THandle>; // Dictionary of all loaded DLL handles
    FConnection: PConnectionWrapper; // Pointer to the connection wrapper
    FCompanyName: string;            // Company name used for authentication
    FTask: ITask;                    // Task for asynchronous connection
    FConnecting: Boolean;            // Flag indicating a connection attempt in progress
    
    // DLL function pointers - these will be assigned addresses from the loaded DLL
    FCreateCommunication: TCreateCommunication;
    FDeleteCommunication: TDeleteCommunication;
    FReconnect: TReconnect;
    FGetActiveDeviceIndex: TGetActiveDeviceIndex;
    FSendBasket: TSendBasket;
    FSendPayment: TSendPayment;
    FGetFiscalInfo: TGetFiscalInfo;
    FSetSerialInCallback: TSetSerialInCallback;
    FSetDeviceStateCallback: TSetDeviceStateCallback;
    
    {* Loads the main IntegrationHubCpp DLL *}
    procedure LoadDll;
    
    {* Unloads the main DLL *}
    procedure UnloadDll;
    
    {* Loads all required DLLs listed in REQUIRED_DLLS array *}
    procedure LoadRequiredDlls;
    
    {* Unloads all previously loaded DLLs *}
    procedure UnloadAllDlls;
    
    {* Initializes function pointers by getting their addresses from the loaded DLL *}
    procedure InitializeFunctions;
    
    {* Static callback method for receiving serial data from the device
     * @param TypeCode Type of the data received
     * @param Value The actual data received as a wide string
     *}
    class procedure SerialInCallback(TypeCode: Integer; Value: PWideChar); stdcall; static;
    
    {* Static callback method for receiving device state changes
     * @param IsConnected Boolean indicating if device is connected
     * @param Id Device identifier as a wide string
     *}
    class procedure DeviceStateCallback(IsConnected: Boolean; Id: PWideChar); stdcall; static;
    
    {* Internal method that performs the actual connection process *}
    procedure DoConnect;
  public
    {* Creates a new instance of TPOSCommunication
     * @param AForm Reference to the main form for UI updates
     * @param ACompanyName Company name used for authentication with the POS system
     *}
    constructor Create(AForm: TForm1; const ACompanyName: string);
    
    {* Destroys the TPOSCommunication instance and cleans up resources *}
    destructor Destroy; override;
    
    {* Provides access to the singleton instance *}
    class property Instance: TPOSCommunication read FInstance;
    
    {* Initiates an asynchronous connection to the POS device *}
    procedure Connect;
    
    {* Disconnects from the POS device and releases resources *}
    procedure Disconnect;
    
    {* Attempts to reconnect to the POS device if already connected,
     * otherwise initiates a new connection
     *}
    procedure Reconnect;
    
    {* Gets the index of the currently active device
     * @return Index of the active device or -1 if no device is active
     *}
    function GetActiveDeviceIndex: Integer;
    
    {* Sends a basket of items to the POS device for processing
     * @param JsonData JSON string containing basket data (items, prices, etc.)
     * @return Response code from the POS device
     *}
    function SendBasket(const JsonData: string): Integer;
    
    {* Sends a payment request to the POS device
     * @param JsonData JSON string containing payment data (amount, type, etc.)
     * @return Response code from the POS device
     *}
    function SendPayment(const JsonData: string): Integer;
    
    {* Retrieves fiscal information from the POS device
     * @return JSON string containing fiscal information
     *}
    function GetFiscalInfo: string;
    
    {* Sets the callback event handlers for serial data and device state changes
     * @param ASerialInEvent Event handler for serial data received
     * @param ADeviceStateEvent Event handler for device state changes
     *}
    procedure SetCallbacks(const ASerialInEvent, ADeviceStateEvent: TNotifyEvent);
    
    {* Indicates whether a connection attempt is currently in progress *}
    property Connecting: Boolean read FConnecting;
  end;

  {*
   * TForm1
   * 
   * The main form of the application that provides a UI for interacting with the POS device.
   * It displays logs, allows sending test transactions, and shows connection status.
   *}
  TForm1 = class(TForm)
    btnSendBasket: TButton;       // Button to send a test basket
    btnSendPayment: TButton;      // Button to send a test payment
    btnGetFiscalInfo: TButton;    // Button to get fiscal information
    memLog: TMemo;                // Memo component for displaying logs
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnSendBasketClick(Sender: TObject);
    procedure btnSendPaymentClick(Sender: TObject);
    procedure btnGetFiscalInfoClick(Sender: TObject);
  private
    FPOSComm: TPOSCommunication;  // Reference to the POS communication object
    FLastSerialValue: string;     // Last value received from the serial callback
    FLastDeviceId: string;        // Last device ID received from the device state callback
    FLastTypeCode: Integer;       // Last type code received from the serial callback
    FIsConnected: Boolean;        // Flag indicating if a device is connected
    
    {* Adds a timestamped message to the log
     * @param Msg The message to add to the log
     *}
    procedure Log(const Msg: string);
    
    {* Event handler for when serial data is received from the device *}
    procedure OnSerialIn(Sender: TObject);
    
    {* Event handler for when the device connection state changes *}
    procedure OnDeviceState(Sender: TObject);
    
    {* Updates button enabled states based on connection status *}
    procedure UpdateButtons;
  public
    {* Last value received from the serial callback *}
    property LastSerialValue: string read FLastSerialValue write FLastSerialValue;
    
    {* Last device ID received from the device state callback *}
    property LastDeviceId: string read FLastDeviceId write FLastDeviceId;
    
    {* Last type code received from the serial callback *}
    property LastTypeCode: Integer read FLastTypeCode write FLastTypeCode;
    
    {* Flag indicating if a device is connected *}
    property IsConnected: Boolean read FIsConnected write FIsConnected;
  end;

var
  Form1: TForm1;  // Global Form1 instance

implementation

{$R *.dfm}

{ TPOSCommunication }

{*
 * Creates a new instance of TPOSCommunication
 * 
 * @param AForm Reference to the main form for UI updates
 * @param ACompanyName Company name used for authentication with the POS system
 * 
 * @throws Exception if another instance already exists (enforcing singleton pattern)
 *}
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

{*
 * Destroys the TPOSCommunication instance and cleans up resources
 * 
 * This includes disconnecting from any active device, unloading all DLLs,
 * and freeing all allocated memory.
 *}
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

{*
 * Loads all required DLLs listed in REQUIRED_DLLS array
 * 
 * The method attempts to load each DLL from either the application directory
 * or the Win32\Debug subdirectory. If a DLL fails to load, an exception is thrown.
 * 
 * @throws Exception if any required DLL fails to load
 *}
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

{*
 * Unloads all previously loaded DLLs
 * 
 * This method releases all DLL handles in the FDllHandles dictionary
 * and clears the dictionary.
 *}
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

{*
 * Loads the main IntegrationHubCpp DLL
 * 
 * Note: The actual loading is done in LoadRequiredDlls, this method
 * just retrieves the handle from the dictionary.
 * 
 * @throws Exception if the main DLL handle cannot be retrieved
 *}
procedure TPOSCommunication.LoadDll;
begin
  // Main DLL is already loaded in LoadRequiredDlls, just get its handle
  if not FDllHandles.TryGetValue(DLL_NAME, FDllHandle) then
  begin
    raise Exception.CreateFmt('Failed to get handle for %s', [DLL_NAME]);
  end;
  InitializeFunctions;
end;

{*
 * Unloads the main DLL
 * 
 * This method clears the DLL handle. Actual unloading is done in UnloadAllDlls.
 *}
procedure TPOSCommunication.UnloadDll;
begin
  FDllHandle := 0; // Will be freed in UnloadAllDlls
end;

{*
 * Initializes function pointers by getting their addresses from the loaded DLL
 * 
 * This method uses GetProcAddress to obtain the address of each exported function
 * from the DLL and assigns them to the corresponding function pointers.
 * 
 * @throws Exception if any required function cannot be found in the DLL
 *}
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

{*
 * Static callback method for receiving serial data from the device
 * 
 * This method is called by the DLL when data is received from the POS device.
 * It queues the event to be processed on the main thread.
 * 
 * @param TypeCode Type of the data received
 * @param Value The actual data received as a wide string
 *}
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

{*
 * Static callback method for receiving device state changes
 * 
 * This method is called by the DLL when the connection state with the POS device changes.
 * It queues the event to be processed on the main thread.
 * 
 * @param IsConnected Boolean indicating if device is connected
 * @param Id Device identifier as a wide string
 *}
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

{*
 * Initiates an asynchronous connection to the POS device
 * 
 * This method starts a background task that attempts to establish
 * a connection with the POS device. It does nothing if already connected
 * or if a connection attempt is in progress.
 *}
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

{*
 * Internal method that performs the actual connection process
 * 
 * This method creates a communication instance, sets up callbacks,
 * and initializes the connection with the POS device.
 * 
 * @throws Exception if the connection cannot be established
 *}
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

{*
 * Disconnects from the POS device and releases resources
 * 
 * This method calls the DLL's delete function to close the connection
 * and release any resources associated with it.
 *}
procedure TPOSCommunication.Disconnect;
begin
  if FConnection <> nil then
  begin
    FDeleteCommunication(FConnection);
    FConnection := nil;
  end;
end;

{*
 * Attempts to reconnect to the POS device
 * 
 * If not already connected, this initiates a new connection.
 * If already connected, it calls the DLL's reconnect function.
 *}
procedure TPOSCommunication.Reconnect;
begin
  if FConnection = nil then
    Connect
  else
    FReconnect(FConnection);
end;

{*
 * Gets the index of the currently active device
 * 
 * @return Index of the active device
 * @throws Exception if not connected to a device
 *}
function TPOSCommunication.GetActiveDeviceIndex: Integer;
begin
  if FConnection = nil then
    raise Exception.Create('Not connected');
  Result := FGetActiveDeviceIndex(FConnection);
end;

{*
 * Sends a basket of items to the POS device for processing
 * 
 * @param JsonData JSON string containing basket data (items, prices, etc.)
 * @return Response code from the POS device
 * @throws Exception if not connected to a device
 *}
function TPOSCommunication.SendBasket(const JsonData: string): Integer;
begin
  if FConnection = nil then
    raise Exception.Create('Not connected');
  Result := FSendBasket(FConnection, PWideChar(WideString(JsonData)));
end;

{*
 * Sends a payment request to the POS device
 * 
 * @param JsonData JSON string containing payment data (amount, type, etc.)
 * @return Response code from the POS device
 * @throws Exception if not connected to a device
 *}
function TPOSCommunication.SendPayment(const JsonData: string): Integer;
begin
  if FConnection = nil then
    raise Exception.Create('Not connected');
  Result := FSendPayment(FConnection, PWideChar(WideString(JsonData)));
end;

{*
 * Retrieves fiscal information from the POS device
 * 
 * @return JSON string containing fiscal information
 * @throws Exception if not connected to a device
 *}
function TPOSCommunication.GetFiscalInfo: string;
begin
  if FConnection = nil then
    raise Exception.Create('Not connected');
  Result := string(WideString(FGetFiscalInfo(FConnection)));
end;

{*
 * Sets the callback event handlers for serial data and device state changes
 * 
 * @param ASerialInEvent Event handler for serial data received
 * @param ADeviceStateEvent Event handler for device state changes
 *}
procedure TPOSCommunication.SetCallbacks(const ASerialInEvent, ADeviceStateEvent: TNotifyEvent);
begin
  FSerialInEvent := ASerialInEvent;
  FDeviceStateEvent := ADeviceStateEvent;
end;

{ TForm1 }

{*
 * Initializes the form when it's created
 * 
 * This method disables buttons, initializes the POS communication object,
 * sets up callbacks, and attempts an initial connection.
 *}
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

{*
 * Cleans up resources when the form is destroyed
 *}
procedure TForm1.FormDestroy(Sender: TObject);
begin
    try
    // First disable callbacks to prevent further messages from being queued
    if Assigned(FPOSComm) then
    begin
      Log('Disconnecting from device...');
      // Set callbacks to nil to prevent further events from being processed
      FPOSComm.SetCallbacks(nil, nil);
      // Explicitly disconnect before freeing
      FPOSComm.Disconnect;

      // Give background threads a moment to complete before unloading DLLs
      Sleep(500); // 500ms delay

      Log('Releasing communication resources...');
      FPOSComm.Free;
      FPOSComm := nil;
    end;
  except
    on E: Exception do
    begin
      Log('Error during cleanup: ' + E.Message);
    end;
  end;
end;

{*
 * Adds a timestamped message to the log
 * 
 * @param Msg The message to add to the log
 *}
procedure TForm1.Log(const Msg: string);
begin
  memLog.Lines.Add(Format('[%s] %s', [FormatDateTime('yyyy-mm-dd hh:nn:ss', Now), Msg]));
end;

{*
 * Event handler for when serial data is received from the device
 * 
 * This method logs the received data with its type code.
 *}
procedure TForm1.OnSerialIn(Sender: TObject);
begin
  Log(Format('Serial In - Type: %d, Value: %s', [FLastTypeCode, FLastSerialValue]));
end;

{*
 * Event handler for when the device connection state changes
 * 
 * This method logs the state change, updates the UI, and initiates
 * automatic reconnection if the device is disconnected.
 *}
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

{*
 * Updates button enabled states based on connection status
 * 
 * This method enables or disables buttons based on whether a device
 * is connected and whether a connection attempt is in progress.
 * It also updates the form caption to reflect the current state.
 *}
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

{*
 * Handler for the SendBasket button click
 * 
 * This method sends a sample basket to the POS device and logs the result.
 * The sample basket is a JSON string containing document type, amount, and customer info.
 *}
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

{*
 * Handler for the SendPayment button click
 * 
 * This method sends a sample payment to the POS device and logs the result.
 * The sample payment is a JSON string containing amount and payment type.
 *}
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

{*
 * Handler for the GetFiscalInfo button click
 * 
 * This method retrieves fiscal information from the POS device and logs it.
 *}
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