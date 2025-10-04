unit TestAIHarness.View.Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  DelphiAIDev.Types, DelphiAIDev.Settings, DelphiAIDev.AI.Facade, DelphiAIDev.AI.Interfaces,
  DelphiAIDev.AI.Response, DelphiAIDev.AI.Gemini; // Added DelphiAIDev.AI.Gemini

type
  TFormTestAIHarness = class(TForm)
    pnlTop: TPanel;
    pnlClient: TPanel;
    pnlBottom: TPanel;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    cboAIProvider: TComboBox;
    Label2: TLabel;
    edtApiKey: TEdit;
    btnListModels: TButton;
    btnSend: TButton;
    GroupBox2: TGroupBox;
    mmPrompt: TMemo;
    GroupBox3: TGroupBox;
    mmResponse: TMemo;
    Splitter1: TSplitter;
    procedure FormCreate(Sender: TObject);
    procedure btnSendClick(Sender: TObject);
    procedure btnListModelsClick(Sender: TObject);
  private
    FSettings: TDelphiAIDevSettings;
    procedure SendRequest(const AProvider: TC4DAiAvailable; const AApiKey, AQuestion: string);
    procedure ListModelsForProvider(const AProvider: TC4DAiAvailable; const AApiKey: string);
  public
    { Public declarations }
  end;

var
  FormTestAIHarness: TFormTestAIHarness;

implementation

{$R *.dfm}

procedure TFormTestAIHarness.FormCreate(Sender: TObject);
var
  LProvider: TC4DAiAvailable;
begin
  FSettings := TDelphiAIDevSettings.Create;
  // We don't load from file, just use it as a data container.

  cboAIProvider.Items.Clear;
  for LProvider := Low(TC4DAiAvailable) to High(TC4DAiAvailable) do
    cboAIProvider.Items.Add(LProvider.ToString);
  cboAIProvider.ItemIndex := 0;

  mmPrompt.Text := 'Write a simple "Hello, World!" program in Delphi.';
end;

procedure TFormTestAIHarness.btnSendClick(Sender: TObject);
begin
  if Trim(edtApiKey.Text) = '' then
  begin
    ShowMessage('Please enter an API Key.');
    Exit;
  end;

  if Trim(mmPrompt.Text) = '' then
  begin
    ShowMessage('Please enter a prompt.');
    Exit;
  end;

  mmResponse.Text := 'Sending request...';
  Application.ProcessMessages;

  SendRequest(
    TC4DAiAvailable(cboAIProvider.ItemIndex),
    edtApiKey.Text,
    mmPrompt.Text
  );
end;

procedure TFormTestAIHarness.btnListModelsClick(Sender: TObject);
begin
  if Trim(edtApiKey.Text) = '' then
  begin
    ShowMessage('Please enter an API Key to list models.');
    Exit;
  end;

  mmResponse.Text := 'Listing models...';
  Application.ProcessMessages;

  ListModelsForProvider(
    TC4DAiAvailable(cboAIProvider.ItemIndex),
    edtApiKey.Text
  );
end;

procedure TFormTestAIHarness.SendRequest(const AProvider: TC4DAiAvailable;
  const AApiKey, AQuestion: string);
var
  LAIFacade: TDelphiAIDevAIFacade;
  LResponse: IDelphiAIDevAIResponse;
  Cont1:TStringList;
begin
  // Set settings dynamically for the test
  case AProvider of
    TC4DAiAvailable.Gemini: FSettings.ApiKeyGemini := AApiKey;
    TC4DAiAvailable.OpenAI: FSettings.ApiKeyOpenAI := AApiKey;
    TC4DAiAvailable.Groq: FSettings.ApiKeyGroq := AApiKey;
    TC4DAiAvailable.Ollama: FSettings.ApiKeyOllama := AApiKey;
  end;
  FSettings.AIDefault := AProvider;

  LAIFacade := TDelphiAIDevAIFacade.Create;
  Cont1:= TStringList.Create;
  try
    LResponse := LAIFacade.AiUse(AProvider).ProcessSend(AQuestion).Response;
    Cont1.Assign(LResponse.GetContent);
    mmResponse.Text :=Cont1.Text;
    if LResponse.GetStatusCode <> 200 then
    begin
        mmResponse.Text := 'Status Code: ' + IntToStr(LResponse.GetStatusCode) + #13#10 + mmResponse.Text;
    end;
  finally
    Cont1.Free;
    LAIFacade.Free;
  end;
end;

procedure TFormTestAIHarness.ListModelsForProvider(const AProvider: TC4DAiAvailable;
  const AApiKey: string);
var
  LGemini: IDelphiAIDevAI;
  LResponse: IDelphiAIDevAIResponse;
  LModels: string;
begin
  if AProvider <> TC4DAiAvailable.Gemini then
  begin
    mmResponse.Text := 'ListModels is only implemented for the Gemini provider in this plugin.';
    Exit;
  end;

  FSettings.ApiKeyGemini := AApiKey;
  FSettings.AIDefault := AProvider;

  LResponse := TDelphiAIDevAIResponse.New;
  LGemini := TDelphiAIDevAIGemini.Create(FSettings, LResponse);
  try
    LModels := LGemini.ListModels;
    mmResponse.Text := 'Available Models:' + #13#10 + LModels;
  finally
    // LGemini is an interface, so no need to free it.
  end;
end;

end.
