import os
import textwrap

# --- File Content Definitions ---

DPR_CONTENT = """
program TestAIHarness;

uses
  Vcl.Forms,
  TestAIHarness.View.Main in 'Views\\TestAIHarness.View.Main.pas' {FormTestAIHarness};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormTestAIHarness, FormTestAIHarness);
  Application.Run;
end.
"""

DFM_CONTENT = """
object FormTestAIHarness: TFormTestAIHarness
  Left = 0
  Top = 0
  Caption = 'Delphi AI Developer - Test Harness'
  ClientHeight = 550
  ClientWidth = 700
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  TextHeight = 13
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 700
    Height = 135
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object GroupBox1: TGroupBox
      Left = 0
      Top = 0
      Width = 700
      Height = 135
      Align = alClient
      Caption = 'Configuration'
      TabOrder = 0
      object Label1: TLabel
        Left = 16
        Top = 24
        Width = 60
        Height = 13
        Caption = 'AI Provider'
      end
      object Label2: TLabel
        Left = 16
        Top = 72
        Width = 41
        Height = 13
        Caption = 'API Key'
      end
      object cboAIProvider: TComboBox
        Left = 16
        Top = 40
        Width = 250
        Height = 21
        Style = csDropDownList
        TabOrder = 0
      end
      object edtApiKey: TEdit
        Left = 16
        Top = 88
        Width = 500
        Height = 21
        TabOrder = 1
      end
      object btnListModels: TButton
        Left = 530
        Top = 87
        Width = 150
        Height = 25
        Caption = 'List Models'
        TabOrder = 2
        OnClick = btnListModelsClick
      end
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 515
    Width = 700
    Height = 35
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    object btnSend: TButton
      Left = 594
      Top = 6
      Width = 90
      Height = 25
      Caption = 'Send Request'
      TabOrder = 0
      OnClick = btnSendClick
    end
  end
  object pnlClient: TPanel
    Left = 0
    Top = 135
    Width = 700
    Height = 380
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object Splitter1: TSplitter
      Left = 0
      Top = 180
      Width = 700
      Height = 8
      Cursor = crVSplit
      Align = alTop
      ExplicitTop = 157
      ExplicitWidth = 698
    end
    object GroupBox2: TGroupBox
      Left = 0
      Top = 0
      Width = 700
      Height = 180
      Align = alTop
      Caption = 'Prompt'
      TabOrder = 0
      object mmPrompt: TMemo
        Left = 2
        Top = 15
        Width = 696
        Height = 163
        Align = alClient
        ScrollBars = ssVertical
        TabOrder = 0
      end
    end
    object GroupBox3: TGroupBox
      Left = 0
      Top = 188
      Width = 700
      Height = 192
      Align = alClient
      Caption = 'Response'
      TabOrder = 1
      object mmResponse: TMemo
        Left = 2
        Top = 15
        Width = 696
        Height = 175
        Align = alClient
        ReadOnly = True
        ScrollBars = ssVertical
        TabOrder = 0
      end
    end
  end
end
"""

PAS_CONTENT = """
unit TestAIHarness.View.Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  DelphiAIDev.Types, DelphiAIDev.Settings, DelphiAIDev.AI.Facade, DelphiAIDev.AI.Interfaces,
  DelphiAIDev.AI.Response;

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
begin
  // Set settings dynamically for the test
  case AProvider of
    TC4DAiAvailable.Gemini: FSettings.ApiKeyGemini := AApiKey;
    TC4DAiAvailable.ChatGPT: FSettings.ApiKeyOpenAI := AApiKey;
    TC4DAiAvailable.Groq: FSettings.ApiKeyGroq := AApiKey;
    TC4DAiAvailable.Ollama: FSettings.ApiKeyOllama := AApiKey;
  end;
  FSettings.AIDefault := AProvider;

  LResponse := TDelphiAIDevAIResponse.Create;
  LAIFacade := TDelphiAIDevAIFacade.Create(FSettings, LResponse);
  try
    LAIFacade.GetResponse(AQuestion);
    mmResponse.Text := LResponse.GetContentText;
    if LResponse.GetStatusCode <> 200 then
    begin
        mmResponse.Text := 'Status Code: ' + IntToStr(LResponse.GetStatusCode) + #13#10 + mmResponse.Text;
    end;
  finally
    LAIFacade.Free;
  end;
end;

procedure TFormTestAIHarness.ListModelsForProvider(const AProvider: TC4DAiAvailable;
  const AApiKey: string);
var
  LAIFacade: TDelphiAIDevAIFacade;
  LResponse: IDelphiAIDevAIResponse;
  LModels: string;
begin
  // Only Gemini is expected to work, others will return empty.
  if AProvider <> TC4DAiAvailable.Gemini then
  begin
    mmResponse.Text := 'ListModels is only implemented for the Gemini provider in this plugin.';
    Exit;
  end;

  FSettings.ApiKeyGemini := AApiKey;
  FSettings.AIDefault := AProvider;
  
  LResponse := TDelphiAIDevAIResponse.Create;
  LAIFacade := TDelphiAIDevAIFacade.Create(FSettings, LResponse);
  try
    LModels := LAIFacade.ListModels;
    mmResponse.Text := 'Available Models:' + #13#10 + LModels;
  finally
    LAIFacade.Free;
  end;
end;

end.
"""

DPROJ_CONTENT = """
<Project xmlns="http://schemas.microsoft.com/developer/msbuild/2003">
    <PropertyGroup>
        <ProjectGuid>{GUID}</ProjectGuid>
        <ProjectVersion>19.5</ProjectVersion>
        <FrameworkType>VCL</FrameworkType>
        <MainSource>TestAIHarness.dpr</MainSource>
        <Base>True</Base>
        <Config Condition="'$(Config)'==''">Debug</Config>
        <Platform Condition="'$(Platform)'==''">Win32</Platform>
        <TargetedPlatforms>1</TargetedPlatforms>
        <AppType>Application</AppType>
    </PropertyGroup>
    <PropertyGroup Condition="'$(Config)'=='Base' or '$(Base)'!=''">
        <Base>true</Base>
    </PropertyGroup>
    <PropertyGroup Condition="('$(Platform)'=='Win32' and '$(Base)'=='true') or '$(Base_Win32)'!=''">
        <Base_Win32>true</Base_Win32>
        <CfgParent>Base</CfgParent>
        <Base>true</Base>
    </PropertyGroup>
    <PropertyGroup Condition="'$(Config)'=='Debug' or '$(Cfg_1)'!=''">
        <Cfg_1>true</Cfg_1>
        <CfgParent>Base</CfgParent>
        <Base>true</Base>
    </PropertyGroup>
    <PropertyGroup Condition="('$(Platform)'=='Win32' and '$(Cfg_1)'=='true') or '$(Cfg_1_Win32)'!=''">
        <Cfg_1_Win32>true</Cfg_1_Win32>
        <CfgParent>Cfg_1</CfgParent>
        <Cfg_1>true</Cfg_1>
        <Base>true</Base>
    </PropertyGroup>
    <PropertyGroup Condition="'$(Config)'=='Release' or '$(Cfg_2)'!=''">
        <Cfg_2>true</Cfg_2>
        <CfgParent>Base</CfgParent>
        <Base>true</Base>
    </PropertyGroup>
    <PropertyGroup Condition="'$(Config)'=='Base' or '$(Base)'!=''">
        <DCC_UnitSearchPath>..\\Src;..\\Src\\AI;..\\Src\\Chat;..\\Src\\CodeCompletion;..\\Src\\Conn;..\\Src\\Consts;..\\Src\\DB;..\\Src\\DB\\Chat;..\\Src\\DB\\References;..\\Src\\DB\\Registers;..\\Src\\DB\\Utils;..\\Src\\DefaultsQuestions;..\\Src\\IDE;..\\Src\\IDE\\ImageListMain;..\\Src\\IDE\\NTAEditViewNotifier;..\\Src\\IDE\\OTAEditorNotifier;..\\Src\\IDE\\OTAIDENotifier;..\\Src\\IDE\\Splash;..\\Src\\Interfaces;..\\Src\\KeyboardBinding;..\\Src\\MainMenu;..\\Src\\MetaInfo;..\\Src\\ModuleCreator;..\\Src\\PopupMenuProjects;..\\Src\\Projects;..\\Src\\Settings;..\\Src\\Test;..\\Src\\Types;..\\Src\\Utils;..\\Src\\View;..\\Src\\WaitingScreen;..\\Package\\modules\\restrequest4delphi\\src;$(DCC_UnitSearchPath)</DCC_UnitSearchPath>
        <DCC_Namespace>System;Xml;Data;Datasnap;Web;Soap;Vcl;Vcl.Imaging;Vcl.Touch;Vcl.Samples;Vcl.Shell;$(DCC_Namespace)</DCC_Namespace>
        <SanitizedProjectName>TestAIHarness</SanitizedProjectName>
        <DCC_E>false</DCC_E>
        <DCC_F>false</DCC_F>
        <DCC_K>false</DCC_K>
        <DCC_N>true</DCC_N>
        <DCC_S>false</DCC_S>
        <DCC_ImageBase>00400000</DCC_ImageBase>
        <VerInfo_Locale>1033</VerInfo_Locale>
        <VerInfo_Keys>CompanyName=;FileDescription=$(MSBuildProjectName);FileVersion=1.0.0.0;InternalName=;LegalCopyright=;LegalTrademarks=;OriginalFilename=;ProgramID=com.embarcadero.$(MSBuildProjectName);ProductName=$(MSBuildProjectName);ProductVersion=1.0.0.0;Comments=</VerInfo_Keys>
    </PropertyGroup>
    <PropertyGroup Condition="'$(Cfg_1_Win32)'!=''">
        <DCC_DebugDCUs>true</DCC_DebugDCUs>
        <DCC_Define>DEBUG;$(DCC_Define)</DCC_Define>
        <DCC_GenerateStackFrames>true</DCC_GenerateStackFrames>
        <DCC_Optimize>false</DCC_Optimize>
        <DCC_RemoteDebug>true</DCC_RemoteDebug>
    </PropertyGroup>
    <ItemGroup>
        <DelphiCompile Include="$(MainSource)">
            <MainSource>MainSource</MainSource>
        </DelphiCompile>
        <DCCReference Include="Views\\TestAIHarness.View.Main.pas">
            <Form>FormTestAIHarness</Form>
        </DCCReference>
        <BuildConfiguration Include="Release">
            <Key>Cfg_2</Key>
            <CfgParent>Base</CfgParent>
        </BuildConfiguration>
        <BuildConfiguration Include="Base">
            <Key>Base</Key>
        </BuildConfiguration>
        <BuildConfiguration Include="Debug">
            <Key>Cfg_1</Key>
            <CfgParent>Base</CfgParent>
        </BuildConfiguration>
    </ItemGroup>
    <ProjectExtensions>
        <Borland.Personality>Delphi.Personality.12</Borland.Personality>
        <Borland.ProjectType>Application</Borland.ProjectType>
        <BorlandProject>
            <Delphi.Personality/>
        </BorlandProject>
        <ProjectFileVersion>12</ProjectFileVersion>
    </ProjectExtensions>
    <Import Project="$(BDS)\\Bin\\CodeGear.Delphi.Targets" Condition="Exists('$(BDS)\\Bin\\CodeGear.Delphi.Targets')"/>
    <Import Project="$(APPDATA)\\Embarcadero\\$(BDSAPPDATABASEDIR)\\$(PRODUCTVERSION)\\UserTools.proj" Condition="Exists('$(APPDATA)\\Embarcadero\\$(BDSAPPDATABASEDIR)\\$(PRODUCTVERSION)\\UserTools.proj')"/>
</Project>
"""

def create_file(path, content):
    """Creates a file with the given content, ensuring the directory exists."""
    os.makedirs(os.path.dirname(path), exist_ok=True)
    if not os.path.exists(path):
        with open(path, 'w', encoding='utf-8') as f:
            # Use textwrap.dedent to remove leading whitespace from multiline strings
            f.write(textwrap.dedent(content).strip())
        print(f"  - Created: {os.path.basename(path)}")
    else:
        print(f"  - Already exists, skipped: {os.path.basename(path)}")

def create_test_harness(base_path):
    """Creates all necessary files for the VCL test harness project."""
    print("-> Creating Test Harness project...")
    harness_dir = os.path.join(base_path, 'TestHarness')
    views_dir = os.path.join(harness_dir, 'Views')

    # Create directories
    os.makedirs(views_dir, exist_ok=True)

    # Create project files
    create_file(os.path.join(harness_dir, 'TestAIHarness.dpr'), DPR_CONTENT)
    create_file(os.path.join(harness_dir, 'TestAIHarness.dproj'), DPROJ_CONTENT)
    create_file(os.path.join(views_dir, 'TestAIHarness.View.Main.pas'), PAS_CONTENT)
    create_file(os.path.join(views_dir, 'TestAIHarness.View.Main.dfm'), DFM_CONTENT)

    print("  - Test Harness creation complete.")

# --- Main Execution ---
if __name__ == "__main__":
    project_base_path = "."
    print("Starting Delphi test harness creation script...")
    print("-" * 40)
    create_test_harness(project_base_path)
    print("-" * 40)
    print("Script finished.")
    print("\nNext Steps:")
    print("1. Open 'TestHarness\\TestAIHarness.dproj' in Delphi.")
    print("2. The project's search path is already configured in the .dproj file.")
    print("3. Press F9 to compile and run the test application.")
    print("4. Enter an API key for the provider you want to test and send a request.")

    
