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
    ExplicitWidth = 698
    object GroupBox1: TGroupBox
      Left = 0
      Top = 0
      Width = 700
      Height = 135
      Align = alClient
      Caption = 'Configuration'
      TabOrder = 0
      ExplicitWidth = 698
      object Label1: TLabel
        Left = 16
        Top = 24
        Width = 54
        Height = 13
        Caption = 'AI Provider'
      end
      object Label2: TLabel
        Left = 16
        Top = 72
        Width = 38
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
    ExplicitTop = 507
    ExplicitWidth = 698
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
    ExplicitWidth = 698
    ExplicitHeight = 372
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
      ExplicitWidth = 698
      object mmPrompt: TMemo
        Left = 2
        Top = 15
        Width = 696
        Height = 163
        Align = alClient
        ScrollBars = ssVertical
        TabOrder = 0
        ExplicitWidth = 694
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
      ExplicitWidth = 698
      ExplicitHeight = 184
      object mmResponse: TMemo
        Left = 2
        Top = 15
        Width = 696
        Height = 175
        Align = alClient
        ReadOnly = True
        ScrollBars = ssVertical
        TabOrder = 0
        ExplicitWidth = 694
        ExplicitHeight = 167
      end
    end
  end
end
