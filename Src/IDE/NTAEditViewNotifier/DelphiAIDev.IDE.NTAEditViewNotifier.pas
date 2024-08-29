unit DelphiAIDev.IDE.NTAEditViewNotifier;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  System.Types,
  System.SyncObjs,
  System.RegularExpressions,
  Vcl.Dialogs,
  Vcl.Graphics,
  Vcl.Imaging.pngimage,
  DelphiAIDev.Utils,
  DelphiAIDev.Utils.OTA,
  DelphiAIDev.Consts,
  DelphiAIDev.CodeCompletion.Vars,
  ToolsAPI;

type
  TDelphiAIDevIDENTAEditViewNotifier = class(TInterfacedObject, IOTANotifier, INTAEditViewNotifier)
  private
    LVars: TDelphiAIDevCodeCompletionVars;
  protected
    FIOTAEditView: IOTAEditView;
    FIndex: Integer;
    procedure RemoveNotifier;
  public
    constructor Create(FileName: string; AEditView: IOTAEditView);
    destructor Destroy; override;

    { INTAEditViewNotifier }
    ///<summary>EditorIdle � chamado ap�s alguma a��o ter ocorrido na visualiza��o
    ///(edi��o, movimento do cursor, etc.) e um per�odo de tempo ter passado sem que outra
    ///a��o acontecesse. Isso � aproximadamente equivalente ao momento em que o Code
    ///Insight � acionado (e est� vinculado � configura��o de atraso do Code Insight)</summary>
    procedure EditorIdle(const View: IOTAEditView);
    procedure BeginPaint(const View: IOTAEditView; var FullRepaint: Boolean);
    procedure PaintLine(const View: IOTAEditView; LineNumber: Integer;
      const LineText: PAnsiChar; const TextWidth: Word; const LineAttributes: TOTAAttributeArray;
      const Canvas: TCanvas; const TextRect: TRect; const LineRect: TRect; const CellSize: TSize);
    ///<summary>EndPaint � chamado depois que todas as linhas foram repintadas.
    ///Use isso para limpar quaisquer estruturas de dados que foram mantidas
    ///ao longo da pintura das linhas</summary>
    procedure EndPaint(const View: IOTAEditView);

    //IOTANotifier
    procedure AfterSave;
    procedure BeforeSave;
    procedure Modified;
    procedure Destroyed;
  end;

implementation

constructor TDelphiAIDevIDENTAEditViewNotifier.Create(FileName: string; AEditView: IOTAEditView);
begin
  inherited Create;
  FIOTAEditView := AEditView;
  FIndex := FIOTAEditView.AddNotifier(Self);
  LVars := TDelphiAIDevCodeCompletionVars.GetInstance;
end;

destructor TDelphiAIDevIDENTAEditViewNotifier.Destroy;
begin
  Self.RemoveNotifier;
  inherited;
end;

procedure TDelphiAIDevIDENTAEditViewNotifier.Destroyed;
begin
  Self.RemoveNotifier;
end;

procedure TDelphiAIDevIDENTAEditViewNotifier.AfterSave;
begin

end;

procedure TDelphiAIDevIDENTAEditViewNotifier.BeforeSave;
begin

end;

procedure TDelphiAIDevIDENTAEditViewNotifier.BeginPaint(const View: IOTAEditView; var FullRepaint: Boolean);
begin
  FullRepaint := True;
end;

procedure TDelphiAIDevIDENTAEditViewNotifier.EditorIdle(const View: IOTAEditView);
var
  LRow: Integer;
  LColumn: Integer;
begin
  TUtilsOTA.GetCursorPosition(LRow, LColumn);

  //TUtils.AddLog(Format('EditorIdle - %d (%d) - %d (%d) - %s ', [LVars.Row, LRow, LVars.Column, LColumn, DateTimeToStr(Now)]));

  if (LRow <> LVars.Row) or (LColumn <> LVars.Column) then
  begin
    LVars.LineIni := 0;
    LVars.Clear;
  end;

  //TUtils.AddLog('EditorIdle ' + BoolToStr(LVars.ClearNext, True));

//  if LVars.ClearNext then
//  begin
//    LVars.Clear;
//  end
//  else
//    LVars.ClearNext := True;

  //View.AddNotifier()
  //View.GetEditWindow.Form.Repaint;
end;

procedure TDelphiAIDevIDENTAEditViewNotifier.EndPaint(const View: IOTAEditView);
begin
  //TUtils.AddLog('EndPaint');
end;

procedure TDelphiAIDevIDENTAEditViewNotifier.Modified;
begin

end;

procedure TDelphiAIDevIDENTAEditViewNotifier.PaintLine(const View: IOTAEditView; LineNumber: Integer;
  const LineText: PAnsiChar; const TextWidth: Word; const LineAttributes: TOTAAttributeArray;
  const Canvas: TCanvas; const TextRect: TRect; const LineRect: TRect; const CellSize: TSize);
var
  LLineText: string;
begin
  if LineNumber < 1 then
    Exit;

  LLineText := string(LineText);

  if not LLineText.Trim.IsEmpty then
    Exit;

  //if LineNumber <> View.CursorPos.Line then
  //  Exit;

  if (LineNumber >= LVars.LineIni)and(LineNumber <= LVars.LineEnd) then
  begin
    Canvas.Brush.Style := bsClear;
    Canvas.Font.Color := $777777; //$666666;

    try
      LLineText := LVars.Contents[LineNumber - LVars.LineIni];
      Canvas.TextOut(TextRect.Left, TextRect.Top, LLineText.TrimRight);
    except on E: Exception do
      TUtils.AddLog(E.Message);
    end;
  end;
end;

procedure TDelphiAIDevIDENTAEditViewNotifier.RemoveNotifier;
begin
  if Assigned(FIOTAEditView) and (FIndex >= 0) then
  begin
    FIOTAEditView.RemoveNotifier(FIndex);
    FIndex := -1;
    FIOTAEditView := nil;
  end;
end;

end.
