program TestAIHarness;

uses
  Vcl.Forms,
  TestAIHarness.View.Main
    in 'Views\TestAIHarness.View.Main.pas' {FormTestAIHarness};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormTestAIHarness, FormTestAIHarness);
  Application.Run;

end.
