program Project1;

uses
  Vcl.Forms,
  Unit1 in 'Unit1.pas' {Form1},
  uSearchStrings in '..\Source\uSearchStrings.pas',
  uIntList in '..\Source\uIntList.pas',
  uStrUtils in '..\Source\uStrUtils.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
