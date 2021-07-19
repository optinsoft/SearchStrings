program SearchStrings;

uses
  Vcl.Forms,
  SearchStringsMainFormUnit in 'SearchStringsMainFormUnit.pas' {SearchStringsMainForm},
  uAppFolder in 'uAppFolder.pas',
  BufferedFileStreamUnit in '..\Source\BufferedFileStreamUnit.pas',
  StreamReaderExUnit in '..\Source\StreamReaderExUnit.pas',
  uIntList in '..\Source\uIntList.pas',
  uLineReaderWriter in '..\Source\uLineReaderWriter.pas',
  uSearchClass in '..\Source\uSearchClass.pas',
  uSearchStrings in '..\Source\uSearchStrings.pas',
  uStrUtils in '..\Source\uStrUtils.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'Fast Search Strings';
  Application.CreateForm(TSearchStringsMainForm, SearchStringsMainForm);
  Application.Run;
end.
