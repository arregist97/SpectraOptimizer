program MasCalOptimizerApp;

uses
  Vcl.Forms,
  OptimizerTestForm in 'OptimizerTestForm.pas' {Form3},
  SpecGrinder in 'SpecGrinder.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm3, Form3);
  Application.Run;
end.
