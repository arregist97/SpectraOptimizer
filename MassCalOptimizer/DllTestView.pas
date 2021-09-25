unit DllTestView;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm3 = class(TForm)
    Button1: TButton;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form3: TForm3;

implementation

{$R *.dfm}


function fnVS2019TestDllProject: Double; cdecl; external
'GradientBoostedPredictor.dll' name 'fnGradientBoostedPredictor'; //Using Double

procedure TForm3.Button1Click(Sender: TObject);
var
  c: Double;
  modelLoc, dataLoc, resultLoc: String;
  mPtr, dPtr, rPtr: PChar;
begin
  modelLoc := 'C:/Users/arreg/CodeProjects/CppProjects/LightGBM-master/examples/Warrens_model0/model_0.txt';
  dataLoc := 'C:/Users/arreg/CodeProjects/CppProjects/LightGBM-master/examples/Warrens_model0/0PhiTestPredictionDataSet01.txt';
  resultLoc := 'C:/Users/arreg/CodeProjects/CppProjects/LightGBM-master/examples/Warrens_model0/TestMethod3_result_0.txt';
  mPtr := modelLoc[1];
  dPtr := dataLoc[1];
  rPtr := resultLoc[1];
  c := 0;
  c := fnGradientBoostedPredictor(mPtr, dPtr, rPtr);
  Button1.Caption := FloatToStr(c);
end;

end.
