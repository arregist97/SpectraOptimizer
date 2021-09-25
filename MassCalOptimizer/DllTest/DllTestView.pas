unit DllTestView;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm3 = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form3: TForm3;

implementation

{$R *.dfm}

function fnGradientBoostedPredictor: Double; cdecl; external
'GradientBoostedPredictor.dll' name 'fnGradientBoostedPredictor'; //Using Double

procedure TForm3.Button1Click(Sender: TObject);
var
  c: Double;
  n: Integer;
  modelLoc, dataLoc, resultLoc: AnsiString;
  mPtr, dPtr, rPtr: PAnsiChar;
begin
  modelLoc := 'C:/Users/arreg/CodeProjects/CppProjects/LightGBM-master/examples/binary_classification/LightGBM_model.txt';
  dataLoc := 'C:/Users/arreg/CodeProjects/CppProjects/LightGBM-master/examples/binary_classification/RR(2Row)binary.test';
  resultLoc := 'C:/Users/arreg/CodeProjects/CppProjects/LightGBM-master/examples/binary_classification/RR01_LightGBM_predict_result.txt';
  mPtr := Addr(modelLoc[1]);
  dPtr := Addr(dataLoc[1]);
  rPtr := Addr(resultLoc[1]);
  c := 0;
  c := fnGradientBoostedPredictor();
  n := nGradientBoostedPredictor;
  Button1.Caption := FloatToStr(c);
end;

procedure TForm3.FormCreate(Sender: TObject);
var
  d : Integer;
begin
  d := 0;
end;

end.
