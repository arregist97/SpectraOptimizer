unit OptimizerTestForm;

interface

uses
  CsvSpectrum,
  MassSpectrum,
  FeatureCalculator,
  FeatureTable,
  SpecGrinder,
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls;

type
  TForm3 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form3: TForm3;

implementation

{$R *.dfm}

procedure TForm3.Button1Click(Sender: TObject);
var
  iLoc, rLoc, fLoc, csvLoc: String;
  featureCalc: TFeatureCalculator;
  featureTable: TFeatureTable;
  csv: TCsvSpectrum;
  massSpectrum: TMassSpectrum;
  i, j: Integer;
  grinder: TSpecGrinder;

begin
  fLoc := 'C:\Users\arreg\Documents\optimizer_data\Fragment Table.csv';
  rLoc := 'C:\Users\arreg\Documents\optimizer_data\saved_ranges.csv';
  iLoc := 'C:\Users\arreg\Documents\optimizer_data\Elements.txt';
  csvLoc := 'C:\Users\arreg\Documents\optimizer_data\Lactose Particles HR2-004-Neg_XYMassShiftCorrected_pb2.csv';

  grinder := TSpecGrinder.Create(fLoc, rLoc, iLoc, csvLoc);

  memo1.Lines[0] := 'Best Score: ' + FloatToStr(grinder.Score);

end;

end.
