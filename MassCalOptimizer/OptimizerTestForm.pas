unit OptimizerTestForm;

interface

uses
  CsvSpectrum, MassSpectrum, FeatureCalculator, FeatureTable, Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

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

begin
  fLoc := 'C:\Users\arreg\Documents\optimizer_data\Fragment Table.csv';
  rLoc := 'C:\Users\arreg\Documents\optimizer_data\saved_ranges.csv';
  iLoc := 'C:\Users\arreg\Documents\optimizer_data\Elements.txt';
  csvLoc := 'C:\Users\arreg\Documents\optimizer_data\Lactose Border HR2-008-Neg_XYMassShiftCorrected_pb2.csv';

  csv := TCsvSpectrum.Create(csvLoc);

  massSpectrum := TMassSpectrum.Create(csv, csv.MassOverTime, csv.MassOffset);


  featureCalc := TFeatureCalculator.Create(fLoc, rLoc, iLoc);
  featureTable := featureCalc.Feature[massSpectrum];
  i := 0;
  for j := 0 to featureTable.NumFragSplits - 1 do
  begin
    memo1.Lines[i] := IntToStr(i) + ' ' + FloatToStr(featureTable.Matches[j]) + ', ' +
    FloatToStr(featureTable.PropMatches[j]) + ', ' +
    FloatToStr(featureTable.AvgLowerDiffs[j]) + ', ' +
    FloatToStr(featureTable.AvgHigherDiffs[j]);
    Inc(i);
  end;

  for j := 0 to featureTable.NumNpzSplits - 1 do
  begin
    memo1.Lines[i] := IntToStr(i) + ' ' + FloatToStr(featureTable.NpzMatches[j]) + ', ' +
    FloatToStr(featureTable.PropNpzMatches[j]) + ', ' +
    FloatToStr(featureTable.AvgNpzDiffs[j]);
    Inc(i);
  end;


  memo1.Lines[i] := IntToStr(i) + ' ' + FloatToStr(featureTable.TwoElems) + ', ' +
  FloatToStr(featureTable.AvgTwoDist) + ', ' +
  FloatToStr(featureTable.AvgTwoAbundSep) + ', ' +
  FloatToStr(featureTable.ThreeElems) + ', ' +
  FloatToStr(featureTable.AvgThreeDist) + ', ' +
  FloatToStr(featureTable.AvgThreeAbundSep);
end;

end.
