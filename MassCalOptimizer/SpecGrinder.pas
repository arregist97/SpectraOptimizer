unit SpecGrinder;

interface

uses
CsvSpectrum,
FeatureCalculator,
FeatureTable,
MassSpectrum,
SearchGrid,
System.Classes,
System.SysUtils;

const
  c_SlopeRange: Array[0..1] of Double = (0.00001, 0.0000001);//min/max distance the modified slopes can be from the base slope
  c_OffsetRange: Array[0..1] of Double = (0.001, 0.000001);//min/max distance the modified offsets can be from the base offset
  c_Mults: Array[0..1] of Integer = (1, -1);//we simulate 4 quardants by mirroring the positive indices
  c_NumSlopes = 5;//half the number of slopes that will be experimented with(due to the mirror)
  c_NumOffsets = 20;//half the number of offsets that will be experimented with(due to the mirror)
  c_ModelPath: AnsiString = 'C:/Users/arreg/Documents/optimizer_models/model_';
  c_ResultPath: AnsiString = 'C:/Users/arreg/Documents/optimizer_data/DllModelOutput';
  c_FeatPath: AnsiString = 'C:/Users/arreg/Documents/optimizer_data/DllModelInput';
  c_FeatLoc = 'C:\Users\arreg\Documents\optimizer_data\DllModelInput';

type

TSpecGrinder = class
  private
    FBestScore: Double;
    FBestSlope: Double;
    FBestOffset: Double;
    procedure WriteFeatures(FeatTable: TFeatureTable; FeatLoc: String);
    function CalcScore(FeatTable: TFeatureTable): Double;
    procedure RecOptimizeSpectrum(Csv: TCsvSpectrum; FeatureCalc: TFeatureCalculator; SlopeRange, OffsetRange: Array of Double; NumSlopes, NumOffsets: Integer; Prev: Double);
  public
    property Score: Double read FBestScore;
    constructor Create(FragLoc, RangeLoc, IsoLoc, CsvLoc: String);
    destructor Destroy; override;
end;

implementation

function fnGradientBoostedPredictor(mPtr, dPtr, rPtr: PAnsiChar): Double; cdecl; external
'GradientBoostedPredictor.dll' name 'fnGradientBoostedPredictor'; //Using Double

procedure TSpecGrinder.WriteFeatures(FeatTable: TFeatureTable; FeatLoc: String);
var
  featFile, i, check: Integer;
  buffer: AnsiString;

begin
  featFile := FileCreate(FeatLoc);
  if FeatFile = INVALID_HANDLE_VALUE then
  begin
    featFile := FileOpen(FeatLoc, fmOpenWrite);
    if FeatFile = INVALID_HANDLE_VALUE then
    begin
      raise Exception.Create('Could not open file' + FeatLoc);
    end;
  end;

  buffer := '0';

  for i := 0 to featTable.NumFragSplits - 1 do
  begin
    buffer := buffer + #9 + FloatToStr(featTable.Matches[i]) + #9 +
    FloatToStr(featTable.PropMatches[i]) + #9 +
    FloatToStr(featTable.AvgLowerDiffs[i]) + #9 +
    FloatToStr(featTable.AvgHigherDiffs[i]);
  end;

  for i := 0 to featTable.NumNpzSplits - 1 do
  begin
    buffer := buffer + #9 + FloatToStr(featTable.NpzMatches[i]) + #9 +
    FloatToStr(featTable.PropNpzMatches[i]) + #9 +
    FloatToStr(featTable.AvgNpzDiffs[i]);
  end;


  buffer := buffer + #9 + FloatToStr(featTable.TwoElems) + #9 +
  FloatToStr(featTable.AvgTwoDist) + #9 +
  FloatToStr(featTable.AvgTwoAbundSep) + #9 +
  FloatToStr(featTable.ThreeElems) + #9 +
  FloatToStr(featTable.AvgThreeDist) + #9 +
  FloatToStr(featTable.AvgThreeAbundSep) + #13#10;

  check := FileWrite(featFile, buffer[1], Length(buffer));

  if check = -1 then
  begin
    raise Exception.Create('Could not transfer feature data.');
  end;

  FileClose(featFile);

end;

function TSpecGrinder.CalcScore(FeatTable: TFeatureTable): Double;
var
  cumulative: Double;
  featPtr, modelPtr, resultPtr: PAnsiChar;
  i: Integer;
  tempAnsiStr: AnsiString;

begin
  WriteFeatures(FeatTable, c_FeatLoc);
  featPtr := Addr(c_FeatPath[1]);
  resultPtr := Addr(c_ResultPath[1]);
  cumulative := 0;

  for i := 0 to 9 do
  begin
    tempAnsiStr := c_ModelPath + IntToStr(i) + '.txt';//change models
    modelPtr := Addr(tempAnsiStr[1]);
    cumulative := cumulative + fnGradientBoostedPredictor(modelPtr, featPtr, resultPtr);
  end;

  Result := cumulative / 10;
end;

procedure TSpecGrinder.RecOptimizeSpectrum(Csv: TCsvSpectrum; FeatureCalc: TFeatureCalculator; SlopeRange, OffsetRange: Array of Double; NumSlopes, NumOffsets: Integer; Prev: Double);
var
  grid: TSearchGrid;
  massSpec: TMassSpectrum;
  featureTable: TFeatureTable;
  baseSlope, baseOffset: Double;
  score: Double;
  slopeMod, offsetMod: Double;
  slopeMult, offsetMult: Integer;
  i, j: Integer;
  improved: Boolean;
  slope, offset: Double;
  slopeIndex, OffsetIndex: Integer;
  slopeEdge, offsetEdge: Double;
  newSlopeRange, newOffsetRange: Array[0..1] of Double;
  newNumSlopes, newNumOffsets: Integer;


begin
  grid := TSearchGrid.Create(SlopeRange, OffsetRange, NumSlopes, NumOffsets);
  baseSlope := csv.MassOverTime;
  baseOffset := csv.MassOffset;
  if Prev = 0 then
  begin
    massSpec := TMassSpectrum.Create(Csv, baseSlope, baseOffset);
    featureTable := FeatureCalc.Feature[MassSpec];
    score := CalcScore(featureTable);
    FBestScore := score;
    Prev := score;
    massSpec.Free;
  end
  else
  begin
    FBestScore := 0;
  end;

  for i := 0 to NumSlopes - 1 do
  begin
    slopeMod := grid.SlopeAxis[i];
    for slopeMult in c_Mults do
    begin
      slopeMod := slopeMod * slopeMult;
      for j := 0 to NumOffsets - 1 do
      begin
        offsetMod := grid.OffsetAxis[j];
        for offsetMult in  c_Mults do
        begin
          improved := False;
          offsetMod := offsetMod * offsetMult;

          slope := baseSlope + slopeMod * baseSlope;
          offset := baseOffset + offsetMod * baseOffset;
          massSpec := TMassSpectrum.Create(Csv, slope, offset);
          featureTable := FeatureCalc.Feature[MassSpec];
          score := CalcScore(featureTable);

          if score > FBestScore then//>=?
            improved := True;

          if improved then
          begin
            FBestScore := score;
            FBestOffset := offsetMod;
            FBestSlope := slopeMod;

            offsetIndex := grid.OffsetIndex[FBestOffset * offsetMult];
            slopeIndex := grid.SlopeIndex[FBestSlope * slopeMult];

            offsetEdge := 2 * ABS(0.5 - (offsetIndex + 0.1) / NumOffsets);//outdated calculations
            slopeEdge := 2 * ABS(0.5 - (slopeIndex + 0.1) / NumSlopes);
          end;

          massSpec.Free;
        end;

      end;

    end;

  end;

  grid.Free;

  if FBestScore > Prev then
  begin
    newOffsetRange[0] := FBestOffset - (0.5 / offsetEdge) * FBestOffset;//outdated calculations
    newOffsetRange[1] := FBestOffset + (0.5 / offsetEdge) * FBestOffset;
    newSlopeRange[0] := FBestSlope + (0.5 / slopeEdge) * FBestSlope;
    newSlopeRange[1] := FBestSlope - (0.5 / slopeEdge) * FBestSlope;
    newNumOffsets := 20;
    newNumSlopes := 5;
    Prev := FBestScore;



    RecOptimizeSpectrum(Csv, FeatureCalc, newSlopeRange, newOffsetRange, newNumOffsets, newNumSlopes, Prev);
  end;


end;

constructor TSpecGrinder.Create(FragLoc, RangeLoc, IsoLoc, CsvLoc: String);
var
  csv: TCsvSpectrum;
  featureCalc: TFeatureCalculator;
  score: Double;

begin
  csv := TCsvSpectrum.Create(CsvLoc);
  featureCalc := TFeatureCalculator.Create(FragLoc, RangeLoc, IsoLoc);
  recOptimizeSpectrum(csv, featureCalc, c_SlopeRange, c_OffsetRange, c_NumSlopes, c_NumOffsets, 0.0);
  featureCalc.Free;
  csv.Free;
end;

destructor TSpecGrinder.Destroy;
begin
  inherited;
end;

end.
