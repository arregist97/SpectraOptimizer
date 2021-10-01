unit SpecGrinder;

interface

uses
CsvSpectrum,
FeatureCalculator,
FeatureTable,
FileRoutines,
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
  c_ResultPath: AnsiString = 'C:/Users/arreg/Documents/optimizer_data/DllModelOutput_';
  c_FeatPath: AnsiString = 'C:/Users/arreg/Documents/optimizer_data/DllModelInput';
  c_FeatLoc = 'C:\Users\arreg\Documents\optimizer_data\DllModelInput';
  c_ResultsLoc = 'C:\Users\arreg\Documents\optimizer_data\DllModelOutput_';
  c_DataLoc = '\Users\arreg\Documents\optimizer_data\PlotData.csv';

type

TSpecGrinder = class
  private
    FPrevSlope: Double;
    FPrevOffset: Double;
    FBestScore: Double;
    FBestSlope: Double;
    FBestOffset: Double;
    FOffsetEdge: Double;
    FSlopeEdge: Double;
    procedure WriteFeatures(FeatTable: TFeatureTable; FeatWriter: Integer);
    procedure CalcScore;
    procedure FindBestScore(SearchGrid: TSearchGrid; BaseSlope, BaseOffset: Double; NumSlopes, NumOffsets: Integer; var DataFile: TextFile);
    procedure RecOptimizeSpectrum(Csv: TCsvSpectrum; FeatureCalc: TFeatureCalculator; SlopeRange, OffsetRange: Array of Double; NumSlopes, NumOffsets: Integer; Prev: Double);
  public
    property Score: Double read FBestScore;
    constructor Create(FragLoc, RangeLoc, IsoLoc, CsvLoc: String);
    destructor Destroy; override;
end;

implementation

function fnGradientBoostedPredictor(mPtr, dPtr, rPtr: PAnsiChar): Double; cdecl; external
'GradientBoostedPredictor.dll' name 'fnGradientBoostedPredictor'; //Using Double

procedure TSpecGrinder.WriteFeatures(FeatTable: TFeatureTable; FeatWriter: Integer);
var
  i, check: Integer;
  buffer: AnsiString;

begin

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

  check := FileWrite(FeatWriter, buffer[1], Length(buffer));

  if check = -1 then
  begin
    raise Exception.Create('Could not transfer feature data.');
  end;


end;

procedure TSpecGrinder.CalcScore();
var
  featPtr, modelPtr, resultPtr: PAnsiChar;
  i: Integer;
  tempModelStr, tempResultStr: AnsiString;

begin
  featPtr := Addr(c_FeatPath[1]);

  for i := 0 to 9 do
  begin
    tempModelStr := c_ModelPath + IntToStr(i) + '.txt';//change models
    tempResultStr := c_ResultPath + IntToStr(i) + '.txt';//change result files

    modelPtr := Addr(tempModelStr[1]);
    resultPtr := Addr(tempResultStr[1]);
    fnGradientBoostedPredictor(modelPtr, featPtr, resultPtr);
  end;

end;

procedure TSpecGrinder.FindBestScore(SearchGrid: TSearchGrid; BaseSlope, BaseOffset: Double; NumSlopes, NumOffsets: Integer; var DataFile: TextFile);
var
  results: Array[0..9] of Integer;
  fileIndex: Integer;
  loc: String;
  cumulative, score: Double;
  lineStr: WideString;
  slopeMod, offsetMod: Double;
  slopeMult, offsetMult: Integer;
  i, j: Integer;
  slope, offset: Double;
  slopeIndex, OffsetIndex: Integer;
  buffer: String;

begin
  //open result files
  for i := 0 to 9 do
  begin
    loc := c_ResultsLoc + IntToStr(i) + '.txt';
    results[i] := FileOpen(loc, fmOpenRead);
  end;

  FBestScore := 0;

  for i := 0 to NumSlopes - 1 do
  begin
    slopeMod := SearchGrid.SlopeAxis[i];
    for slopeMult in c_Mults do
    begin
      slopeMod := slopeMod * slopeMult;
      for j := 0 to NumOffsets - 1 do
      begin
        offsetMod := SearchGrid.OffsetAxis[j];
        for offsetMult in  c_Mults do
        begin
          offsetMod := offsetMod * offsetMult;

          slope := baseSlope + slopeMod * baseSlope;
          offset := baseOffset + offsetMod * baseOffset;
          //add model scores from each result line
          cumulative := 0;
          for fileIndex := 0 to 9 do
          begin
            lineStr := ReadHeaderLine(results[fileIndex]);
            if lineStr <> '' then
            begin
              cumulative := cumulative + StrToFloat(lineStr);
            end
            else
            begin
              raise Exception.Create('Unexpected end of file: ' + loc);
            end;
          end;

          score := cumulative / 10;

          buffer := FloatToStr(slope) + ',' + FloatToStr(offset) + ',' + FloatToStr(score);
          WriteLn(DataFile, buffer);

          if score > FBestScore then
          begin
            FBestScore := score;
            FBestOffset := offsetMod;
            FBestSlope := slopeMod;

            offsetIndex := SearchGrid.OffsetIndex[FBestOffset * offsetMult];
            slopeIndex := SearchGrid.SlopeIndex[FBestSlope * slopeMult];

            FOffsetEdge := 2 * ABS(0.5 - (offsetIndex + 0.1) / NumOffsets);//outdated calculations
            FSlopeEdge := 2 * ABS(0.5 - (slopeIndex + 0.1) / NumSlopes);
          end;

        end;

      end;

    end;

  end;

  //close result files
  for i := 0 to 9 do
  begin
    FileClose(results[i]);
  end;

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
  slope, offset: Double;
  slopeIndex, OffsetIndex: Integer;
  newSlopeRange, newOffsetRange: Array[0..1] of Double;
  newNumSlopes, newNumOffsets: Integer;
  featFile: Integer;
  dataFile: TextFile;


begin
  grid := TSearchGrid.Create(SlopeRange, OffsetRange, NumSlopes, NumOffsets);
  baseSlope := csv.MassOverTime;
  baseOffset := csv.MassOffset;
  featFile := FileCreate(c_FeatLoc);
  if FeatFile = INVALID_HANDLE_VALUE then
  begin
    featFile := FileOpen(c_FeatLoc, fmOpenWrite);
    if FeatFile = INVALID_HANDLE_VALUE then
    begin
      raise Exception.Create('Could not open file' + c_FeatLoc);
    end;
  end;

  AssignFile(dataFile, c_DataLoc);
  if Prev = 0 then
  begin
    ReWrite(dataFile);
    WriteLn(dataFile, 'MassOverTime,MassOffset,Score');
  end
  else
  begin
    Append(dataFile);
  end;


  (*
  if Prev = 0 then
  begin
    massSpec := TMassSpectrum.Create(Csv, baseSlope, baseOffset);
    featureTable := FeatureCalc.Feature[MassSpec];
    WriteFeatures(featureTable, featFile);
    score := CalcScore();
    FBestScore := score;
    Prev := score;
    massSpec.Free;
  end
  else
  begin
    FBestScore := 0;
  end;
  *)
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
          offsetMod := offsetMod * offsetMult;

          slope := baseSlope + slopeMod * baseSlope;
          offset := baseOffset + offsetMod * baseOffset;
          massSpec := TMassSpectrum.Create(Csv, slope, offset);
          featureTable := FeatureCalc.Feature[MassSpec];
          WriteFeatures(featureTable, featFile);
          massSpec.Free;
        end;

      end;

    end;

  end;

  FileClose(featFile);

  CalcScore();
  FindBestScore(grid, baseSlope, baseOffset, NumSlopes, NumOffsets, dataFile);
  CloseFile(dataFile);

  grid.Free;

  if FBestScore > Prev then
  begin
    newOffsetRange[0] := FBestOffset - (0.5 / FOffsetEdge) * FBestOffset;//outdated calculations
    newOffsetRange[1] := FBestOffset + (0.5 / FOffsetEdge) * FBestOffset;
    newSlopeRange[0] := FBestSlope + (0.5 / FSlopeEdge) * FBestSlope;
    newSlopeRange[1] := FBestSlope - (0.5 / FSlopeEdge) * FBestSlope;
    newNumOffsets := 20;
    newNumSlopes := 5;
    Prev := FBestScore;
    FPrevSlope := FBestSlope;
    FPrevOffset := FBestOffset;

    RecOptimizeSpectrum(Csv, FeatureCalc, newSlopeRange, newOffsetRange, newNumOffsets, newNumSlopes, Prev);
  end
  else
  begin
    FBestScore := Prev;
    FBestSlope := FPrevSlope;
    FBestOffset := FPrevOffset;
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
