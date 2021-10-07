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
  c_SlopeRange: Array[0..1] of Double = (0.00009, 0.0);//min/max distance the modified slopes can be from the base slope
  c_OffsetRange: Array[0..1] of Double = (0.009, 0.0);//min/max distance the modified offsets can be from the base offset
  c_Mults: Array[0..1] of Integer = (1, -1);//we simulate 4 quardants by mirroring the positive indices
  c_NumSlopes = 20;//half the number of slopes that will be experimented with(due to the mirror)
  c_NumOffsets = 20;//half the number of offsets that will be experimented with(due to the mirror)
  c_ModelPath: AnsiString = 'C:/Users/arreg/Documents/optimizer_models/model#';
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
    procedure RecOptimizeSpectrum(BaseSlope, BaseOffset: Double; Csv: TCsvSpectrum; FeatureCalc: TFeatureCalculator; SlopeRange, OffsetRange: Array of Double; NumSlopes, NumOffsets: Integer; Prev: Double);
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
(*
 * Rather than store all the slopes and offsets, we recalculate them here,
 * and write them to a file along with their corresponding scores.
 *
 * Addtionally, we have to include the duplicate protection code here as well,
 * so that the lines we look for sync with the lines we sent to the dll.
 *
 *)
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
  buffer: String;
  duplicate: Boolean;//To avoid mirroring the center axes, we will manually ignore duplicate values.

begin
  //open result files
  for i := 0 to 9 do
  begin
    loc := c_ResultsLoc + IntToStr(i) + '.txt';
    results[i] := FileOpen(loc, fmOpenRead);
  end;

  FBestScore := 0;
  duplicate := False;
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

          //duplicate check
          if (slopeMod = 0) and (slopeMult = -1) then//checks for a duplicate if the slope is zero(mirroring the center is pointless)
          begin
            duplicate := True;
          end;
          if (offsetMod = 0) and (offsetMult = -1) then//checks for a duplicate if the offset is zero
          begin
            duplicate := True;
          end;

          if not duplicate then
          begin
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
              FBestOffset := offset;
              FBestSlope := slope;

            end;

          end;
          duplicate := False;//Every slope/offset needs to be checked for duplicates

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

procedure TSpecGrinder.RecOptimizeSpectrum(BaseSlope, BaseOffset: Double; Csv: TCsvSpectrum; FeatureCalc: TFeatureCalculator; SlopeRange, OffsetRange: Array of Double; NumSlopes, NumOffsets: Integer; Prev: Double);
var
  grid: TSearchGrid;
  massSpec: TMassSpectrum;
  featureTable: TFeatureTable;
  slopeMod, offsetMod: Double;
  slopeMult, offsetMult: Integer;
  i, j: Integer;
  slope, offset: Double;
  slopeIndex, OffsetIndex: Integer;
  newSlope, newOffset: Double;
  newSlopeRange, newOffsetRange: Array[0..1] of Double;
  newNumSlopes, newNumOffsets: Integer;
  featFile: Integer;
  dataFile: TextFile;
  duplicate: Boolean;//To avoid mirroring the center axes, we will manually ignore duplicate values.


begin
  grid := TSearchGrid.Create(SlopeRange, OffsetRange, NumSlopes, NumOffsets);
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
  //In order to minimize the dll access, we calculate our features in batches.
  duplicate := False;
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

          //duplicate check
          if (slopeMod = 0) and (slopeMult = -1) then//checks for a duplicate if the slope is zero(mirroring the center is pointless)
          begin
            duplicate := True;
          end;
          if (offsetMod = 0) and (offsetMult = -1) then//checks for a duplicate if the offset is zero
          begin
            duplicate := True;
          end;

          if not duplicate then
          begin
            slope := BaseSlope + slopeMod * BaseSlope;
            offset := BaseOffset + offsetMod * BaseOffset;
            massSpec := TMassSpectrum.Create(Csv, slope, offset);
            featureTable := FeatureCalc.Feature[MassSpec];
            WriteFeatures(featureTable, featFile);
            massSpec.Free;
          end;
          duplicate := False;//Every slope/offset needs to be checked for duplicates
        end;

      end;

    end;

  end;

  FileClose(featFile);

  CalcScore();
  FindBestScore(grid, BaseSlope, BaseOffset, NumSlopes, NumOffsets, dataFile);
  CloseFile(dataFile);

  grid.Free;

  if FBestScore > Prev then
  begin
    newOffsetRange[0] := OffsetRange[0] * 0.5;//slightly less outdated calculations
    newOffsetRange[1] := OffsetRange[1] * 0.5;
    newSlopeRange[0] := SlopeRange[0] * 0.5;
    newSlopeRange[1] := SlopeRange[1] * 0.5;
    newSlope := FBestSlope;
    newOffset := FBestOffset;
    newNumOffsets := 20;
    newNumSlopes := 20;
    Prev := FBestScore;
    FPrevSlope := FBestSlope;
    FPrevOffset := FBestOffset;

    RecOptimizeSpectrum(newSlope, newOffset, Csv, FeatureCalc, newSlopeRange, newOffsetRange, newNumOffsets, newNumSlopes, Prev);
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
  baseSlope, baseOffset: Double;
  featureCalc: TFeatureCalculator;
  score: Double;

begin
  csv := TCsvSpectrum.Create(CsvLoc);
  baseSlope := csv.MassOverTime;
  baseOffset := csv.MassOffset;
  featureCalc := TFeatureCalculator.Create(FragLoc, RangeLoc, IsoLoc);
  recOptimizeSpectrum(baseSlope, baseOffset, csv, featureCalc, c_SlopeRange, c_OffsetRange, c_NumSlopes, c_NumOffsets, 0.0);
  featureCalc.Free;
  csv.Free;
end;

destructor TSpecGrinder.Destroy;
begin
  inherited;
end;

end.
