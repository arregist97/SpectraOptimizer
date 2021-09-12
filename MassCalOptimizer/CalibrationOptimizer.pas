unit CalibrationOptimizer;

interface

uses
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Generics.Collections,
  CoreUtility,
  Winapi.Windows,
  Winapi.Messages;
type
  TMasses = Array[0..4475] of Double;

  TArrayOfArrayOfList = Array[0..1] of Array [0..2] of TList;

  TRange = Array[0..1] of Double;

  TArrayOfRanges = Array of TRange;

  TArrayOfDouble = Array of Double;

  TSpectrum = class
    public
      FMassOverTime: Double;
      FMassOffset: Double;
      FChannels: TArrayOfDouble;
      FSpecBinSize: Double;
      FStartFlightTime: Double;
      FSize: Integer;
  end;

  TDouble = class
    public
      FValue: Double;
  end;

  TPropLDistHDist = class
    public
      FProp: Double;
      FLDist: Double;
      FHDist: Double;
      FNumMasses: Double;
      FNpzMeanDist: Double;
      FNpzPeakCount: Integer;
      constructor Create();

  end;

  TPropSlopeOffset = class
    public
      FProp: Double;
      FSlope: Double;
      FOffset: Double;

  end;

  TSusPeaks = class
    public
      FPeaks: TList;
      FMean: Double;
  end;

  TCalibrationOptimizer = class
    private
      function GetClosest(I: Integer; var Frags: Array of Double; Mass: Double): Integer;
      function CalcMasses(Channels : Array of Double; SpecBinSize, StartTime, MassOverTime, MassOffset: Double): TArrayOfDouble;
      function GetDistanceNPZ(Masses: Array of Double; Ranges: TArrayofRanges; ShowCorrectValues, Proportions: Boolean; MassThresh: Integer):TList;
      function GetSusPeaks(Masses: Array of Double; Ranges: TArrayofRanges; Thresh: Double): TSusPeaks;
      function IsFindable(Floor, Ceiling: Integer): Boolean;
      function CalcFragMatches(Masses, Frags: Array of Double; Threshes: Array of Double; AB: Boolean): TArrayofArrayofList;
      function GetListMean(List:  TList): Double;
      function CalcNewSpectrumStats(Row: TSpectrum; Frags : Array of Double; SlopeVal, OffsetVal : Double; Ranges : TArrayofRanges; Augment, NumPeaks : Boolean): TPropLDistHDist;
      function ReadHeaderLine(FromFile: Integer): WideString;
      function RemoveQuotes(Str : String): String;
      function GetMasses(Loc: String): TMasses;
      function CreateAxisValues(Start, Finish: Double; Count: Integer): TArrayOfDouble;
      function FindIndex(Axis: Array of Double; Value: Double): Integer;
      function GetBestOffset(Spectrum: TSpectrum; SlopeRange, OffsetRange: TRange; Frags: TMasses; NpzRanges: TArrayofRanges; Offsets, Slopes: Integer; Prev: Double; NpzPeakLimit: Integer; NpzDistLimit: Double): TPropSlopeOffset;
      function ReadCsv(Loc: String): TSpectrum;
      function GetHydrocarbons(): TArrayofDouble;
      function GetIsotopes(Loc : String): TList;
      function CombineMasses(Carbons: TArrayOfDouble; Isotopes: TList): TArrayOfDouble;
      function GetRanges(Masses: TArrayOfDouble; MassLimit: Integer): TArrayOfRanges;
    public
      procedure OptimizeCalibration;
  end;




implementation

uses
   System.Math;

constructor TPropLDistHDist.Create();
begin
  FNumMasses := -1;
end;

function TCalibrationOptimizer.GetClosest(I: Integer; var Frags: Array of Double; Mass: Double): Integer;
(*
 * Recursively checks that the closest fragment to a peak is selected.
 *
 * Arguments ------
 * i: index in fragment list to start checking
 * frags: list of mass fragments
 * mass: mass of peak being matched
 *)

var
  d: Double;

begin
  d := Abs(Frags[I] - Mass);
  if (Length(Frags) > I + 1) and (d > Abs(Frags[I + 1] - Mass)) then
    I := GetClosest(I + 1, Frags, Mass)
  else
    if (I - 1 >= 0) and (d > Abs(Frags[I - 1] - Mass)) then
      I := GetClosest(I - 1, Frags, Mass);

  Result := I;
end;



function TCalibrationOptimizer.CalcMasses(Channels : Array of Double; SpecBinSize, StartTime, MassOverTime, MassOffset: Double): TArrayOfDouble;
(*
 * Fast conversion from flightime to mass.
 *)

var
  i: Integer;
  peak: Double;
  peaks: TArrayOfDouble;

begin
  SetLength(peaks, Length(channels));

  for i := 0 to Length(Channels) - 1 do
  begin
    peak := Power(((Channels[i] * 0.001 * SpecBinSize + StartTime) * MassOverTime + MassOffset), 2.0);
    peaks[i] := peak;
  end;
  Result := peaks;
end;



function TCalibrationOptimizer.GetDistanceNPZ(Masses: Array of Double; Ranges: TArrayofRanges; ShowCorrectValues, Proportions: Boolean; MassThresh: Integer):TList;
(*
 * Returns list of how how far into the No Peak Zone the given masses are.
 *
 * Arguments: -------
 * masses: list of peak masses
 * ranges: list of tuples representing 'No Peak Zones'
 * show_correct_peaks: whether, if a peak is not in the 'No Peak Zone',
 * to show how far into the 'correct zone' it is.
 * proportions: boolean, whether to show distance into zone by proportion
 * instead of distance.
 * mass_thresh: how far up the amu scale to find the distance into NPZ,
 * default 800
 *)

var
  dists: TList;
  mass, val, rangeSize: Double;
  truncd: Integer;
  zone: TRange;
  myDouble: TDouble;

begin
  dists := TList.Create();
  for mass in Masses do
  begin
    if mass <= MassThresh then
    begin
      truncd := Trunc(mass);
      zone := ranges[truncd];
      val := Min(Abs(mass - zone[0]), Abs(mass - zone[1]));

      if Not(Proportions) then
      begin
        if (zone[0] < mass) and (mass < zone[1]) then
        begin
          myDouble := TDouble.Create();
          myDouble.FValue := val;
          dists.Add(myDouble);
        end
        else
        begin
          if ShowCorrectValues then
          begin
            myDouble := TDouble.Create();
            myDouble.FValue := val * -1;
            dists.Add(myDouble);
          end
          else
          begin
            myDouble := TDouble.Create();
            myDouble.FValue := 0;
            dists.Add(myDouble);
          end;

        end;
      end
      else
      begin
        rangeSize := zone[1] - zone[0];
        if (zone[0] < mass) and (mass < zone[1]) then
        begin
          myDouble := TDouble.Create();
          myDouble.FValue := val / rangeSize;
          dists.Add(myDouble);
        end
        else
        begin
          myDouble := TDouble.Create();
          myDouble.FValue := 0;
          dists.Add(myDouble);
        end;
      end;

    end;

  end;

  Result := dists;

end;



function TCalibrationOptimizer.GetSusPeaks(Masses: Array of Double; Ranges: TArrayofRanges; Thresh: Double): TSusPeaks;

(*
 * Returns list of all peaks with distance / proportion into the No Peak Zone
 * above the given threshold, thresh as well as the mean distance into the
 * No Peak Zone.
 *
 * Arguments -------
 * masses: list of peak mass values.
 * ranges: list of tuples representing no peak zones.
 * thresh: threshold beyond which peaks in the No Peak Zone are suspicious.
 *)
const
  c_NPZ_Edge = 717;

var
  i, j: Integer;
  susses, peaks: TList;
  cumulative: Double;
  myDouble: TDouble;
  suscount: Integer;

begin
  peaks := TList.Create();

  susses := GetDistanceNpz(Masses, Ranges, False, False, c_NPZ_Edge);

  j := 0;
  cumulative := 0;

  for i := 0 to Length(Masses) - 1 do
  begin
    if Masses[i] < c_NPZ_Edge then
    begin
      if TDouble(susses[j]).FValue > Thresh then
      begin
        myDouble := TDouble.Create();
        myDouble.FValue := Masses[i];
        peaks.Add(myDouble);
      end;

      cumulative := cumulative + TDouble(susses[j]).FValue;

      Inc(j);
    end;

  end;

  Result.FPeaks := peaks;
  Result.FMean := cumulative / susses.count;

end;



function TCalibrationOptimizer.IsFindable(Floor, Ceiling: Integer): Boolean;

begin
  if (Abs(Floor - Ceiling) <= 1) then
    Result := False
  else
    Result := True;

end;



function TCalibrationOptimizer.CalcFragMatches(Masses, Frags: Array of Double; Threshes: Array of Double; AB: Boolean): TArrayofArrayofList;
{*
 * Matches known compound/element masses to peaks in a given spectrum for
 * each threshold in the list of passed threshold.
 *
 * Returns the list of masses matched, list of fragments matched to those
 * masses for each threshold, and the list of distances between the
 * masses and the matched fragments.
 *
 * Arguments -------
 * masses: list of masses for a spectrum
 * frags: fragment list
 * threshes: list of thresholds to use to check if a fragment is close
 *   enough to a peak to call it a match.
 * ab: whether to use absolute value for calculated distances, affects
 *   the average distance per spectrum.
 *
 *}
var
  numThreshes, massIndex, i, j: Integer;
  mass: Double;
  notFound: Boolean;
  floor, ceiling, num: Integer;
  dist: Double;
  stats:  TArrayOfArrayOfList;
  myDouble: TDouble;


begin
  numThreshes := Length(Threshes);
  for i := 0 to numThreshes - 1 do
  begin
    for j := 0 to 2 do
    begin
      stats[i][j] := Tlist.Create();
    end;
  end;
  for mass in Masses do
  begin
    if (mass < MaxValue (Frags)) then
    begin
      notFound := True;
      i := Length(Frags) div 2;
      floor := 0;
      ceiling := Length(frags) - 1;

      while notFound do
      begin
        dist := Frags[i] - mass;

        if (Abs(dist) < MaxValue(Threshes)) then
        begin
          notFound := False;
          i := GetClosest(i, Frags, mass);
          dist := Frags[i] - mass;

          for j := 0 to numThreshes - 1 do
          begin
            if (Abs(dist) < Threshes[j]) then
            begin
              myDouble := TDouble.Create();
              myDouble.FValue := mass;
              stats[j][0].Add(myDouble);
              myDouble := TDouble.Create();
              myDouble.FValue := Frags[i];
              stats[j][1].Add(myDouble);

              myDouble := TDouble.Create();

              if AB then
                myDouble.FValue := Abs(Frags[i] - mass)
              else
                myDouble.FValue := Frags[i] - mass;

              stats[j][2].Add(myDouble);
            end;

          end;

        end
        else
          if (dist > 0) then
          begin
            notFound := IsFindable(floor, ceiling);
            ceiling := i;
            num := Abs(floor - i);
            if Not(num = 1) then
              i := i - (num div 2)
            else
              i := i - 1;

          end
          else
          begin
            notFound := IsFindable(floor, ceiling);
            floor := i;
            num := Abs(ceiling - i);
            if Not(num = 1) then
              i := i + (num div 2)
            else
              i := i + 1;
          end;
      end;
    end;

  end;

  Result := stats;
end;



function TCalibrationOptimizer.GetListMean(List:  TList): Double;

var
  i: Integer;
  cumulative: Double;

begin
  cumulative := 0;
  for i := 0 to List.Count - 1 do
    cumulative := cumulative + TDouble(List[i]).FValue;
  Result := cumulative / List.Count;
end;

function TCalibrationOptimizer.CalcNewSpectrumStats(Row: TSpectrum; Frags : Array of Double; SlopeVal, OffsetVal : Double; Ranges : TArrayofRanges; Augment, NumPeaks : Boolean): TPropLDistHDist;
(*
 * function TCalibrationOptimizer.which asseses the calibration of a spectrum.
 * If augment is True slope_val and offset_val are treated as proportions
 * of slope/offset to augment original values with. Otherwise the values
 * are treated a the new values for slope and offset.
 *
 * Returns a proportion of matched peaks, as well as the avg distance from
 * fragment for a thrshold of .003 and .007 amu. If num_peaks is true also
 * returns the number of matched peaks.
 *
 * Arguments -------
 * row: row corresponding to spectra
 * frags: pd.Series of fragments
 * slope_val: either a proportion of slope to augment slope with or a new value
 * for slope.
 * offset_val: either a proportion of offset to augment offset with or a new
 * value for offset.
 * ranges: array of ranges of npz.
 * augment: whether to treat slope_val/offset_val as a proportion or value
 * num_peaks: if true also returns the number of peaks of matched per spectrum
 * ranges: String, list of tuples representing No Peak Zones.
 *)

var
  i: Integer;
  matchesPossible :  Integer;
  slope, offset, prop, lowDist, HighDist: Double;
  stats: TArrayOfArrayOfList;
  peaks: TArrayOfDouble;
  threshes: Array of Double;
  masses, dist1, dist2: TList;
  PropLDistHDist: TPropLDistHDist;
  npzObject: TSusPeaks;


begin
  if Augment then
  begin
    slope := Row.FMassOverTime + SlopeVal * Row.FMassOverTime;
    offset := Row.FMassOffset + OffsetVal * Row.FMassOffset;
  end
  else
  begin
    slope := SlopeVal + Row.FMassOverTime;
    offset := OffsetVal + Row.FMassOffset;
  end;

  peaks := CalcMasses(Row.Fchannels, Row.FSpecBinSize, Row.FStartFlightTime, slope, offset);
  SetLength(threshes, 2);
  threshes[0] := 0.003;
  threshes[1] := 0.007;

  stats := CalcFragMatches(peaks, Frags, threshes, True);
  masses := stats[0][0];
  dist1 := stats[0][2];
  dist2 := stats[1][2];


  npzObject := GetSusPeaks(peaks, Ranges, 0.1);


  matchesPossible := 0;
  for i := 0 to Row.FSize - 1 do
  begin
    if (peaks[i] < 236) then
      Inc(matchesPossible);
  end;

  prop := masses.Count / (matchesPossible + 1);
  lowDist := 0;
  highDist := 0;

  if(dist1.Count) > 0 then
    lowDist := GetListMean(dist1);
  if(dist2.Count) > 0 then
    HighDist := GetListMean(dist2);

  PropLDistHDist := TPropLDistHDist.Create();
  PropLDistHDist.FProp := prop;
  PropLDistHDist.FLDist := lowDist;
  PropLDistHDist.FHDist := highDist;
  PropLDistHDist.FNpzMeanDist := npzObject.FMean;
  PropLDistHDist.FNpzPeakCount := npzObject.FPeaks.Count;
  if NumPeaks then
    PropLDistHDist.FNumMasses := masses.Count;

  Result := PropLDistHDist;

 end;



function TCalibrationOptimizer.ReadHeaderLine(FromFile: Integer): WideString;
// Reads from file until end of line or end of file is encountered.
// The input is saved in 'line' and 'ok' is set to 'TRUE' if any characters
// are in 'line' otherwise 'ok' is set to 'FALSE'.
const
  c_Max_String_Chars = 1023;
var
  i: CARDINAL;
  num: LONGINT;
  tempStr: AnsiChar;
  tempLine: Array[0..c_Max_String_Chars] of AnsiChar;
  eofile, ok: Boolean;
begin
  num := 0; //Avoid compiler warning
  try
    // Loop till an EOF or a LF character is detected.
    ok := FALSE;
    i := 0;
    eofile := FALSE;
    while (ok = false) and (eofile = false) do
    begin
      // Read a character from the file.
      try
        num := FileRead(FromFile, tempStr, SizeOf(tempStr));
      except
        on E: Exception do
        begin //Log and swallow
          //TLogger.LogException(Classname,E,'ReadRawHeaderLine ignored this exception');
          num := 0; //Note that this will cause code below to set eofile := TRUE
        end;
      end;

      // Check if we have reached the end of the file.
      if (num = 0) then
      begin
        tempLine[i] := #0;
        eofile := TRUE;
      end
      else
      begin
        // Store the character in the Ansi string.
        tempLine[i] := tempStr;

        // If a LF character is detected, add a #0 terminator
        if (tempLine[i] = #10) then
        begin
          tempLine[i] := #0;

          // If the previous character was a CR, set it to #0 also.
          if (tempLine[i-1] = #13) then
            tempLine[i-1] := #0;

          // Set flag to indicate the LF character was found.
          ok := TRUE;
        end
        else if (i = c_Max_String_Chars) then //if the max line length has been exceeded
        begin
          try //Want raise and log an exception here because this should not happen
            tempLine[i] := #0; //We force the final character to be 0 terminator
            //raise ELogException.Create(Classname,'ReadRawHeaderLine max line length has been exceeded: "' + TLogger.FilterControlCodes(String(tempLine)) + '"');
          except //But now that we've raised and logged it, we swallow the exception and try to continue.
            ok := TRUE; // Set flag as if the LF character was found so we can continue
          end;
        end;
        Inc(i);
      end;
    end;

    // Convert the AnsiString to a WideString.
    Result := String(tempLine);
  except
    on E:Exception do
    begin
      //TLogger.LogException(Classname,E,'ReadRawHeaderLine failed',[]);
      raise;
    end;
  end;
end;



function TCalibrationOptimizer.RemoveQuotes(Str : String): String;


begin
  Delete(Str, Length(Str), 1);
  Delete(Str, 1, 1);
  Result := Str;
end;



function TCalibrationOptimizer.GetMasses(Loc: String): TMasses;
(*
 * Read in fragment table and return the fragment masses.
 *)

var
  ret, mass: Double;
  masses: TMasses;
  myDouble: TDouble;
  massStr: String;
  ascSpecFile, index: Integer;
  lineStr: WideString;


begin
  index := 0;
  ascSpecFile:= FileOpen(Loc, fmOpenRead);
  repeat
    lineStr:= ReadHeaderLine(ascSpecFile);
    if (lineStr <> '') then
    begin
      massStr:= Item(lineStr,',',0);
      massStr := RemoveQuotes(massStr);
      mass := StrToFloat(massStr);
      masses[index] := mass;

      Inc(index);
    end;
  until (lineStr = '');
  FileClose(ascSpecFile);
  Result := masses;

end;



function TCalibrationOptimizer.CreateAxisValues(Start, Finish: Double; Count: Integer): TArrayOfDouble;

(*
 * Creates evenly distributed axis values given a start and finish point, and
 * the desired number of intervals.
 *)

var
  i: Integer;
  length, interval, current: Double;
  axis: TArrayOfDouble;


begin
  SetLength(axis, Count);
  length := ABS(Finish - Start);
  if Start > Finish then
    interval := -length / (Count - 1)
  else
    interval := length / (Count - 1);
  current := Start;

  for i := 0 to Count - 1 do
  begin
    axis[i] := current;
    current := current + interval;
  end;

  Result := axis;
end;



function TCalibrationOptimizer.FindIndex(Axis: Array of Double; Value: Double): Integer;

var
  i: Integer;

begin
  for i := 0 to Length(Axis) - 1 do
    if (Axis[i] = Value) then
      Result := i;

end;



function TCalibrationOptimizer.GetBestOffset(Spectrum: TSpectrum; SlopeRange, OffsetRange: TRange; Frags : TMasses; NpzRanges: TArrayofRanges; Offsets, Slopes: Integer; Prev: Double; NpzPeakLimit: Integer; NpzDistLimit: Double): TPropSlopeOffset;

(*
 * Find best amount of slope/offset to add/subtract to achieve the optimal
 * calibration for a spectrum. Calibration is measured using mass fragments.
 * A spectrum with more matches to known frags is more calibrated than one
 * with fewer. A spectrum whose matches are very close to known mass is more
 * calibrated than one with some that are further away.
 *
 * Returns tuple containing the best performning offset and slope changes.
 *
 *
 * Arguments -------
 * spectrum: row from dataframe containing information on a spectrum
 * slope_range: data structure containing min, max slope to try, slope erros
 *               are typically smaller than offset errors.
 * offset_range: data structure containing min, max offset agumentation
 *               to try. This method shrinks the range iteratively until
 *               the best offset is achieved.
 * npzRanges: no peak zone list, sometimes called ranges
 * prev: previous best proportion, used for recursion
 * offsets: number of evenly spaced offsets to generate in range
 * slopes: number of evenly spaced slopes to generate in range
 * frags: a list of mass fragments
 * loc: file location of fragment database
 * npzPeakLimit: int, for recursion: max number of no peak zone peaks allowable
 * npzDistLimit: float, for recursion: max avg distance into no peak zone allowable
 *)

var
  specValues: TPropLDistHDist;
  prop, bestProp, bestLD, bestHD, bestOffset, bestSlope : Double;
  mults: Array[0..1] of Double;
  slopeAxis, offsetAxis: TArrayofDouble;
  i, j: Integer;
  slope, slopeMult, slopeVal, offset, offsetMult, offsetVal: Double;
  improved: Boolean;
  low, high, numPeaks, avgDist: Double;
  slopeIndex, offsetIndex: Integer;
  slopeEdge, offsetEdge: Double;
  newOffsetRange, newSlopeRange: TRange;
  newSlopes, newOffsets: Integer;
  recValues : TPropSlopeOffset;



begin
  if (NpzPeakLimit = -1) and (NpzDistLimit = -1) then
  begin
    specValues := CalcNewSpectrumStats(Spectrum, Frags, 0, 0, NpzRanges, True, False);
    prop := specValues.FProp;
    NpzPeakLimit := specValues.FNpzPeakCount;
    NpzDistLimit := specValues.FNpzMeanDist;

    bestProp := prop;
    prev := prop;
  end
  else
  begin
    bestProp := 0;
  end;

  offsetEdge := 0;
  slopeEdge := 0;
  bestOffset := 0;
  bestSlope := 0;
  bestLD := 1;
  bestHD := 1;
  mults[0] := 1;
  mults[1] := -1;

  slopeAxis := CreateAxisValues(SlopeRange[0], SlopeRange[1], Slopes);

  for i := 0 to Length(slopeAxis) - 1 do
  begin
    slope := slopeAxis[i];
    for slopeMult in mults do
    begin
      slopeVal := slope * slopeMult;
      offsetAxis := CreateAxisValues(OffsetRange[0], OffsetRange[1], Offsets);
      for j := 0 to Length(offsetAxis) - 1 do
      begin
        offset := offsetAxis[j];
        for offsetMult in  mults do
        begin
          improved := False;
          offsetVal := offset * offsetMult;

          specValues := CalcNewSpectrumStats(Spectrum, Frags, slopeVal, OffsetVal, NpzRanges, True, False);
          prop := specValues.FProp;
          low := specValues.FLDist;
          high := specValues.FHDist;
          numPeaks := specValues.FNpzPeakCount;
          avgDist := specValues.FNpzMeanDist;

          if(prop > bestProp) and (numPeaks <= npzPeakLimit) and (avgDist <= npzDistLimit) then
            improved := True
          else
          begin
            if (prop = bestProp) and (bestHD - high > 0) and (bestLD - low  > 0) then
              improved := True
          end;

          if improved then
          begin
            bestProp := prop;
            bestOffset := offsetVal;
            bestSlope := slopeVal;
            bestLD := low;
            bestHD := high;

            offsetIndex := FindIndex(offsetAxis, bestOffset * offsetMult);
            slopeIndex := FindIndex(slopeAxis, bestSlope * slopeMult);

            offsetEdge := 2 * ABS(0.5 - (offsetIndex + 0.1) / Length(offsetAxis));
            slopeEdge := 2 * ABS(0.5 - (slopeIndex + 0.1) / Length(slopeAxis));
          end;

        end;

      end;

    end;

  end;

  if bestProp > Prev then
  begin
    newOffsetRange[0] := bestOffset - (0.5 / offsetEdge) * bestOffset;
    newOffsetRange[1] := bestOffset + (0.5 / offsetEdge) * bestOffset;
    newSlopeRange[0] := bestSlope + (0.5 / slopeEdge) * bestSlope;
    newSlopeRange[1] := bestSlope - (0.5 / slopeEdge) * bestSlope;
    newOffsets := 20;
    newSlopes := 5;



    recValues := GetBestOffset(Spectrum, newSlopeRange, newOffsetRange, Frags, NpzRanges, newOffsets, newSlopes, bestProp, NpzPeakLimit, NpzDistLimit);
                               //spectrum, range, range, string, array of ranges, int, int, double, int, double
    if recValues.FProp >= bestProp then
    begin
      bestProp := recValues.FProp;
      bestSlope := recValues.FSlope;
      bestOffset := recValues.FOffset;
    end;

  end;

  Result := TPropSlopeOffset.Create();
  Result.FProp := bestProp;
  Result.FSlope := bestSlope;
  Result.FOffset := bestOffset;

end;



function TCalibrationOptimizer.ReadCsv(Loc: String): TSpectrum;

var
  ret: Double;
  chanStr, countsStr: String;
  ascSpecFile, channel, index: Integer;
  lineStr: WideString;
  header: Boolean;
  spec: TSpectrum;


begin
  spec := TSpectrum.Create;
  SetLength(spec.FChannels, 5000);
  index := 0;
  ascSpecFile:= FileOpen(Loc, fmOpenRead);
  repeat
    lineStr:= ReadHeaderLine(ascSpecFile);
    if (lineStr <> '') then
    begin
      if lineStr = 'SOFH' then
        header := True
      else
      begin
        if header then
        begin
          if lineStr = 'EOFH' then
            header := False
          else
          begin
            chanStr:= Item(lineStr,':',0);
            if chanStr = 'Mass/Time' then
            begin
              countsStr := Item(lineStr,':',1);
              spec.FMassOverTime := StrToFloat(countsStr);
            end
            else
            begin
              if chanStr = 'MassOffset' then
              begin
                countsStr := Item(lineStr,':',1);
                spec.FMassOffset := StrToFloat(countsStr);
              end
              else
              begin
                if chanStr = 'SpecBinSize' then
                begin
                  countsStr := Item(lineStr,':',1);
                  spec.FSpecBinSize := StrToFloat(countsStr);
                end
                else
                begin
                  if chanStr = 'StartFlightTime' then
                  begin
                    countsStr := Item(lineStr,':',1);
                    spec.FStartFlightTime := StrToFloat(countsStr);
                  end;

                end;

              end;

            end;

          end;

        end
        else
        begin
          chanStr:= Item(lineStr,',',0);
          spec.FChannels[index] := StrToFloat(chanStr);
          Inc(index);
        end;

      end;

    end;


  until (lineStr = '');

  FileClose(ascSpecFile);

  spec.FSize := index;

  Result := spec;
end;



function TCalibrationOptimizer.GetHydrocarbons():TArrayOfDouble;

(*
 * Return array of hydrocarbon masses in amu by calculating all possible
 * hydrocarbons with at most 51 carbons.
 *)

 const
   c_Max_Num_Carbs = 51;
   c_Diff = 0.1564;

 var
  hydrocarbon, prev, mass: double;
  realHydrocarbons, expandedHydrocarbons: TArrayOfDouble;
  c, i, j, max, dist, num: Integer;

begin

  SetLength(realHydrocarbons, c_Max_Num_Carbs);
  i := 0;

  for c := 1 to c_Max_Num_Carbs do
  begin
    realHydrocarbons[i] := (c * 12 + (2 * c + 1) * 1.00782);
    Inc(i);
  end;

  i := 0;
  j := 0;

  max := Trunc(realHydrocarbons[c_Max_Num_Carbs - 2]);
  SetLength(expandedHydrocarbons, max);

  for hydrocarbon in realHydrocarbons do
  begin
    expandedHydrocarbons[j] := hydrocarbon;
    Inc(j);

    if i < Length(realHydrocarbons) - 1 then
    begin
      prev := hydrocarbon;
      dist := Trunc(realHydrocarbons[i + 1]) - Trunc(hydrocarbon) - 1;
      for num := 0 to dist - 1 do
      begin
        mass := prev + 1 + c_Diff / dist;
        expandedHydrocarbons[j] := mass;
        Inc(j);
        prev := mass;
      end;

    end;

    Inc(i);

  end;

  Result := expandedHydrocarbons;
end;

function TCalibrationOptimizer.GetIsotopes(Loc: String): TList;

var
  ascSpecFile, i: Integer;
  lineStr: WideString;
  pair, mass: String;
  isotopes: TList;
  myDouble: TDouble;
  done: Boolean;



begin
  ascSpecFile:= FileOpen(Loc, fmOpenRead);
  isotopes := TList.Create();
  repeat
    lineStr:= ReadHeaderLine(ascSpecFile);
    if (lineStr <> '') then
    begin
      i := 1;
      done := False;
      while not(done) do
      begin
        pair := Item(lineStr,'(', i);
        Inc(i);
        if pair = '' then
          done := True
        else
        begin
          mass := Item(pair,',', 0);
          myDouble := TDouble.Create;
          myDouble.FValue := StrToFloat(mass);
          isotopes.add(myDouble);

        end;


      end;

    end;
  until (lineStr = '');
  FileClose(ascSpecFile);

  Result := isotopes;

end;



function TCalibrationOptimizer.CombineMasses(Carbons: TArrayOfDouble; Isotopes: TList): TArrayOfDouble;

var
  masses: TArrayOfDouble;
  max, index, i: Integer;

begin
  max := Isotopes.Count + Length(Carbons);
  SetLength(masses, max);
  index := 0;

  for i := 0 to Isotopes.Count - 1 do
  begin
    masses[index] := TDouble(Isotopes[i]).FValue;
    Inc(index);
  end;


  for i := 0 to Length(carbons) - 1 do
  begin
    masses[index] := Carbons[i];
    Inc(index);
  end;

  SetLength(Carbons, 0);
  Isotopes.Free;
  Result := masses;

end;



function TCalibrationOptimizer.GetRanges(Masses: TArrayOfDouble; MassLimit: Integer): TArrayOfRanges;

var
  ranges: TArrayofRanges;
  mass: Double;
  i, rounded, truncated: Integer;

begin
  SetLength(ranges, MassLimit);
  for i := 0 to MassLimit - 1 do
  begin
    ranges[i][0] := i;
    ranges[i][1] := i + 1;
  end;
  for mass in Masses do
  begin
    i := Trunc(mass);
    if mass < 236 then
    begin
      rounded := Round(mass);
      if(rounded = i + 1) and (mass < ranges[i][1]) then
        ranges[i][1] := mass
      else
        if (rounded = i) and (mass > ranges[i][0]) then
          ranges[i][0] := mass;

    end
    else
    begin
      ranges[i][0] := mass;
      ranges[i][1] := i + 0.9871;
    end;
  end;
  SetLength(Masses, 0);
  Result := ranges;

end;

procedure TCalibrationOptimizer.OptimizeCalibration;

var
  isoLoc, csvLoc, massLoc: String;
  spec: TSpectrum;
  isos: TList;
  carbons, masses: TArrayofDouble;
  frags: TMasses;
  ranges: TArrayofRanges;
  output: TPropSlopeOffset;
  slopeRange, offsetRange: TRange;
  slopes, offsets: Integer;
  prop, slope, offset, test: Double;

begin
  csvLoc := 'C:\Users\arreg\Documents\optimizer_data\091405-01.csv';
  isoLoc := 'C:\Users\arreg\Documents\optimizer_data\Elements.txt';
  massLoc := 'C:\Users\arreg\Documents\optimizer_data\Fragment Table.csv';



  isos := GetIsotopes(isoLoc);

  carbons := GetHydrocarbons();

  masses := CombineMasses(carbons, isos);
  ranges := GetRanges(masses, 800);

  frags := GetMasses(massLoc);

  spec := ReadCsv(csvLoc);

  slopeRange[0] := 0.00001;
  slopeRange[1] := 0.0000001;
  offsetRange[0] := 0.001;
  offsetRange[1] := 0.000001;
  offsets := 20;
  slopes := 10;

  output := GetBestOffset(spec, slopeRange, OffsetRange, frags, ranges, offsets, slopes, 0,  -1, -1);
  //spectrum, range, range, string, array of ranges, int, int, double, int, double
  prop := output.FProp;
  slope := output.FSlope;
  offset := output.FOffset;
  test := 0;
end;

end.
