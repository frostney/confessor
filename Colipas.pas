(*
  Colipas
  =======

  Command-line library for FreePascal/Delphi

  Colipas is inspired by Commander (http://visionmedia.github.com/commander/) and
  Commander.js (http://tjholowaychuk.com/post/9103188408/commander-js-nodejs-command-line-interfaces-made-easy).
  Originally, I wanted to name the project Commander.pas, but because it's not a 1:1 port of the Commander interface,
  Colipas was the next name I came up with.

  Sorry, if a few functions/procedures are a bit verbose. I've been doing a lot of Objective-C lately.


  Features
  --------

  - Uses callbacks to actions
  - Utilize lists
  - Confirm messages (yes/no)
  - Prompts with additional default statement
  - Add options to your command-line apps as one or multiple char, one or more strings or both of those options
  - Options with parameter support
  - Supports LibNotify and Growl (through GrowlNotify) if available


  Usage
  -----
    1. Include Colipas in your application
    2. Sub-class TCommandApp
    3. Implement the method Execute in your sub-class and make sure it calls the inherited method
    4. Create an instance of your sub-class.
    5. In your main code file add call the methods you need, for example:
         MySubClass.Print(SomeString, [poLibNotify, poGrowl]); //< This send a notification to LibNotify or Growl if available
         MySubClass.Option('s', 'someoption', @CallbackToMyOption); //< If the user would be call the application with the parameter
           // -s or --someoption, CallbackToMyOption would be called, whereas CallbackMyOption should be implemented as a method of MySubClass
    6. Call MySubClass.Execute
    (You need to rename MySubClass to the atual)

  Note:
    Some features are missing when using Colipas on Windows.


  See the examples for detailed information on usage.

  It is recommended to use Colipas with InstantFPC (http://wiki.lazarus.freepascal.org/InstantFPC).


  License
  -------

  MIT license, see LICENSE.txt for more information.


  Author
  ------

  (C) Johannes Stein 2011
  http://www.freeze-dev.com
  johannesstein@freeze-dev.com
*)

unit Colipas;

interface

{$IFDEF FPC}
  {$MODE OBJFPC}

  {$DEFINE CAN_INLINE}
{$ENDIF}

uses
  {$IFDEF UNIX}
  Unix,
  {$ENDIF}
  SysUtils,
  Classes;

type
  // Callbacks
  TCmdOptionCallback = procedure(cbResult: AnsiString) of object;
  TCmdChooseCallback = procedure(cbResult: Integer) of object;
  TCmdPromptCallback = procedure(cbResult: AnsiString) of object;
  TCmdPasswordCallback = procedure(cbResult: AnsiString) of object;
  TCmdConfirmCallback = procedure(cbResult: Boolean) of object;

  TPrintOutputOption = (poConsole, poGrowl, poLibNotify);
  TPrintOutputOptions = set of TPrintOutputOption;

  { TCommandApp }
  TCommandApp = class
  private
    fCommandStrList: TStringList; //< Stores all available options

    function IsGrowlNotifyInstalled(): Boolean; //< Checks if Growl is installed (Mac OS X only)
    function IsLibNotifyInstalled(): Boolean; //< Check is libNotify is installed (Linux only)

    procedure AddParamsToList(aChars: array of Char; aStrings: array of AnsiString; aDescription: AnsiString; aParameterName: String = '');
  protected
    fName, fVersion: AnsiString;

    function HasParam(aChars: array of Char; aStrings: array of AnsiString): Boolean; Overload;
    function HasParam(aChar: Char; aString: AnsiString): Boolean; Overload;
    function HasParam(aChar: Char): Boolean; Overload;
    function HasParam(aString: AnsiString): Boolean; Overload;

    function GetParamPos(aChars: array of Char; aStrings: array of AnsiString): Integer; Overload;
    function GetParamPos(aChar: Char; aString: AnsiString): Integer; Overload;
    function GetParamPos(aChar: Char): Integer; Overload;
    function GetParamPos(aString: AnsiString): Integer; Overload;

    procedure ShowHelp(cbResult: AnsiString);
    procedure ShowVersion(cbResult: AnsiString);
  public
    constructor Create();
    destructor Destroy; Override;

    procedure Print(aString: AnsiString); Overload;
    procedure Print(aString: AnsiString; Args: array of const; OutputOption: TPrintOutputOptions = [poConsole]); Overload;

    procedure Option(aChar: Char; aDescription: AnsiString; aCallback: TCmdOptionCallback); Overload;
    procedure Option(aChars: array of Char; aDescription: AnsiString; aCallback: TCmdOptionCallback); Overload;
    procedure Option(aString, aDescription: AnsiString; aCallback: TCmdOptionCallback); Overload;
    procedure Option(aStrings: array of AnsiString; aDescription: AnsiString; aCallback: TCmdOptionCallback);
    procedure Option(aChar: Char; aString, aDescription: AnsiString; aCallback: TCmdOptionCallback); Overload;
    procedure Option(aChars: array of Char; aStrings: array of AnsiString; aDescription: AnsiString; aCallback: TCmdOptionCallback);

    procedure OptionWithParameter(aChar: Char; aDescription, aParameterName: AnsiString; aCallback: TCmdOptionCallback); Overload;
    procedure OptionWithParameter(aChars: array of Char; aDescription, aParameterName: AnsiString; aCallback: TCmdOptionCallback); Overload;
    procedure OptionWithParameter(aString, aDescription, aParameterName: AnsiString; aCallback: TCmdOptionCallback); Overload;
    procedure OptionWithParameter(aStrings: array of AnsiString; aDescription, aParameterName: AnsiString; aCallback: TCmdOptionCallback);
    procedure OptionWithParameter(aChar: Char; aString, aDescription, aParameterName: AnsiString; aCallback: TCmdOptionCallback); Overload;
    procedure OptionWithParameter(aChars: array of Char; aStrings: array of AnsiString; aDescription, aParameterName: AnsiString; aCallback: TCmdOptionCallback); Overload;

    procedure Choose(aChooseText: AnsiString; aList: array of AnsiString; aDefaultValue: Integer; aCallback: TCmdChooseCallback); Overload;
    procedure Choose(aChooseText: AnsiString; aList: array of Char; aDefaultValue: Integer; aCallback: TCmdChooseCallback); Overload;
    procedure Choose(aChooseText: AnsiString; aList: array of Single; aDefaultValue: Integer; aCallback: TCmdChooseCallback); Overload;
    procedure Choose(aChooseText: AnsiString; aList: array of Integer; aDefaultValue: Integer; aCallback: TCmdChooseCallback); Overload;

    procedure Prompt(aPromptText: AnsiString; aCallback: TCmdPromptCallback); Overload;
    procedure Prompt(aPromptText, aDefaultText: AnsiString; aCallback: TCmdPromptCallback); Overload;

    //procedure Password(aPasswordText: AnsiString; aMaskChar: Char; aCallback: TCmdPasswordCallback);

    procedure Confirm(aConfirmText: AnsiString; aCallback: TCmdConfirmCallback); Overload;
    procedure Confirm(aConfirmText: AnsiString; aDefaultValue: Boolean; aCallback: TCmdConfirmCallback); Overload;

    procedure Execute(); virtual;
  published
    property Name: AnsiString read fName write fName;
    property Version: AnsiString read fVersion write fVersion;
  end;

  // https://gist.github.com/1208591
  function CmdToString(Command: AnsiString; var CmdResult: Integer): TStringList; Overload;
  function CmdToString(Command: AnsiString): TStringList; Overload;

// Messages
resourcestring
  rsInputNotNumber = 'Error: Input is not a number';
  rsInputNotString = 'Error: Input is not a string';

implementation

// https://gist.github.com/1208591
function CmdToString(Command: AnsiString; var CmdResult: Integer): TStringList;
var
  formattedDateTime: AnsiString = '';
  Filename: AnsiString = '';
  tmpStringList: TStringList;
begin
  {$IFDEF UNIX}
  // Format date time string & construct filename
  DateTimeToString(formattedDateTime, 'yyyy-mm-dd_hh-nn-ss-z', Now);
  FileName := GetTempDir(true) + formattedDateTime + '.txt';

  // Create temporary file in temporary folder with timestamp as filename
  CmdResult := fpSystem(Command + ' > ' + FileName);

  // Create string list
  tmpStringList := TStringList.Create;

  // Load file into temporary string list
  tmpStringList.LoadFromFile(Filename);

  // Delete file
  DeleteFile(Filename);

  // Return temporary string list
  Result := tmpStringList;
  {$ELSE}
  Result := nil;
  {$ENDIF}
end;

function CmdToString(Command: AnsiString): TStringList;
var
  tmpInteger: Integer;
begin
  Result := CmdToString(Command, tmpInteger);
end;

{ TCommandApp }

constructor TCommandApp.Create;
begin
  fName := '';
  fVersion := '1.0.0';

  fCommandStrList := TStringList.Create;
end;

destructor TCommandApp.Destroy;
begin
  fCommandStrList.Destroy;
end;

function TCommandApp.IsGrowlNotifyInstalled(): Boolean;
begin
  Result := false;

  {$IFDEF DARWIN}
    if CmdToString('which growlnotify')[0] <> '' then Result := true;
  {$ENDIF}
end;

function TCommandApp.IsLibNotifyInstalled(): Boolean;
begin
  Result := false;

  {$IFDEF LINUX}
    if CmdToString('which libnotify-send')[0] <> '' then Result := true;
  {$ENDIF}
end;

procedure TCommandApp.AddParamsToList(aChars: array of Char;
  aStrings: array of AnsiString; aDescription: AnsiString;
  aParameterName: String);
var
 tmpString: String;
 i, j: Integer;
begin
  tmpString := '';

  for i := 0 to Length(aChars) - 1 do
  begin
    tmpString := tmpString + '-' + aChars[i];
    if ((Length(aChars) > 0) and (i < Length(aChars) - 1)) then tmpString := ', ';
  end;

  if Length(aStrings) > 0 then
  begin
    tmpString := tmpString + ',  ';

    for j := 0 to Length(aStrings) - 1 do
    begin
      tmpString := tmpString + '--' + aStrings[j];
      if ((Length(aStrings) > 0) and (j < Length(aStrings) - 1)) then tmpString := ', ';
    end;
  end;

  if (aParameterName <> '') then tmpString := tmpString + ' <' + aParameterName + '>';

  tmpString := tmpString + '\t ';
  tmpString := tmpString + aDescription;
  fCommandStrList.Add(tmpString);

end;

function TCommandApp.HasParam(aChars: array of Char; aStrings: array of AnsiString
  ): Boolean;
begin
  if GetParamPos(aChars, aStrings) = -1 then Result := false
  else Result := true;
end;

function TCommandApp.HasParam(aChar: Char; aString: AnsiString): Boolean;
begin
  Result := HasParam([aChar], [aString]);
end;

function TCommandApp.HasParam(aChar: Char): Boolean;
begin
  Result := HasParam([aChar], []);
end;

function TCommandApp.HasParam(aString: AnsiString): Boolean;
begin
  Result := HasParam([], [aString]);
end;

function TCommandApp.GetParamPos(aChars: array of Char;
  aStrings: array of AnsiString): Integer;
var
  i, j, k: Integer;
begin
 Result := -1;

  if ParamCount >= 1 then
  begin
    for i := 1 to ParamCount do
    begin

      if Length(aChars) > 0 then
      begin
        // Check for chars
        for j := 0 to Length(aChars) - 1 do
        begin
          if (ParamStr(i) = ('-' + aChars[j])) then
          begin
            Result := i;
            Exit;
          end;
        end;
      end;

      if Length(aStrings) > 0 then
      begin
        // Check for strings
        for k := 0 to Length(aStrings) - 1 do
        begin
          if (ParamStr(i) = ('--' + aStrings[k])) then
          begin
            Result := i;
            Exit;
          end;
        end;
      end;

    end;
  end;
end;

function TCommandApp.GetParamPos(aChar: Char; aString: AnsiString): Integer;
begin
  Result := GetParamPos([aChar], [aString]);
end;

function TCommandApp.GetParamPos(aChar: Char): Integer;
begin
  Result := GetParamPos([aChar], []);
end;

function TCommandApp.GetParamPos(aString: AnsiString): Integer;
begin
  Result := GetParamPos([], [aString]);
end;

procedure TCommandApp.ShowHelp(cbResult: AnsiString);
var
 i: Integer;
begin
  Print('\nUsage: %s [options]\n', [Self.Name]);

  Print('Options:\n');
  for i := 0 to fCommandStrList.Count - 1 do
  begin
    Print('  ' + fCommandStrList.Strings[i]);
  end;
  Print('\n');
end;

procedure TCommandApp.ShowVersion(cbResult: AnsiString);
begin
  Print('\n%s\nVersion: %s\n', [Self.Name, Self.Version]);
end;

procedure TCommandApp.Print(aString: AnsiString);
begin
  Print(aString, []);
end;

procedure TCommandApp.Print(aString: AnsiString; Args: array of const; OutputOption: TPrintOutputOptions = [poConsole]);
var
 tmpString, formattedString: String;
begin
  if OutputOption = [] then OutputOption := [poConsole, poGrowl, poLibNotify];

  tmpString := Format(aString, Args);
  formattedString := StringReplace(tmpString, '\n', sLineBreak, [rfReplaceAll, rfIgnoreCase]);
  formattedString := StringReplace(formattedString, '\t', #9, [rfReplaceAll, rfIgnoreCase]);

  if poConsole in OutputOption then WriteLn(formattedString);
  if poGrowl in OutputOption then
  begin
    {$IFDEF DARWIN}
      if IsGrowlNotifyInstalled then fpSystem('growlnotify -m "' + Format(aString, Args) + '"');
    {$ENDIF}
  end;
  if poLibNotify in OutputOption then
  begin
    {$IFDEF DARWIN}
      if IsLibNotifyInstalled then fpSystem('notify-send "' + Self.Name + '" "' + Format(aString, Args) + '"');
    {$ENDIF}
  end;
end;

procedure TCommandApp.Option(aChar: Char; aDescription: AnsiString;
  aCallback: TCmdOptionCallback);
begin
  Option([aChar], [], aDescription, aCallback);
end;

procedure TCommandApp.Option(aChars: array of Char; aDescription: AnsiString;
  aCallback: TCmdOptionCallback);
begin
  Option(aChars, [], aDescription, aCallback);
end;

procedure TCommandApp.Option(aString, aDescription: AnsiString;
  aCallback: TCmdOptionCallback);
begin
  Option([], [aString], aDescription, aCallback);
end;

procedure TCommandApp.Option(aStrings: array of AnsiString; aDescription: AnsiString;
  aCallback: TCmdOptionCallback);
begin
  Option([], aStrings, aDescription, aCallback);
end;

procedure TCommandApp.Option(aChar: Char; aString, aDescription: AnsiString;
  aCallback: TCmdOptionCallback);
begin
  Option([aChar], [aString], aDescription, aCallback);
end;

procedure TCommandApp.Option(aChars: array of Char; aStrings: array of AnsiString;
  aDescription: AnsiString; aCallback: TCmdOptionCallback);
begin
  AddParamsToList(aChars, aStrings, aDescription);

  if (HasParam(aChars, aStrings)) then aCallback('');
end;

procedure TCommandApp.OptionWithParameter(aChar: Char; aDescription,
  aParameterName: AnsiString; aCallback: TCmdOptionCallback);
begin
  OptionWithParameter([aChar], [], aDescription, aParameterName, aCallback);
end;

procedure TCommandApp.OptionWithParameter(aChars: array of Char; aDescription,
  aParameterName: AnsiString; aCallback: TCmdOptionCallback);
begin
  OptionWithParameter(aChars, [], aDescription, aParameterName, aCallback);
end;

procedure TCommandApp.OptionWithParameter(aString, aDescription,
  aParameterName: AnsiString; aCallback: TCmdOptionCallback);
begin
  OptionWithParameter([], [aString], aDescription, aParameterName, aCallback);
end;

procedure TCommandApp.OptionWithParameter(aStrings: array of AnsiString;
  aDescription, aParameterName: AnsiString; aCallback: TCmdOptionCallback);
begin
  OptionWithParameter([], aStrings, aDescription, aParameterName, aCallback);
end;

procedure TCommandApp.OptionWithParameter(aChar: Char; aString, aDescription,
  aParameterName: AnsiString; aCallback: TCmdOptionCallback);
begin
  OptionWithParameter([aChar], [aString], aDescription, aParameterName, aCallback);
end;

procedure TCommandApp.OptionWithParameter(aChars: array of Char;
  aStrings: array of AnsiString; aDescription, aParameterName: AnsiString;
  aCallback: TCmdOptionCallback);
var
 paramPos: Integer;
begin
  AddParamsToList(aChars, aStrings, aDescription, aParameterName);

  paramPos := GetParamPos(aChars, aStrings);

  if (paramPos <> -1) then aCallback(ParamStr(paramPos + 1));
end;

procedure TCommandApp.Choose(aChooseText: AnsiString; aList: array of AnsiString;
  aDefaultValue: Integer; aCallback: TCmdChooseCallback);
var
  i, choiceInt: Integer;
  choice: ShortString;
begin
  WriteLn(aChooseText);
  for i := 0 to Length(aList) - 1 do
  begin
    if aDefaultValue = i then WriteLn('[' + IntToStr(i + 1) + ']) ' + aList[i])
    else WriteLn(' ' + IntToStr(i + 1) + ' ) ' + aList[i]);
  end;
  ReadLn(choice);

  if (choice = '') then choice := IntToStr(aDefaultValue + 1);

  if TryStrToInt(choice, choiceInt) then
  begin
    choiceInt := choiceInt - 1;
    if choiceInt > Length(aList) - 1 then choiceInt := Length(aList) - 1;
    aCallback(choiceInt)
  end else WriteLn(rsInputNotNumber);
end;

procedure TCommandApp.Choose(aChooseText: AnsiString; aList: array of Char;
  aDefaultValue: Integer; aCallback: TCmdChooseCallback);
var
  i: Integer;
  tmpList: array of AnsiString;
begin
  SetLength(tmpList, Length(aList));
  for i := 0 to Length(aList) - 1 do
    tmpList[i] := aList[i];

  Choose(aChooseText, tmpList, aDefaultValue, aCallback);
end;

procedure TCommandApp.Choose(aChooseText: AnsiString; aList: array of Single;
  aDefaultValue: Integer; aCallback: TCmdChooseCallback);
var
  i: Integer;
  tmpList: array of AnsiString;
begin
  SetLength(tmpList, Length(aList));
  for i := 0 to Length(aList) - 1 do
    tmpList[i] := FloatToStr(aList[i]);

  Choose(aChooseText, tmpList, aDefaultValue, aCallback);
end;

procedure TCommandApp.Choose(aChooseText: AnsiString; aList: array of Integer;
  aDefaultValue: Integer; aCallback: TCmdChooseCallback);
var
  i: Integer;
  tmpList: array of AnsiString;
begin
  SetLength(tmpList, Length(aList));
  for i := 0 to Length(aList) - 1 do
    tmpList[i] := IntToStr(aList[i]);

  Choose(aChooseText, tmpList, aDefaultValue, aCallback);
end;

procedure TCommandApp.Prompt(aPromptText: AnsiString; aCallback: TCmdPromptCallback
  );
begin
  Prompt(aPromptText, '', aCallback);
end;

procedure TCommandApp.Prompt(aPromptText, aDefaultText: AnsiString;
  aCallback: TCmdPromptCallback);
var
  promptChoice: String;
begin
  WriteLn('');
  if (aDefaultText = '') then Write(aPromptText + ' ')
  else Write(aPromptText + ' [' + aDefaultText + '] ');
  Read(promptChoice);
  WriteLn('');

  aCallback(promptChoice);
end;

(*procedure TCommandApp.Password(aPasswordText: AnsiString; aMaskChar: Char;
  aCallback: TCmdPasswordCallback);
begin

end;*)

procedure TCommandApp.Confirm(aConfirmText: AnsiString;
  aCallback: TCmdConfirmCallback);
begin
  Confirm(aConfirmText, true, aCallback);
end;

procedure TCommandApp.Confirm(aConfirmText: AnsiString; aDefaultValue: Boolean;
  aCallback: TCmdConfirmCallback);
var
  confirmChoice, tmpString: String;
begin
  WriteLn('');

  if aDefaultValue then Write(aConfirmText + ' ([Y]es / [N]o) [Yes] ')
  else Write(aConfirmText + ' ([Y]es / [N]o) [No] ');

  Read(confirmChoice);

  tmpString := LowerCase(confirmChoice);

  if aDefaultValue then tmpString := 'y'
  else tmpString := 'n';

  if ((tmpString = 'y') or (tmpString = 'yes') or (tmpString = '1') or (tmpString = 'true')) then aCallback(true)
  else aCallback(false);
end;

procedure TCommandApp.Execute;
begin
  Option('v', 'version', 'Shows the version number', @Self.ShowVersion);
  Option('h', 'help', 'Shows this help screen.', @Self.ShowHelp);

  if ParamCount = 0 then ShowHelp('');
end;

end.
