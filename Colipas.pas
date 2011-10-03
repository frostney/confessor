(*
  Colipas
  =======

  Command-line library for FreePascal/Delphi


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
  TCmdOptionCallback = procedure() of object;
  TCmdChooseCallback = procedure(cbResult: Integer) of object;
  TCmdPromptCallback = procedure(cbResult: String) of object;
  TCmdPasswordCallback = procedure(cbResult: String) of object;
  TCmdConfirmCallback = procedure(cbResult: Boolean) of object;

  TPrintOutputOption = (poConsole, poGrowl, poLibNotify);
  TPrintOutputOptions = set of TPrintOutputOption;

  { TCommandApp }

  TCommandApp = class
  private
    fCommandStrList: TStringList; //< Stores all available options

    function IsGrowlNotifyInstalled(): Boolean; //< Checks if Growl is installed (Mac OS X only)
    function IsLibNotifyInstalled(): Boolean; //< Check is libNotify is installed (Linux only)
  protected
    fName, fVersion: String;
  public
    constructor Create();
    destructor Destroy; Override;

    procedure Print(aString: String; OutputOption: TPrintOutputOptions = [poConsole]); Overload;
    procedure Print(aString: String; Args: array of const; OutputOption: TPrintOutputOptions); Overload;

    procedure Option(aChar: Char; aDescription: String; aCallback: TCmdOptionCallback); Overload;
    procedure Option(aChars: array of Char; aDescription: String; aCallback: TCmdOptionCallback); Overload;
    procedure Option(aString, aDescription: String; aCallback: TCmdOptionCallback); Overload;
    procedure Option(aStrings: array of String; aDescription: String; aCallback: TCmdOptionCallback);
    procedure Option(aChar: Char; aString, aDescription: String; aCallback: TCmdOptionCallback); Overload;
    procedure Option(aChars: array of Char; aStrings: array of String; aDescription: String; aCallback: TCmdOptionCallback);

    procedure Choose(aChooseText: String; aList: array of String; aCallback: TCmdChooseCallback); Overload;
    procedure Choose(aChooseText: String; aList: array of Char; aCallback: TCmdChooseCallback); Overload;
    procedure Choose(aChooseText: String; aList: array of Single; aCallback: TCmdChooseCallback); Overload;
    procedure Choose(aChooseText: String; aList: array of Integer; aCallback: TCmdChooseCallback); Overload;

    procedure Prompt(aPromptText: String; aCallback: TCmdPromptCallback); Overload;
    procedure Prompt(aPromptText, aDefaultText: String; aCallback: TCmdPromptCallback); Overload;

    procedure Password(aPasswordText: String; aMaskChar: Char; aCallback: TCmdPasswordCallback);

    procedure Confirm(aConfirmText: String; aCallback: TCmdConfirmCallback); Overload;
    procedure Confirm(aConfirmText, aDefaultText: String; aCallback: TCmdConfirmCallback); Overload;

    procedure Execute(); virtual;
  published
    property Name: String read fName write fName;
    property Version: String read fVersion write fVersion;
  end;

  // https://gist.github.com/1208591
  function CmdToString(Command: AnsiString): TStringList;

// Messages
resourcestring
  rsInputNotNumber = 'Error: Input is not a number';
  rsInputNotString = 'Error: Input is not a string';

implementation

// https://gist.github.com/1208591
function CmdToString(Command: AnsiString): TStringList;
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
  fpSystem(Command + ' > ' + FileName);

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
var
 tmpStringList: TStringList;
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

procedure TCommandApp.Print(aString: String; OutputOption: TPrintOutputOptions = [poConsole]);
begin
  Print(aString, [], OutputOption);
end;

procedure TCommandApp.Print(aString: String; Args: array of const; OutputOption: TPrintOutputOptions);
begin
  if OutputOption = [] then OutputOption := [poConsole, poGrowl, poLibNotify];

  if poConsole in OutputOption then WriteLn(Format(aString, Args));
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

procedure TCommandApp.Option(aChar: Char; aDescription: String;
  aCallback: TCmdOptionCallback);
begin

end;

procedure TCommandApp.Option(aChars: array of Char; aDescription: String;
  aCallback: TCmdOptionCallback);
begin

end;

procedure TCommandApp.Option(aString, aDescription: String;
  aCallback: TCmdOptionCallback);
begin

end;

procedure TCommandApp.Option(aStrings: array of String; aDescription: String;
  aCallback: TCmdOptionCallback);
begin

end;

procedure TCommandApp.Option(aChar: Char; aString, aDescription: String;
  aCallback: TCmdOptionCallback);
begin

end;

procedure TCommandApp.Option(aChars: array of Char; aStrings: array of String;
  aDescription: String; aCallback: TCmdOptionCallback);
begin

end;

procedure TCommandApp.Choose(aChooseText: String; aList: array of String;
  aCallback: TCmdChooseCallback);
var
  i, choiceInt: Integer;
  choice: ShortString;
begin
  WriteLn(aChooseText);
  for i := 0 to Length(aList) - 1 do
  begin
    WriteLn(IntToStr(i) + ') ' + aList[i]);
  end;
  ReadLn(choice);

  if TryStrToInt(choice, choiceInt) then aCallback(choiceInt)
  else WriteLn(rsInputNotNumber);
end;

procedure TCommandApp.Choose(aChooseText: String; aList: array of Char;
  aCallback: TCmdChooseCallback);
var
  i, choice: Integer;
begin
  WriteLn(aChooseText);
  for i := 0 to Length(aList) - 1 do
  begin
    WriteLn(IntToStr(i) + ') ' + aList[i]);
  end;
  ReadLn(choice);
  aCallback(choice);
end;

procedure TCommandApp.Choose(aChooseText: String; aList: array of Single;
  aCallback: TCmdChooseCallback);
var
  i, choice: Integer;
begin
  WriteLn(aChooseText);
  for i := 0 to Length(aList) - 1 do
  begin
    WriteLn(IntToStr(i) + ') ' + FloatToStr(aList[i]));
  end;
  ReadLn(choice);
  aCallback(choice);
end;

procedure TCommandApp.Choose(aChooseText: String; aList: array of Integer;
  aCallback: TCmdChooseCallback);
var
  i, choice: Integer;
begin
  WriteLn(aChooseText);
  for i := 0 to Length(aList) - 1 do
  begin
    WriteLn(IntToStr(i) + ') ' + IntToStr(aList[i]));
  end;
  ReadLn(choice);
  aCallback(choice);
end;

procedure TCommandApp.Prompt(aPromptText: String; aCallback: TCmdPromptCallback
  );
begin

end;

procedure TCommandApp.Password(aPasswordText: String; aMaskChar: Char;
  aCallback: TCmdPasswordCallback);
begin

end;

procedure TCommandApp.Confirm(aConfirmText: String;
  aCallback: TCmdConfirmCallback);
begin

end;

procedure TCommandApp.Execute;
begin

end;

end.
