program example_list;

{$IFDEF FPC}
  {$MODE OBJFPC}
{$ENDIF}

uses
  Colipas,
  SysUtils;

type
  // StringArray type = dynamic array of String
  TStringArray = array of AnsiString;

  // Sub-classed TCommandApp
  TMyApp = class(TCommandApp)
  private
    // Chose callback
    procedure ChosenCallback(cbResult: Integer);
  public
    WeaponArray: TStringArray;

    procedure Execute(); Override;
  end;

// Instance of TMyApp
var
  MyApp: TMyApp;

// Populate array helper function
function PopulateArray(Values: array of AnsiString): TStringArray;
var
  tmpStringArray: TStringArray;
  i: Integer;
begin
  SetLength(tmpStringArray, Length(Values));

  for i := 0 to Length(Values) - 1 do
    tmpStringArray[i] := Values[i];

  Result := tmpStringArray;
end;

// Callback for our chose function
procedure TMyApp.ChosenCallback(cbResult: Integer);
begin
  WriteLn('You chose: ' + WeaponArray[cbResult]);
end;

// We don't need to call Execute in this example
procedure TMyApp.Execute;
begin
  inherited;
end;

begin
  // Create instance
  MyApp := TMyApp.Create;

  // Populate array
  MyApp.WeaponArray := PopulateArray(['Toaster', 'Spoon', 'Leftovers']);
  WriteLn(Length(MyApp.WeaponArray));

  // Let the user select an option
  MyApp.Choose('Your weapon of choice:', ['Toaster', 'Spoon', 'Leftovers'], 0, @MyApp.ChosenCallback);

  // Destroy instance
  MyApp.Destroy;
end.
