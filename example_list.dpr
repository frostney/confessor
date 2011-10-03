program example_list;

{$IFDEF FPC}
  {$MODE OBJFPC}
{$ENDIF}

uses
  Colipas,
  SysUtils;

type
  TMyApp = class(TCommandApp)
  private
    procedure ChosenCallback(cbResult: Integer);
  public
    WeaponArray: array of String;

    procedure Execute(); Override;
  end;

var
  MyApp: TMyApp;

procedure TMyApp.ChosenCallback(cbResult: Integer);
begin
  //WriteLn('You chose: ' + WeaponArray[cbResult]);
  WriteLn('You chose: ' + IntToStr(cbResult));
end;

procedure TMyApp.Execute;
begin
  inherited;
end;

begin
  MyApp := TMyApp.Create;
  //MyApp.WeaponArray := ['Toaster', 'Spoon', 'Leftovers'];

  WriteLn('ayn');
  MyApp.Choose('Your weapon of choice:', ['Toaster', 'Spoon', 'Leftovers'], @MyApp.ChosenCallback);

  MyApp.Print('Something', [poGrowl]);

  MyApp.Destroy;
end.
