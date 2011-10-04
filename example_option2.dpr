program example_option2;

{$IFDEF FPC}
  {$MODE OBJFPC}
{$ENDIF}

uses
  Colipas,
  SysUtils;

type
  // Sub-classed TCommandApp
  TMyApp = class(TCommandApp)
  private
    // Chose callback
    procedure OptionCallback(cbResult: AnsiString);
  public
    procedure Execute(); Override;
  end;

// Instance of TMyApp
var
  MyApp: TMyApp;

// Callback for our chose function
procedure TMyApp.OptionCallback(cbResult: AnsiString);
begin
  if cbResult = '' then Print('Please enter a parameter.')
  else Print('You entered ' + cbResult + ' as a parameter.');
end;

procedure TMyApp.Execute;
begin
  inherited;
end;

begin
  // Create instance
  MyApp := TMyApp.Create;
  MyApp.Name := 'example_option2';

  MyApp.OptionWithParameter('p', 'Call this app with a parameter.', 'SomeString', @MyApp.OptionCallback);

  MyApp.Execute();

  // Destroy instance
  MyApp.Destroy;
end.
