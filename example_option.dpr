program example_option;

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
    procedure OptionACallback(cbResult: AnsiString);
  public
    procedure Execute(); Override;
  end;

// Instance of TMyApp
var
  MyApp: TMyApp;

// Callback for our chose function
procedure TMyApp.OptionACallback(cbResult: AnsiString);
begin
  WriteLn('You chose Option A.');
end;

procedure TMyApp.Execute;
begin
  inherited;
end;

begin
  // Create instance
  MyApp := TMyApp.Create;
  MyApp.Name := 'example_option';

  MyApp.Option('a', 'This is called Option A.', @MyApp.OptionACallback);

  MyApp.Execute();

  // Destroy instance
  MyApp.Destroy;
end.
