program Wieze_Hanoi_pr;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  { you can add units after this }
  Wieze_Hanoi_Forma;

{$R *.res}

begin

  RequireDerivedFormResource := True;
  Application.Title := 'Wieże Hanoi';
  Application.Scaled := True;
  Application.Initialize();
  Application.CreateForm( TWieze_Hanoi_Forma_Form, Wieze_Hanoi_Forma_Form );
  Application.Run();

end.

