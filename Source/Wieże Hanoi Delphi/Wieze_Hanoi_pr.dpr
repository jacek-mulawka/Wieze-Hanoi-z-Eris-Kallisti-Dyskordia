program Wieze_Hanoi_pr;

uses
  Vcl.Forms,
  Wieze_Hanoi_Forma in 'Wieze_Hanoi_Forma.pas' {Wieze_Hanoi_Forma_Form},
  Wieze_Hanoi in '..\Wieze_Hanoi.pas';

{$R *.res}

begin

  ReportMemoryLeaksOnShutdown := DebugHook <> 0;

  Application.Initialize();
  Application.MainFormOnTaskbar := True;
  Application.HintHidePause := 30000;
  Application.CreateForm( TWieze_Hanoi_Forma_Form, Wieze_Hanoi_Forma_Form );
  Application.Run();

end.
