unit Wieze_Hanoi_Forma;{04.Kwi.2021}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  Wieze_Hanoi;

type

  { TWieze_Hanoi_Forma_Form }

  TWieze_Hanoi_Forma_Form = class( TForm )
    procedure FormCreate( Sender: TObject );
    procedure FormShow( Sender: TObject );
    procedure FormClose( Sender: TObject; var CloseAction: TCloseAction );
  private
    wieze_hanoi : TWieze_Hanoi;
  public

  end;

var
  Wieze_Hanoi_Forma_Form: TWieze_Hanoi_Forma_Form;

implementation

{$R *.lfm}

//FormCreate().
procedure TWieze_Hanoi_Forma_Form.FormCreate( Sender: TObject );
begin

  // W delphi zmiana forma_glowna.Position ciągle wywołuje forma_glowna.FormShow().
  // Utworzenie TWieze_Hanoi w FormCreate() nieprawidłowo określa rozmieszczenie komponentów na panelu (wysokość).

  //wieze_hanoi := TWieze_Hanoi.Create( Self );
  wieze_hanoi := nil;

end;//---//FormCreate().

//FormShow().
procedure TWieze_Hanoi_Forma_Form.FormShow( Sender: TObject );
begin

  // W delphi zmiana forma_glowna.Position ciągle wywołuje forma_glowna.FormShow().

  if wieze_hanoi = nil then
    wieze_hanoi := TWieze_Hanoi.Create( Self );

end;//---//FormShow().

//FormClose().
procedure TWieze_Hanoi_Forma_Form.FormClose( Sender: TObject; var CloseAction: TCloseAction );
begin

  FreeAndNil( wieze_hanoi );

end;//---//FormClose().

end.

