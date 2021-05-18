unit Wieze_Hanoi_Forma;{04.Kwi.2021}

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Wieze_Hanoi;

type
  TWieze_Hanoi_Forma_Form = class( TForm )
    procedure FormCreate( Sender: TObject );
    procedure FormShow( Sender: TObject );
    procedure FormClose( Sender: TObject; var Action: TCloseAction );
  private
    { Private declarations }
    wieze_hanoi : TWieze_Hanoi;
  public
    { Public declarations }
  end;

var
  Wieze_Hanoi_Forma_Form: TWieze_Hanoi_Forma_Form;

implementation

{$R *.dfm}

//FormCreate().
procedure TWieze_Hanoi_Forma_Form.FormCreate( Sender: TObject );
begin

  // W delphi zmiana forma_glowna.Position ci¹gle wywo³uje forma_glowna.FormShow().
  // Utworzenie TWieze_Hanoi w FormCreate() nieprawid³owo okreœla rozmieszczenie komponentów na panelu (wysokoœæ).

  //wieze_hanoi := TWieze_Hanoi.Create( Self );

  Self.Position := poScreenCenter;

end;//---//FormCreate().

//FormShow().
procedure TWieze_Hanoi_Forma_Form.FormShow( Sender: TObject );
begin

  // W delphi zmiana forma_glowna.Position ci¹gle wywo³uje forma_glowna.FormShow().

  wieze_hanoi := TWieze_Hanoi.Create( Self );

end;//---//FormShow().

//FormClose().
procedure TWieze_Hanoi_Forma_Form.FormClose( Sender: TObject; var Action: TCloseAction );
begin

  FreeAndNil( wieze_hanoi );

end;//---//FormClose().

end.
