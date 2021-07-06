unit Wieze_Hanoi;{04.Kwi.2021}

  //
  // MIT License
  //
  // Copyright (c) 2021 Jacek Mulawka
  //
  // j.mulawka@interia.pl
  //
  // https://github.com/jacek-mulawka
  //

  //   +y         +z
  // +x  -x     /
  //   -y    -z

  // Platformy (tag)
  // 1 2 3
  // L Ś P

  // Tag poziomu wieży oznacza, na którym jest podeście (L, P, S).
  // TagFloat poziomu wieży oznacza wielkość poziomu wieży (najmniejszy, na górze równa się 1; największy na dole równa się tyle ile jest poziomów).

{$I Definicje.inc}

interface

uses
  Forms, SysUtils, ExtCtrls, StdCtrls, Spin, Buttons, Classes, Graphics, Math, DateUtils, ComCtrls,
  Dialogs, Windows, Controls,
  GLScene, GLCadencer, GLColor, GLMaterial, GLVectorGeometry, GLBehaviours, GLTexture, GLSkyBox, GLWindowsFont, GLHUDObjects, GLCrossPlatform, GLFireFX,

  {$IFDEF delphi}
  GLWin32Viewer,
  {$ELSE delphi}
  GLLCLViewer,
  {$ENDIF}

  GLObjects, GLGeomObjects;

type
  TWieza_Poziom_Animacja_Ruchu_Etap = ( wpare_Brak, wpare_Unoszenie, wpare_Przesuwanie, wpare_Opadanie );
  TPlatforma_Strona = ( ps_Brak, ps_Lewa, ps_Srodkowa, ps_Prawa );

  TWieza_Poziom_Animacja_Ruchu_r = record
    id_wieza_poziom_wybrany : integer;
    docelowe_x,
    docelowe_y,
    unoszenie_do_y
      : single;
    animacja_trwa : boolean;
    wieza_poziom_animacja_ruchu_etap : TWieza_Poziom_Animacja_Ruchu_Etap;
  end;//---//TWieza_Poziom_Animacja_Ruchu_r

  TDemo_Krok = class
  private
    { Private declarations }
    platforma_strona__z,
    platforma_strona__do
      : word;
  public
    { Public declarations }
    constructor Create( const platforma_strona__z_f, platforma_strona__do_f : TPlatforma_Strona );
  end;//---//TDemo_Krok

  TPlatek = class( GLObjects.TGLDummyCube )
  private
    { Private declarations }
    opadanie_do_y : single; // Do jakiej wartości współrzędnej Y płatek opada.
    strony_kontener_gl_dummy_cube : GLObjects.TGLDummyCube;
    strona_1_gl_disk,
    strona_2_gl_disk
     : GLGeomObjects.TGLDisk;
  public
    { Public declarations }
    constructor Create( AOwner : TComponent );
    destructor Destroy(); override;
  end;//---//TPlatek

  { TWieze_Hanoi }

  TWieze_Hanoi = class
  private
    czy_demo_g,
    czy_demo_wikipedia_g,
    czy_demo_wykonano_ruch_g // Czy dla zadanych platform udało się przenieść poziom wierzy w danym ruchu.
      : boolean;

    demo__wieza_poziomy_ilosc_g,
    wieza_poziomy_ilosc_g // Ilość poziomów wieży ustawionych dla rozpoczętej rozgrywki.
      : integer;

    ruchy_ilosc_g,
    ruchy_ilosc__minimalna_potrzebna_g // Minimalna potrzebna ilość ruchów do przeniesienia wszystkich poziomów wieży na inna platformę.
      : Int64;

    platek__ostatnio_utworzony_czas_g,
    podmuch__kolejny_sekundy_g, // Co ile sekund przesuwać płatki.
    podmuch__ostatni_czas_g
      : double;

    demo_ostatni_krok_czas_g, // Kiedy został wykonany ostatni krok algorytmu dema.
    {$IFDEF lazarus}
    ostatnie_odswiezenie_okna_czas_g, // W lazarusie panel o programie się nie odświeża.
    {$ENDIF}
    fajerwerki_ostatnie_wywolanie_czas_g,
    wieza_poziom_numer_ostatnie_wywolanie_czas_g
      : TDateTime;

    demo__platforma_strona_g,
    demo__wikipedia__wieza_poziom_najmniejszy__przenoszenie_kierunek_g, // W którą stronę według algorytmu należy przenosić najmniejszy poziom wieży.
    demo__wikipedia__wieza_poziom_najmniejszy__platforma_strona_g, // Na której platformie znajduje się aktualnie poziom najmniejszy wieży.
    platforma_strona_docelowa_zwyciestwo_g, // Jeżeli na tej platformie znajdą się wszystkie poziomy wieży zadanie zostanie ukończone.
    platforma_strona_wybrana_g // Tag strony platformy.
      : TPlatforma_Strona;

    wieza_poziom_animacja_ruchu_r : TWieza_Poziom_Animacja_Ruchu_r;

    forma_glowna : TForm;
    forma_glowna_form_resize_oryginalne : procedure( Sender : TObject ) of object;

    O_Programie_Panel : ExtCtrls.TPanel;

    O_Programie_Splitter : ExtCtrls.TSplitter;

    Kamera_Wspolrzedne_Label,
    O_Programie_Label,
    Ruchy_Ilosc_Label,
    Wieza_Poziomy_Ilosc_Etykieta_Label
      : StdCtrls.TLabel;

    Logo_Image : ExtCtrls.TImage;

    Demo_Oczekiwanie_Milisekund_SpinEdit,
    Wieza_Poziomy_Ilosc_SpinEdit
      : Spin.TSpinEdit;

    Demo_BitBtn,
    Nowa_Gra_BitBtn,
    Pelny_Ekran_BitBtn,
    Pomoc_BitBtn,
    Tlo_Nastepne_BitBtn,
    Tlo_Poprzednie_BitBtn,
    Zamknij_BitBtn
      : Buttons.TBitBtn;
    Wieza_Strona_Poczatkowa_RadioGroup : ExtCtrls.TRadioGroup;

    Demo_Wikipedia_CheckBox,
    Animacja_CheckBox,
    Platki_CheckBox
      : StdCtrls.TCheckBox;

    Demo_ProgressBar : ComCtrls.TProgressBar;

    //Wieza_Poziom_Numer_Timer : ExtCtrls.TTimer; // W lazarusie się zacina.

    Gra_GLScene : TGLScene;
    Gra_GLSceneViewer : TGLSceneViewer;
    Gra_GLCamera : TGLCamera;
    Gra_GLLightSource : TGLLightSource;
    Gra_GLCadencer : TGLCadencer;

    Zero_GLSphere : GLObjects.TGLSphere;

    Platforma_Lewa_GLCube,
    Platforma_Prawa_GLCube,
    Platforma_Srodkowa_GLCube
      : GLObjects.TGLCube;

    Fajerwerki_Gl_Dummy_Cube : GLObjects.TGLDummyCube;

    Wieza_Poziom_Numer_GLWindowsBitmapFont : GLWindowsFont.TGLWindowsBitmapFont;

    Wieza_Poziom_Numer_GLHUDSprite : GLHUDObjects.TGLHUDSprite;
    Wieza_Poziom_Numer_GLHUDText : GLHUDObjects.TGLHUDText;

    GLMaterialLibrary1 : GLMaterial.TGLMaterialLibrary;
    Tlo_GLSkyBox : GLSkyBox.TGLSkyBox;

    demo_kroki_list,
    platki_list,
    wieza_elementy_list
      : Classes.TList;

    GLFireFXManager1 : GLFireFX.TGLFireFXManager;

    procedure Form_Resize( Sender : TObject );

    procedure Nowa_Gra_BitBtnClick( Sender : TObject );
    procedure Pelny_Ekran_BitBtnClick( Sender : TObject );
    procedure Pomoc_BitBtnClick( Sender : TObject );
    procedure Demo_BitBtnClick( Sender : TObject );
    procedure Tlo_BitBtnClick( Sender : TObject );
    procedure Wieza_Poziomy_Ilosc_SpinEditKeyDown( Sender : TObject; var Key : Word; Shift : TShiftState );
    procedure Wieza_Elementy__Zwolnij_Wszystkie();

    procedure Wieza_Element__Animacja_Ruchu( delta_czasu_f : double );

    procedure Ruchy_Ilosc_Etykieta_Wartosc_Wpisz();

    procedure Platforma_Zaznaczenie_Ustaw( const platforma_strona_f : TPlatforma_Strona );

    procedure Platki__Dodaj_Jeden();
    procedure Platki__Zwolnij_Jeden( pocisk_f : TPlatek  );
    procedure Platki__Zwolnij_Wszystkie();
    procedure Platki__Ruch( delta_czasu_f : double );

    procedure Kamera_Wspolrzedne_Wypisz();

    procedure Fajerwerki_Animuj();
    procedure Demo__Animuj();
    procedure Demo__Etap_Przelicz();

    procedure Demo__Kroki__Zwolnij_Wszystkie();

    procedure Zwyciestwo_Sprawdz();

    procedure Gra_GLCadencerProgress( Sender : TObject; const deltaTime, newTime : Double );

    procedure Gra_GLSceneViewerMouseDown( Sender : TObject; Button : TMouseButton; Shift : TShiftState; X, Y : Integer );
    procedure Gra_GLSceneViewerKeyDown( Sender : TObject; var Key : Word; Shift : TShiftState );
    procedure Gra_GLSceneViewerDblClick( Sender : TObject );
    procedure Gra_GLSceneViewerMouseMove( Sender : TObject; Shift : TShiftState; X, Y : Integer );

    procedure Wieza_Poziom_Numer_TimerTimer( Sender : TObject );
  public
    constructor Create( forma_f : TForm );
    destructor Destroy(); override; // Bez override; się nie wywołuje Destroy().
  end;//---//TWieze_Hanoi

const
  platek_dodanie_sekundy_c : double = 0.2; // Co ile sekund dodać kolejny płatek.
  platek_opadanie_szybkosc_c : double = 0.1;

implementation

//Konstruktor klasy TDemo_Krok.
constructor TDemo_Krok.Create( const platforma_strona__z_f, platforma_strona__do_f : TPlatforma_Strona );
begin

  Self.platforma_strona__z := Ord(  IntToStr( integer(platforma_strona__z_f) )[ 1 ]  );
  Self.platforma_strona__do := Ord(  IntToStr( integer(platforma_strona__do_f) )[ 1 ]  );

end;//---//Konstruktor klasy TDemo_Krok.

//Konstruktor klasy TPlatek.
constructor TPlatek.Create( AOwner : TComponent );
begin

  inherited Create( AOwner );

  Self.Pickable := false;


  Self.strony_kontener_gl_dummy_cube := GLObjects.TGLDummyCube.Create( AOwner );
  Self.strony_kontener_gl_dummy_cube.Parent := Self;
  Self.strony_kontener_gl_dummy_cube.Pickable := false;

  Self.strona_1_gl_disk := GLGeomObjects.TGLDisk.Create( Self.Owner );
  Self.strona_1_gl_disk.Parent := Self.strony_kontener_gl_dummy_cube;
  Self.strona_1_gl_disk.Pickable := false;
  Self.strona_1_gl_disk.TurnAngle := 180;
  Self.strona_1_gl_disk.StartAngle := 150;
  Self.strona_1_gl_disk.SweepAngle := 60;
  Self.strona_1_gl_disk.OuterRadius := 0.05 - 0.03 * Random( 100 ) * 0.01;
  Self.strona_1_gl_disk.Position.Y := Self.strona_1_gl_disk.OuterRadius * 0.5;

  Self.strona_1_gl_disk.Material.BlendingMode := GLMaterial.bmCustom;

  Self.strona_1_gl_disk.Material.FrontProperties.Ambient.Color := GLColor.clrWhite;
  Self.strona_1_gl_disk.Material.FrontProperties.Diffuse.Color := GLColor.clrLightPurple;
  Self.strona_1_gl_disk.Material.FrontProperties.Diffuse.Alpha := 0.3;
  Self.strona_1_gl_disk.Material.FrontProperties.Emission.Color := GLColor.clrTransparent;


  Self.strona_2_gl_disk := GLGeomObjects.TGLDisk.Create( Self.Owner );
  Self.strona_2_gl_disk.Parent := Self.strony_kontener_gl_dummy_cube;
  Self.strona_2_gl_disk.Pickable := false;

  Self.strona_2_gl_disk.StartAngle := Self.strona_1_gl_disk.StartAngle;
  Self.strona_2_gl_disk.SweepAngle := Self.strona_1_gl_disk.SweepAngle;
  Self.strona_2_gl_disk.OuterRadius := Self.strona_1_gl_disk.OuterRadius;
  Self.strona_2_gl_disk.Position.Y := Self.strona_1_gl_disk.Position.Y;

  Self.strona_2_gl_disk.Material.BlendingMode := Self.strona_1_gl_disk.Material.BlendingMode;

  Self.strona_2_gl_disk.Material.FrontProperties.Ambient.Color := Self.strona_1_gl_disk.Material.FrontProperties.Ambient.Color;
  Self.strona_2_gl_disk.Material.FrontProperties.Diffuse.Color := Self.strona_1_gl_disk.Material.FrontProperties.Diffuse.Color;
  Self.strona_2_gl_disk.Material.FrontProperties.Diffuse.Alpha := Self.strona_1_gl_disk.Material.FrontProperties.Diffuse.Alpha;
  Self.strona_2_gl_disk.Material.FrontProperties.Emission.Color := Self.strona_1_gl_disk.Material.FrontProperties.Emission.Color;


  GetOrCreateInertia( Self.Behaviours ).Mass := 1;
  GetOrCreateInertia( Self.strony_kontener_gl_dummy_cube.Behaviours ).Mass := 1;

  GLBehaviours.GetOrCreateInertia( Self.Behaviours ).TranslationDamping.SetDamping( 0.05, 0.0, 0.0 );
  GLBehaviours.GetOrCreateInertia( Self.strony_kontener_gl_dummy_cube.Behaviours ).RotationDamping.SetDamping( 1, 1.3, 0 );

end;//---//Konstruktor klasy TPlatek.

//Destruktor klasy TPlatek.
destructor TPlatek.Destroy();
begin

  FreeAndNil( Self.strona_1_gl_disk );
  FreeAndNil( Self.strona_2_gl_disk );

  FreeAndNil( strony_kontener_gl_dummy_cube );

  inherited;

end;//---//Destruktor klasy TPlatek.

//Konstruktor klasy TWieze_Hanoi.
constructor TWieze_Hanoi.Create( forma_f : TForm );
var
  zts : string;
begin

  if   ( forma_f = nil )
    or (  not Assigned( forma_f )  ) then
    begin

      raise Exception.Create( 'Należy wskazać prawidłowy obiekt TForm. [Mann muss auf ein korrektes TForm Objekt zeigen.]' ); // uses SysUtils.

    end;
  //---//if   ( forma_f = nil ) (...)

  forma_glowna := forma_f;


  {$IFDEF delphi}
  if @forma_glowna.OnResize <> nil then
  {$ELSE delphi}
  if Assigned( forma_glowna.OnResize ) then
  {$ENDIF}
    forma_glowna_form_resize_oryginalne := forma_glowna.OnResize
  else//if @forma_glowna.OnResize <> nil then
    forma_glowna_form_resize_oryginalne := nil;


  {$IFDEF delphi}
  forma_glowna.OnResize := Form_Resize;
  {$ELSE delphi}
  forma_glowna.OnResize := @Form_Resize;
  {$ENDIF}


  forma_glowna.Height := 500;
  forma_glowna.Width := 900;

  {$IFNDEF delphi}
  forma_glowna.Position := poScreenCenter;
  {$ENDIF}

  forma_glowna.Caption := 'Wieże Hanoi z Eris Kallisti Dyskordia';

  czy_demo_g := false;
  fajerwerki_ostatnie_wywolanie_czas_g := Now();
  podmuch__kolejny_sekundy_g := 3;
  wieza_poziom_numer_ostatnie_wywolanie_czas_g := Now();
  wieza_poziomy_ilosc_g := 3;

  Randomize();

  demo_kroki_list := TList.Create();
  platki_list := TList.Create();
  wieza_elementy_list := TList.Create();


  O_Programie_Panel := ExtCtrls.TPanel.Create( forma_glowna );
  O_Programie_Panel.Parent := forma_glowna;
  O_Programie_Panel.Align := Controls.alRight;
  O_Programie_Panel.Width := 300;

  O_Programie_Splitter := ExtCtrls.TSplitter.Create( forma_glowna );
  O_Programie_Splitter.Parent := forma_glowna;
  O_Programie_Splitter.Align := Controls.alRight;
  O_Programie_Splitter.Width := 5;
  {$IFDEF delphi}
  O_Programie_Splitter.OnMoved := Form_Resize;
  {$ELSE delphi}
  O_Programie_Splitter.OnMoved := @Form_Resize;
  {$ENDIF}


  O_Programie_Label := StdCtrls.TLabel.Create( forma_glowna );
  O_Programie_Label.Parent := O_Programie_Panel;
  O_Programie_Label.Align := Controls.alTop;
  O_Programie_Label.AutoSize := true; // W lazarusie ładniej i lepiej dostosowuje wielkość etykiety tak aby cały napis było widać.
  O_Programie_Label.WordWrap := true;
  O_Programie_Label.Caption :=
    'Wieże Hanoi z Eris Kallisti Dyskordia' + #13 +
    'wersja 2021.07.05.' +
    {$IFDEF delphi}
    ' [delphi]' +
    {$ENDIF}
    {$IFDEF lazarus}
    ' [lazarus]' +
    {$ENDIF}
    #13 +
    #13 +
    'MIT License' + #13 +
    #13 +
    'Copyright (c) 2021 Jacek Mulawka' + #13 +
    #13 +
    'j.mulawka@interia.pl' + #13 +
    #13 +
    'https://github.com/jacek-mulawka' + #13 +
    #13 +
    #13 +
    'Program jest darmowy, udostępniony w postaci takiej jakiej jest wraz ze wszystkimi błędami. Użytkownik zyskuje dostęp do programu, używa programu na własne ' +
    'ryzyko i ponosi wyłączną odpowiedzialność za wszelkie szkody (własne i niewłasne) materialne i niematerialne oraz utratę danych w związku z używaniem programu.';


  zts := 'Wieze_Hanoi.ico';

  if not FileExists( zts ) then
    zts := '..\Wieze_Hanoi.ico';

  if not FileExists( zts ) then
    zts := '';

  if zts = '' then
    Logo_Image := nil
  else//if zts = '' then
    begin

      Logo_Image := ExtCtrls.TImage.Create( forma_glowna );
      Logo_Image.Parent := O_Programie_Panel;
      Logo_Image.Picture.LoadFromFile( zts );
      Logo_Image.AutoSize := true;
      Logo_Image.Top := 0;
      Logo_Image.Left := Logo_Image.Parent.Width - Logo_Image.Width;
      Logo_Image.Anchors := [ Controls.akRight, Controls.akTop ];

    end;
  //---//if zts = '' then



  Ruchy_Ilosc_Label := StdCtrls.TLabel.Create( forma_glowna );
  Ruchy_Ilosc_Label.Parent := O_Programie_Panel;
  Ruchy_Ilosc_Label.AutoSize := true;
  Ruchy_Ilosc_Label.ShowHint := true;
  Ruchy_Ilosc_Label.Caption :=
    'Ilość ruchów: <?>.';
  Ruchy_Ilosc_Label.Hint :=
    'Ilość ruchów / minimalna potrzebna ilość ruchów do wykonania zadania.' + #13 +
    '[Die Anzahl der Bewegungen / die Mindestanzahl von Zügen, die zur Ausführung der Aufgabe erforderlich sind.]';
  Ruchy_Ilosc_Label.Left := 10;
  Ruchy_Ilosc_Label.Top := O_Programie_Label.Height + 50;

  Wieza_Poziomy_Ilosc_Etykieta_Label := StdCtrls.TLabel.Create( forma_glowna );
  Wieza_Poziomy_Ilosc_Etykieta_Label.Parent := O_Programie_Panel;
  Wieza_Poziomy_Ilosc_Etykieta_Label.AutoSize := true;
  Wieza_Poziomy_Ilosc_Etykieta_Label.WordWrap := false;
  Wieza_Poziomy_Ilosc_Etykieta_Label.ShowHint := true;
  Wieza_Poziomy_Ilosc_Etykieta_Label.Caption :=
    'Ilość poziomów wieży';
  Wieza_Poziomy_Ilosc_Etykieta_Label.Hint :=
    'Ilość poziomów wieży.' + #13 +
    'Enter - nowa gra.' + #13 +
    '[Türme von Hanoi Niveau.' + #13 +
    'Eingabetaste - neues Spiel.]';
  Wieza_Poziomy_Ilosc_Etykieta_Label.Left := 10;
  Wieza_Poziomy_Ilosc_Etykieta_Label.Top := Ruchy_Ilosc_Label.Top + Ruchy_Ilosc_Label.Height + 10;

  Wieza_Poziomy_Ilosc_SpinEdit := Spin.TSpinEdit.Create( forma_glowna );
  Wieza_Poziomy_Ilosc_SpinEdit.Parent := O_Programie_Panel;
  Wieza_Poziomy_Ilosc_SpinEdit.MinValue := 1;
  Wieza_Poziomy_Ilosc_SpinEdit.MaxValue := 999999999;
  Wieza_Poziomy_Ilosc_SpinEdit.Value := wieza_poziomy_ilosc_g;
  Wieza_Poziomy_Ilosc_SpinEdit.ShowHint := true;
  Wieza_Poziomy_Ilosc_SpinEdit.Hint := Wieza_Poziomy_Ilosc_Etykieta_Label.Hint;
  Wieza_Poziomy_Ilosc_SpinEdit.Left := 10;
  Wieza_Poziomy_Ilosc_SpinEdit.Top := Wieza_Poziomy_Ilosc_Etykieta_Label.Top + Wieza_Poziomy_Ilosc_Etykieta_Label.Height + 10;
  Wieza_Poziomy_Ilosc_SpinEdit.Width := Wieza_Poziomy_Ilosc_Etykieta_Label.Width;
  {$IFDEF delphi}
  Wieza_Poziomy_Ilosc_SpinEdit.OnKeyDown := Wieza_Poziomy_Ilosc_SpinEditKeyDown;
  {$ELSE delphi}
  Wieza_Poziomy_Ilosc_SpinEdit.OnKeyDown := @Wieza_Poziomy_Ilosc_SpinEditKeyDown;
  {$ENDIF}


  Wieza_Strona_Poczatkowa_RadioGroup := ExtCtrls.TRadioGroup.Create( forma_glowna );
  Wieza_Strona_Poczatkowa_RadioGroup.Parent := O_Programie_Panel;
  Wieza_Strona_Poczatkowa_RadioGroup.ShowHint := true;
  Wieza_Strona_Poczatkowa_RadioGroup.Caption :=
    'Strona początkowa';
  Wieza_Strona_Poczatkowa_RadioGroup.Hint :=
    'Po której stronie ustawić początkowo wieżę' + #13 +
    '[Auf welcher Seite den Turm zunächst platzieren.]';
  Wieza_Strona_Poczatkowa_RadioGroup.Left := Wieza_Poziomy_Ilosc_SpinEdit.Left + Wieza_Poziomy_Ilosc_SpinEdit.Width + 20;
  Wieza_Strona_Poczatkowa_RadioGroup.Top := Wieza_Poziomy_Ilosc_Etykieta_Label.Top;
  Wieza_Strona_Poczatkowa_RadioGroup.Columns := 1;
  Wieza_Strona_Poczatkowa_RadioGroup.Height := 90;
  Wieza_Strona_Poczatkowa_RadioGroup.Width := 150;
  Wieza_Strona_Poczatkowa_RadioGroup.Items.Add( 'lewa [links]' );
  Wieza_Strona_Poczatkowa_RadioGroup.Items.Add( 'prawa [rechts]' );
  Wieza_Strona_Poczatkowa_RadioGroup.ItemIndex := 0;


  Nowa_Gra_BitBtn := Buttons.TBitBtn.Create( forma_glowna );
  Nowa_Gra_BitBtn.Parent := O_Programie_Panel;
  Nowa_Gra_BitBtn.Kind := bkIgnore;
  Nowa_Gra_BitBtn.Caption := 'N';
  Nowa_Gra_BitBtn.ModalResult := mrNone;
  Nowa_Gra_BitBtn.ShowHint := true;
  Nowa_Gra_BitBtn.Hint :=
    'Nowa gra.' + #13 +
    '[Neues Spiel.]';
  Nowa_Gra_BitBtn.Left := 10;
  Nowa_Gra_BitBtn.Top := Wieza_Poziomy_Ilosc_SpinEdit.Top + Wieza_Poziomy_Ilosc_SpinEdit.Height + 10;
  {$IFDEF delphi}
  Nowa_Gra_BitBtn.OnClick := Nowa_Gra_BitBtnClick;
  {$ELSE delphi}
  Nowa_Gra_BitBtn.OnClick := @Nowa_Gra_BitBtnClick;
  {$ENDIF}


  Pomoc_BitBtn := Buttons.TBitBtn.Create( forma_glowna );
  Pomoc_BitBtn.Parent := O_Programie_Panel;
  Pomoc_BitBtn.Kind := bkHelp;
  Pomoc_BitBtn.Caption := '';
  Pomoc_BitBtn.ModalResult := mrNone;
  Pomoc_BitBtn.ShowHint := true;
  Pomoc_BitBtn.Hint :=
    'Pomoc.' + #13 +
    '[Hilfe.]';
  Pomoc_BitBtn.Left := 10;
  Pomoc_BitBtn.Top := Nowa_Gra_BitBtn.Top + Nowa_Gra_BitBtn.Height + 20;
  {$IFDEF delphi}
  Pomoc_BitBtn.OnClick := Pomoc_BitBtnClick;
  {$ELSE delphi}
  Pomoc_BitBtn.OnClick := @Pomoc_BitBtnClick;
  {$ENDIF}


  Animacja_CheckBox := StdCtrls.TCheckBox.Create( Application );
  Animacja_CheckBox.Parent := O_Programie_Panel;
  Animacja_CheckBox.Checked := true;
  Animacja_CheckBox.Caption := 'Animacja';
  Animacja_CheckBox.ShowHint := true;
  Animacja_CheckBox.Hint :=
    'Animacja przemieszczania poziomów wieży.' + #13 +
    '[Türme Niveau Bewegungsanimation.]';
  Animacja_CheckBox.Left := Pomoc_BitBtn.Left + Pomoc_BitBtn.Width + 30;
  Animacja_CheckBox.Top := Pomoc_BitBtn.Top;
  Animacja_CheckBox.Width := 60;


  Platki_CheckBox := StdCtrls.TCheckBox.Create( Application );
  Platki_CheckBox.Parent := O_Programie_Panel;
  Platki_CheckBox.Checked := true;
  Platki_CheckBox.Caption := 'Płatki';
  Platki_CheckBox.ShowHint := true;
  Platki_CheckBox.Hint :=
    'Opadające płatki.' + #13 +
    '[Fallende Blütenblätter.]';
  Platki_CheckBox.Left := Animacja_CheckBox.Left + Animacja_CheckBox.Width + 20;
  Platki_CheckBox.Top := Pomoc_BitBtn.Top;
  Platki_CheckBox.Width := 50;


  Pelny_Ekran_BitBtn := Buttons.TBitBtn.Create( forma_glowna );
  Pelny_Ekran_BitBtn.Parent := O_Programie_Panel;
  Pelny_Ekran_BitBtn.Kind := bkCustom;
  Pelny_Ekran_BitBtn.Caption := '[E]';
  Pelny_Ekran_BitBtn.ModalResult := mrNone;
  Pelny_Ekran_BitBtn.ShowHint := true;
  Pelny_Ekran_BitBtn.Hint :=
    'Pełny ekran.' + #13 +
    '[Vollbild.]';
  Pelny_Ekran_BitBtn.Left := 10;
  Pelny_Ekran_BitBtn.Top := Pomoc_BitBtn.Top + Pomoc_BitBtn.Height + 40;
  {$IFDEF delphi}
  Pelny_Ekran_BitBtn.OnClick := Pelny_Ekran_BitBtnClick;
  {$ELSE delphi}
  Pelny_Ekran_BitBtn.OnClick := @Pelny_Ekran_BitBtnClick;
  {$ENDIF}


  Zamknij_BitBtn := Buttons.TBitBtn.Create( forma_glowna );
  Zamknij_BitBtn.Parent := O_Programie_Panel;
  Zamknij_BitBtn.Kind := bkClose;
  Zamknij_BitBtn.Caption := 'X';
  //Zamknij_BitBtn.ModalResult := mrClose;
  Zamknij_BitBtn.ShowHint := true;
  Zamknij_BitBtn.Hint :=
    'Zamknij.' + #13 +
    '[Schließe.]';
  Zamknij_BitBtn.Left := Pelny_Ekran_BitBtn.Left + Pelny_Ekran_BitBtn.Width + 40;
  Zamknij_BitBtn.Top := Pelny_Ekran_BitBtn.Top;


  Demo_BitBtn := Buttons.TBitBtn.Create( forma_glowna );
  Demo_BitBtn.Parent := O_Programie_Panel;
  Demo_BitBtn.Kind := bkRetry;
  Demo_BitBtn.Caption := '';
  Demo_BitBtn.ModalResult := mrNone;
  Demo_BitBtn.ShowHint := true;
  Demo_BitBtn.Hint :=
    'Demo.' + #13 +
    '[Demo.]';
  Demo_BitBtn.Left := 10;
  Demo_BitBtn.Top := Pelny_Ekran_BitBtn.Top + Pelny_Ekran_BitBtn.Height + 40;
  {$IFDEF delphi}
  Demo_BitBtn.OnClick := Demo_BitBtnClick;
  {$ELSE delphi}
  Demo_BitBtn.OnClick := @Demo_BitBtnClick;
  {$ENDIF}


  Demo_Oczekiwanie_Milisekund_SpinEdit := Spin.TSpinEdit.Create( forma_glowna );
  Demo_Oczekiwanie_Milisekund_SpinEdit.Parent := O_Programie_Panel;
  Demo_Oczekiwanie_Milisekund_SpinEdit.MinValue := 0;
  Demo_Oczekiwanie_Milisekund_SpinEdit.MaxValue := 999999999;
  Demo_Oczekiwanie_Milisekund_SpinEdit.Value := 1000;
  Demo_Oczekiwanie_Milisekund_SpinEdit.ShowHint := true;
  Demo_Oczekiwanie_Milisekund_SpinEdit.Hint :=
    'Oczekiwanie na kolejny krok dema w milisekundach.' + #13 +
    '[Warten auf den nächsten Demoschritt in Millisekunden.]';
  Demo_Oczekiwanie_Milisekund_SpinEdit.Left := Demo_BitBtn.Left + Demo_BitBtn.Width + 20;
  Demo_Oczekiwanie_Milisekund_SpinEdit.Top := Demo_BitBtn.Top;
  Demo_Oczekiwanie_Milisekund_SpinEdit.Width := 70;
  Demo_Oczekiwanie_Milisekund_SpinEdit.Increment := 100;


  Demo_Wikipedia_CheckBox := StdCtrls.TCheckBox.Create( Application );
  Demo_Wikipedia_CheckBox.Parent := O_Programie_Panel;
  Demo_Wikipedia_CheckBox.Checked := true;
  Demo_Wikipedia_CheckBox.Caption := 'Wikipedia';
  Demo_Wikipedia_CheckBox.ShowHint := true;
  Demo_Wikipedia_CheckBox.Hint :=
    'Algorytm dema z Wikipedii.' + #13 +
    '[Der Demoalgorithmus von Wikipedia.]';
  Demo_Wikipedia_CheckBox.Left := Demo_Oczekiwanie_Milisekund_SpinEdit.Left + Demo_Oczekiwanie_Milisekund_SpinEdit.Width + 20;
  Demo_Wikipedia_CheckBox.Top := Demo_BitBtn.Top;
  Demo_Wikipedia_CheckBox.Width := 100;
  //Demo_Wikipedia_CheckBox.Enabled := false; //???


  Demo_ProgressBar := ComCtrls.TProgressBar.Create( forma_glowna );
  Demo_ProgressBar.Parent := O_Programie_Panel;
  Demo_ProgressBar.Left := 10;
  Demo_ProgressBar.Top := Demo_BitBtn.Top + Demo_BitBtn.Height + 10;
  Demo_ProgressBar.Width := Demo_ProgressBar.Parent.Width - 20;
  Demo_ProgressBar.Anchors := Demo_ProgressBar.Anchors + [ Controls.akRight ];
  Demo_ProgressBar.Step := 1;


  Tlo_Poprzednie_BitBtn := Buttons.TBitBtn.Create( forma_glowna );
  Tlo_Poprzednie_BitBtn.Parent := O_Programie_Panel;
  Tlo_Poprzednie_BitBtn.Kind := bkCustom;
  Tlo_Poprzednie_BitBtn.Caption := '<';
  Tlo_Poprzednie_BitBtn.ModalResult := mrNone;
  Tlo_Poprzednie_BitBtn.Default := false;
  Tlo_Poprzednie_BitBtn.ShowHint := true;
  Tlo_Poprzednie_BitBtn.Hint :=
    'Poprzednie tło.' + #13 +
    '[Vorheriger Hintergrund.]';
  Tlo_Poprzednie_BitBtn.Left := 10;
  Tlo_Poprzednie_BitBtn.Top := Demo_ProgressBar.Top + Demo_ProgressBar.Height + 20;
  {$IFDEF delphi}
  Tlo_Poprzednie_BitBtn.OnClick := Tlo_BitBtnClick;
  {$ELSE delphi}
  Tlo_Poprzednie_BitBtn.OnClick := @Tlo_BitBtnClick;
  {$ENDIF}

  Tlo_Nastepne_BitBtn := Buttons.TBitBtn.Create( forma_glowna );
  Tlo_Nastepne_BitBtn.Parent := O_Programie_Panel;
  Tlo_Nastepne_BitBtn.Kind := bkCustom;
  Tlo_Nastepne_BitBtn.Caption := '>';
  Tlo_Nastepne_BitBtn.ModalResult := mrNone;
  Tlo_Nastepne_BitBtn.Default := false;
  Tlo_Nastepne_BitBtn.ShowHint := true;
  Tlo_Nastepne_BitBtn.Hint :=
    'Następne tło.' + #13 +
    '[Nächster Hintergrund.]';
  Tlo_Nastepne_BitBtn.Left := Tlo_Nastepne_BitBtn.Left + Tlo_Nastepne_BitBtn.Width + 20;
  Tlo_Nastepne_BitBtn.Top := Demo_ProgressBar.Top + Demo_ProgressBar.Height + 20;
  {$IFDEF delphi}
  Tlo_Nastepne_BitBtn.OnClick := Tlo_BitBtnClick;
  {$ELSE delphi}
  Tlo_Nastepne_BitBtn.OnClick := @Tlo_BitBtnClick;
  {$ENDIF}


  Kamera_Wspolrzedne_Label := StdCtrls.TLabel.Create( forma_glowna );
  Kamera_Wspolrzedne_Label.Parent := O_Programie_Panel;
  Kamera_Wspolrzedne_Label.AutoSize := true;
  Kamera_Wspolrzedne_Label.WordWrap := false;
  Kamera_Wspolrzedne_Label.ShowHint := true;
  Kamera_Wspolrzedne_Label.Caption :=
    'Kamera <?>.';
  Kamera_Wspolrzedne_Label.Hint :=
    'Kamera - współrzędne, skala.' + #13 +
    '[Kamera - Koordinate, Maßstab.]';
  Kamera_Wspolrzedne_Label.Left := 10;
  Kamera_Wspolrzedne_Label.Top := Tlo_Nastepne_BitBtn.Top + Tlo_Nastepne_BitBtn.Height + 10;


  //Wieza_Poziom_Numer_Timer := ExtCtrls.TTimer.Create( Application );
  //Wieza_Poziom_Numer_Timer.Enabled := false;
  //Wieza_Poziom_Numer_Timer.Interval := 500;
  {$IFDEF delphi}
  //Wieza_Poziom_Numer_Timer.OnTimer := Wieza_Poziom_Numer_TimerTimer;
  {$ELSE delphi}
  //Wieza_Poziom_Numer_Timer.OnTimer := @Wieza_Poziom_Numer_TimerTimer;
  {$ENDIF}

  Gra_GLScene := TGLScene.Create( forma_glowna );

  Gra_GLCamera := TGLCamera.Create( Gra_GLScene );
  Gra_GLCamera.Parent := Gra_GLScene.Objects;
  Gra_GLCamera.Direction.Z := 1;
  Gra_GLCamera.Position.Z := -10;
  Gra_GLCamera.CameraStyle := csOrthogonal;
  //Gra_GLCamera.DepthOfView := 100;

  Gra_GLLightSource := TGLLightSource.Create( Gra_GLScene ); // Zawsze zgłasza jakiś wyciek pamięci w delphi 10.2.
  Gra_GLLightSource.Parent := Gra_GLScene.Objects;
  Gra_GLLightSource.Position.Z := -100;

  Gra_GLSceneViewer := TGLSceneViewer.Create( forma_glowna );
  Gra_GLSceneViewer.Parent := forma_glowna;
  Gra_GLSceneViewer.Align := Controls.alClient;
  Gra_GLSceneViewer.Camera := Gra_GLCamera;
  Gra_GLSceneViewer.Buffer.BackgroundColor := Graphics.clSkyBlue;
  {$IFDEF delphi}
  Gra_GLSceneViewer.OnKeyDown := Gra_GLSceneViewerKeyDown;
  Gra_GLSceneViewer.OnMouseDown := Gra_GLSceneViewerMouseDown;
  Gra_GLSceneViewer.OnDblClick := Gra_GLSceneViewerDblClick;
  //Gra_GLSceneViewer.OnMouseMove := Gra_GLSceneViewerMouseMove; // Wywoływane w ten sposób za bardzo spowalnia działanie programu.
  {$ELSE delphi}
  Gra_GLSceneViewer.OnKeyDown := @Gra_GLSceneViewerKeyDown;
  Gra_GLSceneViewer.OnMouseDown := @Gra_GLSceneViewerMouseDown;
  Gra_GLSceneViewer.OnDblClick := @Gra_GLSceneViewerDblClick;
  //Gra_GLSceneViewer.OnMouseMove := @Gra_GLSceneViewerMouseMove; // Wywoływane w ten sposób za bardzo spowalnia działanie programu.
  {$ENDIF}

  Platforma_Lewa_GLCube := GLObjects.TGLCube.Create( Gra_GLScene );
  Platforma_Lewa_GLCube.Parent := Gra_GLScene.Objects;
  Platforma_Lewa_GLCube.Tag := integer(ps_Lewa);
  Platforma_Lewa_GLCube.Material.FrontProperties.Ambient.Color := GLColor.clrBrown;
  Platforma_Lewa_GLCube.Material.FrontProperties.Diffuse.Color := GLColor.clrBronze;
  Platforma_Lewa_GLCube.Material.FrontProperties.Emission.Color := GLColor.clrTransparent;

  //Platforma_Lewa_GLCube.CubeHeight := 1.0;

  Platforma_Prawa_GLCube := GLObjects.TGLCube.Create( Gra_GLScene );
  Platforma_Prawa_GLCube.Parent := Gra_GLScene.Objects;
  Platforma_Prawa_GLCube.Tag := integer(ps_Prawa);
  Platforma_Prawa_GLCube.Material.FrontProperties.Ambient.Color := Platforma_Lewa_GLCube.Material.FrontProperties.Ambient.Color;
  Platforma_Prawa_GLCube.Material.FrontProperties.Diffuse.Color := Platforma_Lewa_GLCube.Material.FrontProperties.Diffuse.Color;
  Platforma_Prawa_GLCube.Material.FrontProperties.Emission.Color := Platforma_Lewa_GLCube.Material.FrontProperties.Emission.Color;
  //Platforma_Prawa_GLCube.CubeHeight := 1.0;

  Platforma_Srodkowa_GLCube := GLObjects.TGLCube.Create( Gra_GLScene );
  Platforma_Srodkowa_GLCube.Parent := Gra_GLScene.Objects;
  Platforma_Srodkowa_GLCube.Tag := integer(ps_Srodkowa);
  Platforma_Srodkowa_GLCube.Material.FrontProperties.Ambient.Color := Platforma_Lewa_GLCube.Material.FrontProperties.Ambient.Color;
  Platforma_Srodkowa_GLCube.Material.FrontProperties.Diffuse.Color := Platforma_Lewa_GLCube.Material.FrontProperties.Diffuse.Color;
  Platforma_Srodkowa_GLCube.Material.FrontProperties.Emission.Color := Platforma_Lewa_GLCube.Material.FrontProperties.Emission.Color;
  //Platforma_Srodkowa_GLCube.CubeHeight := 1.0;

  Wieza_Poziom_Numer_GLWindowsBitmapFont := GLWindowsFont.TGLWindowsBitmapFont.Create( Application );
  Wieza_Poziom_Numer_GLWindowsBitmapFont.Font.Name := 'Georgia';
  Wieza_Poziom_Numer_GLWindowsBitmapFont.Font.Size := 20;

  Wieza_Poziom_Numer_GLHUDSprite := GLHUDObjects.TGLHUDSprite.Create( Application );
  Wieza_Poziom_Numer_GLHUDSprite.Parent := Gra_GLScene.Objects;
  Wieza_Poziom_Numer_GLHUDSprite.Pickable := false;
  Wieza_Poziom_Numer_GLHUDSprite.Height := 40;
  Wieza_Poziom_Numer_GLHUDSprite.Width := 80;
  Wieza_Poziom_Numer_GLHUDSprite.Position.X := 5 + Wieza_Poziom_Numer_GLHUDSprite.Width * 0.5;
  Wieza_Poziom_Numer_GLHUDSprite.Position.Y := 5 + Wieza_Poziom_Numer_GLHUDSprite.Height * 0.5;
  Wieza_Poziom_Numer_GLHUDSprite.Material.BlendingMode := GLMaterial.bmCustom;
  //Wieza_Poziom_Numer_GLHUDSprite.Material.FrontProperties.Ambient.Color := GLColor.clrTransparent;
  //Wieza_Poziom_Numer_GLHUDSprite.Material.FrontProperties.Diffuse.Color := GLColor.clrPink;
  //Wieza_Poziom_Numer_GLHUDSprite.Material.FrontProperties.Diffuse.Alpha := 0.85;
  //Wieza_Poziom_Numer_GLHUDSprite.Material.FrontProperties.Emission.Color := GLColor.clrTransparent;
  Wieza_Poziom_Numer_GLHUDSprite.Material.FrontProperties.Ambient.Color := GLColor.clrLightPurple;
  Wieza_Poziom_Numer_GLHUDSprite.Material.FrontProperties.Ambient.Alpha := 0.5;
  Wieza_Poziom_Numer_GLHUDSprite.Material.FrontProperties.Diffuse.Color := Wieza_Poziom_Numer_GLHUDSprite.Material.FrontProperties.Ambient.Color;
  Wieza_Poziom_Numer_GLHUDSprite.Material.FrontProperties.Diffuse.Alpha := Wieza_Poziom_Numer_GLHUDSprite.Material.FrontProperties.Ambient.Alpha;
  Wieza_Poziom_Numer_GLHUDSprite.Material.FrontProperties.Emission.Color := Wieza_Poziom_Numer_GLHUDSprite.Material.FrontProperties.Ambient.Color;
  Wieza_Poziom_Numer_GLHUDSprite.Material.FrontProperties.Emission.Alpha := Wieza_Poziom_Numer_GLHUDSprite.Material.FrontProperties.Ambient.Alpha;
  Wieza_Poziom_Numer_GLHUDSprite.Visible := false;

  Wieza_Poziom_Numer_GLHUDText := GLHUDObjects.TGLHUDText.Create( Application );
  Wieza_Poziom_Numer_GLHUDText.Parent := Gra_GLScene.Objects;
  Wieza_Poziom_Numer_GLHUDText.Pickable := false;
  Wieza_Poziom_Numer_GLHUDText.BitmapFont := Wieza_Poziom_Numer_GLWindowsBitmapFont;
  Wieza_Poziom_Numer_GLHUDText.Position.X := 10;
  Wieza_Poziom_Numer_GLHUDText.Position.Y := 10;
  //Wieza_Poziom_Numer_GLHUDText.Text := '<?>';
  Wieza_Poziom_Numer_GLHUDText.Text := '';
  //Wieza_Poziom_Numer_GLHUDText.Visible := false;


  Zero_GLSphere := GLObjects.TGLSphere.Create( Gra_GLScene );
  Zero_GLSphere.Parent := Gra_GLScene.Objects;
  //Zero_GLSphere.Radius := 0.10;
  //Zero_GLSphere.Position.X := 0.6;
  Zero_GLSphere.Visible := false;



  Gra_GLCadencer := GLCadencer.TGLCadencer.Create( Application );
  Gra_GLCadencer.Scene := Gra_GLScene;
  platek__ostatnio_utworzony_czas_g := Gra_GLCadencer.CurrentTime;
  podmuch__ostatni_czas_g := platek__ostatnio_utworzony_czas_g;
  {$IFDEF delphi}
  Gra_GLCadencer.OnProgress := Gra_GLCadencerProgress;
  {$ELSE delphi}
  Gra_GLCadencer.OnProgress := @Gra_GLCadencerProgress;
  {$ENDIF}


  GLFireFXManager1 := GLFireFX.TGLFireFXManager.Create( Application );
  GLFireFXManager1.Cadencer := Gra_GLCadencer;
  GLFireFXManager1.Disabled := true;
  GLFireFXManager1.FireRadius := 0.5;
  GLFireFXManager1.ParticleInterval := 0.3;
  GLFireFXManager1.ParticleSize := 0.1;
  GLFireFXManager1.InnerColor.RandomColor();
  GLFireFXManager1.OuterColor.RandomColor();

  Fajerwerki_Gl_Dummy_Cube := GLObjects.TGLDummyCube.Create( Application );
  Fajerwerki_Gl_Dummy_Cube.Parent := Gra_GLScene.Objects;
  //Fajerwerki_Gl_Dummy_Cube.VisibleAtRunTime := true;
  TGLBFireFX(Fajerwerki_Gl_Dummy_Cube.AddNewEffect(TGLBFireFX)).Manager := GLFireFXManager1;

  Tlo_GLSkyBox := nil;
  GLMaterialLibrary1 := nil;


  Tlo_BitBtnClick( nil );

  //Wieza_Poziom_Numer_Timer.Enabled := true;

  forma_glowna.WindowState := wsMaximized;

  Nowa_Gra_BitBtnClick( nil );
  Gra_GLSceneViewer.SetFocus();

  //Animacja_CheckBox.Checked := false; //???
  //Demo_Oczekiwanie_Milisekund_SpinEdit.Value := 500; //???

  {$IFDEF lazarus}
  Form_Resize( nil );
  {$ENDIF}

end;//---//Konstruktor klasy TWieze_Hanoi.

//Destruktor klasy TWieze_Hanoi.
destructor TWieze_Hanoi.Destroy();
begin

  //Wieza_Poziom_Numer_Timer.Enabled := false;

  {$IFDEF delphi}
  if @forma_glowna_form_resize_oryginalne <> nil then
  {$ELSE delphi}
  if forma_glowna_form_resize_oryginalne <> nil then
  //if Assigned( forma_glowna_form_resize_oryginalne ) then
  {$ENDIF}
    forma_glowna.OnResize := forma_glowna_form_resize_oryginalne;


  forma_glowna_form_resize_oryginalne := nil;


  Demo__Kroki__Zwolnij_Wszystkie();
  FreeAndNil( demo_kroki_list );

  Platki__Zwolnij_Wszystkie();
  FreeAndNil( platki_list );

  Wieza_Elementy__Zwolnij_Wszystkie();
  FreeAndNil( wieza_elementy_list );

  FreeAndNil( Platforma_Lewa_GLCube );
  FreeAndNil( Platforma_Prawa_GLCube );
  FreeAndNil( Platforma_Srodkowa_GLCube );

  FreeAndNil( Ruchy_Ilosc_Label );
  FreeAndNil( Wieza_Poziomy_Ilosc_Etykieta_Label );
  FreeAndNil( Wieza_Poziomy_Ilosc_SpinEdit );
  FreeAndNil( Wieza_Strona_Poczatkowa_RadioGroup );
  FreeAndNil( Nowa_Gra_BitBtn );
  FreeAndNil( Pomoc_BitBtn );
  FreeAndNil( Animacja_CheckBox );
  FreeAndNil( Platki_CheckBox );
  FreeAndNil( Pelny_Ekran_BitBtn );
  FreeAndNil( Zamknij_BitBtn );
  FreeAndNil( Demo_BitBtn );
  FreeAndNil( Demo_Oczekiwanie_Milisekund_SpinEdit );
  FreeAndNil( Demo_Wikipedia_CheckBox );
  FreeAndNil( Demo_ProgressBar );
  FreeAndNil( Tlo_Nastepne_BitBtn );
  FreeAndNil( Tlo_Poprzednie_BitBtn );
  FreeAndNil( Kamera_Wspolrzedne_Label );

  //FreeAndNil( Wieza_Poziom_Numer_Timer );

  if Logo_Image <> nil then
    FreeAndNil( Logo_Image );


  FreeAndNil( O_Programie_Label );

  FreeAndNil( O_Programie_Panel );

  FreeAndNil( O_Programie_Splitter );

  FreeAndNil( Wieza_Poziom_Numer_GLHUDSprite );
  FreeAndNil( Wieza_Poziom_Numer_GLHUDText );

  FreeAndNil( Fajerwerki_Gl_Dummy_Cube );

  FreeAndNil( Wieza_Poziom_Numer_GLWindowsBitmapFont );

  FreeAndNil( GLFireFXManager1 );

  FreeAndNil( Zero_GLSphere );

  FreeAndNil( Gra_GLCadencer );


  if Tlo_GLSkyBox <> nil then
    FreeAndNil( Tlo_GLSkyBox );


  if GLMaterialLibrary1 <> nil then
    begin

      GLMaterialLibrary1.Materials.Clear();
      FreeAndNil( GLMaterialLibrary1 );

    end;
  //---//if GLMaterialLibrary1 <> nil then


  FreeAndNil( Gra_GLCamera );
  FreeAndNil( Gra_GLLightSource );
  FreeAndNil( Gra_GLSceneViewer );
  FreeAndNil( Gra_GLScene );

end;//---//Destruktor klasy TWieze_Hanoi.

//Form_Resize().
procedure TWieze_Hanoi.Form_Resize( Sender : TObject );
var
  zt_min,
  zt_max
    : integer;
  ztsi_1,
  ztsi_2
    : single;
begin

  if    ( Gra_GLCamera <> nil )
    and ( Gra_GLSceneViewer <> nil ) then
    begin

      zt_min := Math.Min( Gra_GLSceneViewer.Width, Gra_GLSceneViewer.Height );
      zt_max := Math.Max( Gra_GLSceneViewer.Width, Gra_GLSceneViewer.Height );


      if wieza_poziomy_ilosc_g > 19 then
        Gra_GLCamera.Position.Y := Gra_GLCamera.TagFloat + 0.05 * ( wieza_poziomy_ilosc_g - 19 );


//      if wieza_poziomy_ilosc_g > 60 then
//        //Gra_GLCamera.SceneScale := -Math.Log10( wieza_poziomy_ilosc_g ) * 0.6 + 1.37
//        Gra_GLCamera.SceneScale := -Math.LogN( wieza_poziomy_ilosc_g * 0.45, wieza_poziomy_ilosc_g ) + 0.77 + Math.LogN( wieza_poziomy_ilosc_g * 0.45, 10 )
//      else
      if wieza_poziomy_ilosc_g > 10 then
        //Gra_GLCamera.SceneScale := -Math.Log10( wieza_poziomy_ilosc_g ) * 0.6 + 1.37
        //Gra_GLCamera.SceneScale := -Math.LogN( 30, wieza_poziomy_ilosc_g ) + 0.77 + Math.LogN( 30, 10 )
        ztsi_1 := -Math.LogN( 30, wieza_poziomy_ilosc_g ) + 1.45
      else//if wieza_poziomy_ilosc_g > 10 then
        ztsi_1 := 1;

      if zt_max <> 0 then
        ztsi_2 := zt_min / zt_max;

      if wieza_poziomy_ilosc_g > 10 then
        ztsi_2 := ztsi_2 * 0.6;

      if ztsi_2 < ztsi_1 then
        ztsi_1 := ztsi_2;


      if ztsi_1 < 0.02 then
        ztsi_1 := 0.02;


      Gra_GLCamera.SceneScale := ztsi_1;

//      if wieza_poziomy_ilosc_g > 220 then
//        Gra_GLCamera.SceneScale := 0.07 - ( wieza_poziomy_ilosc_g - 220 ) * 0.00025
//      else//if wieza_poziomy_ilosc_g > 31 then
//      if wieza_poziomy_ilosc_g > 110 then
//        Gra_GLCamera.SceneScale := 0.14 - ( wieza_poziomy_ilosc_g - 100 ) * 0.00075
//      else//if wieza_poziomy_ilosc_g > 31 then
//      if wieza_poziomy_ilosc_g > 70 then
//        Gra_GLCamera.SceneScale := 0.22 - ( wieza_poziomy_ilosc_g - 70 ) * 0.0022
//      else//if wieza_poziomy_ilosc_g > 31 then
//      if wieza_poziomy_ilosc_g > 50 then
//        Gra_GLCamera.SceneScale := 0.31 - ( wieza_poziomy_ilosc_g - 50 ) * 0.005
//      else//if wieza_poziomy_ilosc_g > 31 then
//      if wieza_poziomy_ilosc_g > 30 then
//        Gra_GLCamera.SceneScale := 0.49 - ( wieza_poziomy_ilosc_g - 30 ) * 0.0095
//      else//if wieza_poziomy_ilosc_g > 31 then
//      if wieza_poziomy_ilosc_g > 10 then
//        Gra_GLCamera.SceneScale := 0.77 - ( wieza_poziomy_ilosc_g - 11 ) * 0.015
//      else//if wieza_poziomy_ilosc_g > 10 then
//        Gra_GLCamera.SceneScale := 1;

//      if zt_max <> 0 then
//        Gra_GLCamera.SceneScale := zt_min / zt_max
//      else//if zt_max <> 0 then
//        Gra_GLCamera.SceneScale := 1;


//      if wieza_poziomy_ilosc_g > 27 then
//        begin
//
//          Gra_GLCamera.SceneScale := Gra_GLCamera.SceneScale - ( 0.8 / wieza_poziomy_ilosc_g ) * ( wieza_poziomy_ilosc_g - 19 );
//          Gra_GLCamera.SceneScale := Gra_GLCamera.SceneScale - ( 0.8 / wieza_poziomy_ilosc_g ) * ( wieza_poziomy_ilosc_g - 19 );
//
//        end
//      else//if wieza_poziomy_ilosc_g > 19 then
//      if wieza_poziomy_ilosc_g > 19 then
//        Gra_GLCamera.SceneScale := Gra_GLCamera.SceneScale - Gra_GLCamera.SceneScale * wieza_poziomy_ilosc_g * 0.02;

    end;
  //---//if    ( Gra_GLCamera <> nil ) (...)


  {$IFDEF delphi}
  if @forma_glowna_form_resize_oryginalne <> nil then
  {$ELSE delphi}
  if forma_glowna_form_resize_oryginalne <> nil then
  //if Assigned( forma_glowna_form_resize_oryginalne ) then
  {$ENDIF}
    forma_glowna_form_resize_oryginalne( Sender );


  Kamera_Wspolrzedne_Wypisz();


  {$IFDEF lazarus}
  // W lazarusie panel o programie się nie odświeża.

  //if O_Programie_Panel <> nil then
  //  O_Programie_Panel.Repaint();
  //
  //if O_Programie_Splitter <> nil then
  //  O_Programie_Splitter.Repaint();

  if forma_glowna <> nil then
    forma_glowna.Repaint();

  ostatnie_odswiezenie_okna_czas_g := Now();
  {$ENDIF}

end;//---//Form_Resize().

//Nowa_Gra_BitBtnClick().
procedure TWieze_Hanoi.Nowa_Gra_BitBtnClick( Sender : TObject );
const
  wieza_element_promien__wysokosci_c_l : single = 0.05; // Promień wysokości elementu wieży.
  wieza_element_promien__najwiekszy_c_l : single = 0.27; // Promień największego elementu wieży (dolnego).
var
  i : integer;
  ztsi : single;
  zt_gl_torus : TGLTorus;
begin

  wieza_poziom_animacja_ruchu_r.animacja_trwa := false;
  wieza_poziom_animacja_ruchu_r.wieza_poziom_animacja_ruchu_etap := wpare_Brak;

  if czy_demo_g then
    Demo_BitBtnClick( Sender );


  Demo_ProgressBar.Position := 0;

  if Wieza_Poziomy_Ilosc_SpinEdit.Value < Wieza_Poziomy_Ilosc_SpinEdit.MinValue then
    Wieza_Poziomy_Ilosc_SpinEdit.Value := Wieza_Poziomy_Ilosc_SpinEdit.MinValue;

  wieza_poziomy_ilosc_g := Wieza_Poziomy_Ilosc_SpinEdit.Value;

  ruchy_ilosc_g := 0;
  ruchy_ilosc__minimalna_potrzebna_g := Round(  Math.Power( 2, wieza_poziomy_ilosc_g )  ) - 1;
  Ruchy_Ilosc_Etykieta_Wartosc_Wpisz();

  GLFireFXManager1.Disabled := true;


  Wieza_Elementy__Zwolnij_Wszystkie();


  if Wieza_Strona_Poczatkowa_RadioGroup.ItemIndex = 0 then // Lewa.
    platforma_strona_docelowa_zwyciestwo_g := ps_Prawa
  else//if Wieza_Strona_Poczatkowa_RadioGroup.ItemIndex = 0 then
    platforma_strona_docelowa_zwyciestwo_g := ps_Lewa;


  platforma_strona_wybrana_g := ps_Brak;
  Platforma_Zaznaczenie_Ustaw( platforma_strona_wybrana_g );


  if   ( wieza_elementy_list = nil )
    or (  not Assigned( wieza_elementy_list )  ) then
    Exit;


  // Automatycznie dostosuje widok dla ilości elementów do 19 włącznie.
  if wieza_poziomy_ilosc_g <= 19 then
    Gra_GLCamera.Position.Y := 0.9 * wieza_poziomy_ilosc_g / 19
  else//if wieza_poziomy_ilosc_g <= 19 then
    Gra_GLCamera.Position.Y := 0.9;

  Gra_GLCamera.TagFloat := Gra_GLCamera.Position.Y;


  // Różnica promienia między największym a najmniejszym elementem wieży.
  ztsi := ( wieza_element_promien__najwiekszy_c_l - wieza_element_promien__wysokosci_c_l );

  if wieza_poziomy_ilosc_g > 10 then
    ztsi := ztsi + wieza_poziomy_ilosc_g * 0.01;


  for i := 1 to wieza_poziomy_ilosc_g do
    begin

      zt_gl_torus := GLGeomObjects.TGLTorus.Create( Gra_GLScene );
      zt_gl_torus.Parent := Gra_GLScene.Objects;
      zt_gl_torus.TagFloat := wieza_poziomy_ilosc_g - i + 1;
      zt_gl_torus.PitchAngle := 90;

      zt_gl_torus.MinorRadius := wieza_element_promien__wysokosci_c_l;
      zt_gl_torus.MajorRadius := ztsi * zt_gl_torus.TagFloat / wieza_poziomy_ilosc_g;


      zt_gl_torus.Position.X := 0.66;

      if wieza_poziomy_ilosc_g > 10 then
        zt_gl_torus.Position.X := zt_gl_torus.Position.X + wieza_poziomy_ilosc_g * 0.02; // 0.01 * 2.


      // Spód dolnego poziomu leży na wysokości 0.
      zt_gl_torus.Position.Y := zt_gl_torus.MinorRadius + zt_gl_torus.MinorRadius * ( i - 1 ) * 2;


      if platforma_strona_docelowa_zwyciestwo_g = ps_Lewa then
        begin

          // Prawa.

          zt_gl_torus.Position.X := -zt_gl_torus.Position.X;
          zt_gl_torus.Tag := integer(ps_Prawa);

        end
      else//if platforma_strona_docelowa_zwyciestwo_g = ps_Lewa then
        zt_gl_torus.Tag := integer(ps_Lewa);


      zt_gl_torus.Material.FrontProperties.Ambient.RandomColor();
      zt_gl_torus.Material.FrontProperties.Diffuse.RandomColor();
      zt_gl_torus.Material.FrontProperties.Emission.RandomColor();

      wieza_elementy_list.Add( zt_gl_torus );

    end;
  //---//for i := 1 to wieza_poziomy_ilosc_g do

  Platforma_Lewa_GLCube.CubeWidth := GLGeomObjects.TGLTorus(wieza_elementy_list[ 0 ]).MajorRadius * 2;
  Platforma_Prawa_GLCube.CubeWidth := Platforma_Lewa_GLCube.CubeWidth;
  Platforma_Srodkowa_GLCube.CubeWidth := Platforma_Lewa_GLCube.CubeWidth;

  //Platforma_Lewa_GLCube.CubeHeight := GLGeomObjects.TGLTorus(wieza_elementy_list[ 0 ]).MinorRadius * 2 * GLGeomObjects.TGLTorus(wieza_elementy_list[ 0 ]).Scale.Z * 2;
  Platforma_Lewa_GLCube.CubeHeight := GLGeomObjects.TGLTorus(wieza_elementy_list[ 0 ]).MinorRadius * 2;
  Platforma_Prawa_GLCube.CubeHeight := Platforma_Lewa_GLCube.CubeHeight;
  Platforma_Srodkowa_GLCube.CubeHeight := Platforma_Lewa_GLCube.CubeHeight;

  Platforma_Lewa_GLCube.CubeDepth := 0.6;
  Platforma_Prawa_GLCube.CubeDepth := Platforma_Lewa_GLCube.CubeDepth;
  Platforma_Srodkowa_GLCube.CubeDepth := Platforma_Lewa_GLCube.CubeDepth;


  Platforma_Lewa_GLCube.Position.X := Abs( GLGeomObjects.TGLTorus(wieza_elementy_list[ 0 ]).Position.X );
  Platforma_Prawa_GLCube.Position.X := -Platforma_Lewa_GLCube.Position.X;

  Platforma_Lewa_GLCube.Position.Y := -Platforma_Lewa_GLCube.CubeHeight * 0.5;
  //Platforma_Lewa_GLCube.Position.Y := -0.5;
  Platforma_Prawa_GLCube.Position.Y := Platforma_Lewa_GLCube.Position.Y;
  Platforma_Srodkowa_GLCube.Position.Y := Platforma_Lewa_GLCube.Position.Y;


  Wieza_Poziom_Numer_GLHUDSprite.MoveLast();
  Wieza_Poziom_Numer_GLHUDText.MoveLast();

  GLFireFXManager1.FireRadius := Platforma_Lewa_GLCube.CubeWidth * 0.5;

  Form_Resize( nil );

end;//---//Nowa_Gra_BitBtnClick().

//Pelny_Ekran_BitBtnClick().
procedure TWieze_Hanoi.Pelny_Ekran_BitBtnClick( Sender: TObject );
begin

  if forma_glowna = nil then
    Exit;


  if forma_glowna.BorderStyle <> bsNone then
    begin

      forma_glowna.BorderStyle := bsNone;

      {$IFDEF delphi}
      if forma_glowna.WindowState = wsMaximized then
        forma_glowna.WindowState := wsNormal; // Zmaksymalizowane okno czasami nie zasłania paska start.
      {$ENDIF}

      forma_glowna.WindowState := wsMaximized;

      forma_glowna.BringToFront();

    end
  else//if Self.WindowState = wsMaximized then
    begin

      forma_glowna.BorderStyle := bsSizeable;

    end;
  //---//if Self.WindowState = wsMaximized then

end;//---//Pelny_Ekran_BitBtnClick().

//Pomoc_BitBtnClick().
procedure TWieze_Hanoi.Pomoc_BitBtnClick( Sender : TObject );
begin

  ShowMessage
    (
      'Zaznaczenie platformy [Plattform auswählen]:' + #13 +
      '   1 - lewa [linke]' + #13 +
      '   2 - środkowa [mittlere],' + #13 +
      '   3 - prawa [rechte],' + #13 +
      '   Esc - brak [keine].' + #13 +
      #13 +
      '2 x LMP [LMB] - o programie [über das Programm].' + #13 +
      #13 +
      '   E - pełny ekran [Vollbild], ' + #13 +
      #13 +
      '   < - poprzednie tło [vorheriger Hintergrund], ' + #13 +
      '   > - następne tło [nächster Hintergrund],' + #13 +
      #13 +
      'Ruch kamery [Bewegung der Kamera]:' + #13 +
      '   (z Shift szybciej [mit Shift schneller])' + #13 +
      '   W - góra [oben],' + #13 +
      '   S - dół [unten],' + #13 +
      '   A - lewo [links],' + #13 +
      '   D - prawo [rechts],' + #13 +
      '   R - przybliżenie [näheren],' + #13 +
      '   F - oddalenie [entfernen].'
    );

end;//---//Pomoc_BitBtnClick().

//Demo_BitBtnClick().
procedure TWieze_Hanoi.Demo_BitBtnClick( Sender : TObject );
//var
  //zt_demo_krok : TDemo_Krok;
  //platforma_strona_poczatkowa : TPlatforma_Strona;
begin

  czy_demo_g := not czy_demo_g;

  if czy_demo_g then
    begin

      Demo_BitBtn.Kind := bkNo;

      //Demo_Kroki__Zwolnij_Wszystkie();

      demo_ostatni_krok_czas_g := Now();


      if not Demo_Wikipedia_CheckBox.Checked then // Mój algorytm nie działa.
        Demo_Wikipedia_CheckBox.Checked := true;

    end
  else//if czy_demo_g then
    begin

      Demo_BitBtn.Kind := bkRetry;

      //Demo_Kroki__Zwolnij_Wszystkie();

      //Demo_ProgressBar.Position := 0;

    end;
  //---//if czy_demo_g then


  Demo_BitBtn.Caption := '';
  Demo_BitBtn.ModalResult := mrNone;


  Demo__Kroki__Zwolnij_Wszystkie();


  if   ( not czy_demo_g )
    or ( demo_kroki_list = nil )
    or (  not Assigned( demo_kroki_list )  ) then
    Exit;


  //if platforma_strona_docelowa_zwyciestwo_g = ps_Lewa then
  //  platforma_strona_poczatkowa := ps_Prawa
  //else//if Wieza_Strona_Poczatkowa_RadioGroup.ItemIndex = 0 then
  //  platforma_strona_poczatkowa := ps_Lewa;

  if platforma_strona_docelowa_zwyciestwo_g = ps_Lewa then
    demo__platforma_strona_g := ps_Prawa
  else//if Wieza_Strona_Poczatkowa_RadioGroup.ItemIndex = 0 then
    demo__platforma_strona_g := ps_Lewa;


  czy_demo_wikipedia_g := Demo_Wikipedia_CheckBox.Checked;


  if czy_demo_wikipedia_g then
    begin

      // Wikipedia.

      //
      // https://pl.wikipedia.org/wiki/Wie%C5%BCe_Hanoi
      // Rozwiązanie iteracyjne
      //
      // [Algorytm dla przenoszenia wieży z platformy lewej na platformę prawą. Jeżeli przenosi się z platformy prawej na lewą to kolejny słupek jest wyznaczany w przeciwą stronę.]
      //
      // Algorytm iteracyjny składa się z następujących kroków:
      //   1 - przenieś najmniejszy krążek na kolejny* słupek,
      //   2 - wykonaj jedyny możliwy do wykonania ruch, nie zmieniając położenia krążka najmniejszego,
      //   3 - powtarzaj punkty 1 i 2, aż do odpowiedniego ułożenia wszystkich krążków.
      //
      // * - Kolejny słupek wyznaczamy w zależności od liczby krążków.
      //     Jeśli liczba krążków jest parzysta, kolejnym słupkiem jest ten po prawej stronie (gdy dojdziemy do słupka C w następnym ruchu używamy słupka A).
      //     Natomiast jeśli liczba krążków jest nieparzysta, kolejnym słupkiem jest ten po lewej stronie (gdy dojdziemy do słupka A w następnym ruchu używamy słupka C).
      //     [Ułożenie słupków A B C.]
      //
      //     [Nie wiem czy o to chodzi ale kierunek przenoszenia krążków należy wyznaczyć tylko raz na samym początku i potem go nie zmieniać.]
      //

      // Algorytm wymaga prawidłowego ułożenia poziomów wieży inaczej się zapętla i przekłada w kółko te same poziomy.
      Nowa_Gra_BitBtnClick( Sender ); // Ustawi czy_demo_g na false.
        Demo_BitBtn.Kind := bkNo;
        Demo_BitBtn.Caption := '';
        Demo_BitBtn.ModalResult := mrNone;
        czy_demo_g := true;


      //if platforma_strona_poczatkowa = ps_Lewa then
      if platforma_strona_docelowa_zwyciestwo_g = ps_Prawa then
        begin

          // Platforma strona początkowa lewa.

          demo__wikipedia__wieza_poziom_najmniejszy__platforma_strona_g := ps_Lewa;

          if wieza_poziomy_ilosc_g mod 2 = 0 then
            begin

              // Parzysta ilość poziomów.

              demo__wikipedia__wieza_poziom_najmniejszy__przenoszenie_kierunek_g := ps_Prawa;

            end
          else//if wieza_poziomy_ilosc_g mod 2 = 0 then
            begin

              // Nieparzysta ilość poziomów

              demo__wikipedia__wieza_poziom_najmniejszy__przenoszenie_kierunek_g := ps_Lewa;

            end;
          //---//if wieza_poziomy_ilosc_g mod 2 = 0 then

        end
      else//if platforma_strona_docelowa_zwyciestwo_g = ps_Prawa then
        begin

          // Platforma strona początkowa prawa.

          demo__wikipedia__wieza_poziom_najmniejszy__platforma_strona_g := ps_Prawa;

          if wieza_poziomy_ilosc_g mod 2 = 0 then
            begin

              // Parzysta ilość poziomów.

              demo__wikipedia__wieza_poziom_najmniejszy__przenoszenie_kierunek_g := ps_Lewa;

            end
          else//if wieza_poziomy_ilosc_g mod 2 = 0 then
            begin

              // Nieparzysta ilość poziomów

              demo__wikipedia__wieza_poziom_najmniejszy__przenoszenie_kierunek_g := ps_Prawa;

            end;
          //---//if wieza_poziomy_ilosc_g mod 2 = 0 then

        end;
      //---//if platforma_strona_docelowa_zwyciestwo_g = ps_Prawa then


      Demo_ProgressBar.Max := ruchy_ilosc__minimalna_potrzebna_g;

    end
  else//if czy_demo_wikipedia_g then
    begin

      // Mój.

      demo__wieza_poziomy_ilosc_g := wieza_poziomy_ilosc_g;
      //demo__platforma_strona_g := platforma_strona_poczatkowa;

      Demo_ProgressBar.Max := demo_kroki_list.Count;

    end;
  //---//if czy_demo_wikipedia_g then


  Demo__Etap_Przelicz();


  //???
  //if platforma_strona_poczatkowa = ps_Lewa then
  //  begin
  //
  //    if wieza_poziomy_ilosc_g mod 2 = 0 then
  //      begin
  //
  //        // Parzysta ilość poziomów.
  //
  //        zt_demo_krok := TDemo_Krok.Create( ps_Lewa, ps_Srodkowa );
  //        demo_kroki_list.Add( zt_demo_krok );
  //
  //        zt_demo_krok := TDemo_Krok.Create( ps_Lewa, ps_Prawa );
  //        demo_kroki_list.Add( zt_demo_krok );
  //
  //        zt_demo_krok := TDemo_Krok.Create( ps_Srodkowa, ps_Prawa );
  //        demo_kroki_list.Add( zt_demo_krok );
  //
  //      end
  //    else//if wieza_poziomy_ilosc_g mod 2 = 0 then
  //      begin
  //
  //        // Nieparzysta ilość poziomów
  //
  //        zt_demo_krok := TDemo_Krok.Create( ps_Lewa, ps_Prawa );
  //        demo_kroki_list.Add( zt_demo_krok );
  //
  //        zt_demo_krok := TDemo_Krok.Create( ps_Lewa, ps_Srodkowa );
  //        demo_kroki_list.Add( zt_demo_krok );
  //
  //        zt_demo_krok := TDemo_Krok.Create( ps_Prawa, ps_Srodkowa );
  //        demo_kroki_list.Add( zt_demo_krok );
  //
  //      end;
  //    //---//if wieza_poziomy_ilosc_g mod 2 = 0 then
  //
  //  end
  //else//if platforma_strona_poczatkowa = ps_Lewa then
  //  begin
  //
  //    if wieza_poziomy_ilosc_g mod 2 = 0 then
  //      begin
  //
  //        // Parzysta ilość poziomów.
  //
  //        zt_demo_krok := TDemo_Krok.Create( ps_Prawa, ps_Srodkowa );
  //        demo_kroki_list.Add( zt_demo_krok );
  //
  //        zt_demo_krok := TDemo_Krok.Create( ps_Prawa, ps_Lewa );
  //        demo_kroki_list.Add( zt_demo_krok );
  //
  //        zt_demo_krok := TDemo_Krok.Create( ps_Srodkowa, ps_Lewa );
  //        demo_kroki_list.Add( zt_demo_krok );
  //
  //      end
  //    else//if wieza_poziomy_ilosc_g mod 2 = 0 then
  //      begin
  //
  //        // Nieparzysta ilość poziomów
  //
  //        zt_demo_krok := TDemo_Krok.Create( ps_Prawa, ps_Lewa );
  //        demo_kroki_list.Add( zt_demo_krok );
  //
  //        zt_demo_krok := TDemo_Krok.Create( ps_Prawa, ps_Srodkowa );
  //        demo_kroki_list.Add( zt_demo_krok );
  //
  //        zt_demo_krok := TDemo_Krok.Create( ps_Lewa, ps_Srodkowa );
  //        demo_kroki_list.Add( zt_demo_krok );
  //
  //      end;
  //    //---//if wieza_poziomy_ilosc_g mod 2 = 0 then
  //
  //  end;
  ////---//if platforma_strona_poczatkowa = ps_Lewa then
  //
  //
  //if 1 = 2 then
  //  begin
  //
  //    // Algorytm dla wieży o 3 poziomach ustawionej na początku z lewej strony.
  //
  //    zt_demo_krok := TDemo_Krok.Create( ps_Lewa, ps_Prawa );
  //    demo_kroki_list.Add( zt_demo_krok );
  //
  //    zt_demo_krok := TDemo_Krok.Create( ps_Lewa, ps_Srodkowa );
  //    demo_kroki_list.Add( zt_demo_krok );
  //
  //    zt_demo_krok := TDemo_Krok.Create( ps_Prawa, ps_Srodkowa );
  //    demo_kroki_list.Add( zt_demo_krok );
  //
  //    zt_demo_krok := TDemo_Krok.Create( ps_Lewa, ps_Prawa );
  //    demo_kroki_list.Add( zt_demo_krok );
  //
  //    zt_demo_krok := TDemo_Krok.Create( ps_Srodkowa, ps_Lewa );
  //    demo_kroki_list.Add( zt_demo_krok );
  //
  //    zt_demo_krok := TDemo_Krok.Create( ps_Srodkowa, ps_Prawa );
  //    demo_kroki_list.Add( zt_demo_krok );
  //
  //    zt_demo_krok := TDemo_Krok.Create( ps_Lewa, ps_Prawa );
  //    demo_kroki_list.Add( zt_demo_krok );
  //
  //  end;
  ////---//if 1 = 2 then


  Demo_ProgressBar.Position := 0;

end;//---//Demo_BitBtnClick().

//Tlo_BitBtnClick().
procedure TWieze_Hanoi.Tlo_BitBtnClick( Sender : TObject );
var
  zts : string;
  search_rec : SysUtils.TSearchRec;
begin

  zts := 'Tła';

  if not SysUtils.DirectoryExists( zts ) then
    zts := '..\' + zts;

  if not SysUtils.DirectoryExists( zts ) then
    zts := '';


  if zts = '' then
    Exit;


  if GLMaterialLibrary1 = nil then
    GLMaterialLibrary1 := GLMaterial.TGLMaterialLibrary.Create( Application );

  //GLMaterialLibrary1.Materials.Clear(); // Usuwa wszystkie tekstury.

  if GLMaterialLibrary1.Materials.Count <= 0 then
    begin

      if SysUtils.FindFirst( zts + '\*.jpg', faAnyFile, search_rec ) = 0 then // Application potrzebuje w uses Forms.
        begin

          repeat //FindNext( search_rec ) <> 0;
            // Czasami bez begin i end nieprawidłowo rozpoznaje miejsca na umieszczenie breakpoint (linijkę za wysoko) w XE5.

            with GLMaterialLibrary1.AddTextureMaterial(  'Tło ' + IntToStr( GLMaterialLibrary1.Materials.Count + 1 ), zts + '\' + search_rec.Name  ) do
              begin

                Material.Texture.Disabled := false;
                Material.Texture.TextureMode := GLTexture.tmDecal;

              end;
            //---//with GLMaterialLibrary1.AddTextureMaterial(  'Tło ' + IntToStr( GLMaterialLibrary1.Materials.Count + 1 ), zts + '\' + search_rec.Name  ) do

          until SysUtils.FindNext( search_rec ) <> 0; // Zwraca dane kolejnego pliku zgodnego z parametrami wcześniej wywołanej funkcji FindFirst. Jeżeli można przejść do następnego znalezionego pliku zwraca 0.

        end;
      //---//if FindFirst( zts + '\*.jpg', faAnyFile, search_rec ) = 0 then // Application potrzebuje w uses Forms.

      SysUtils.FindClose( search_rec );

    end;
  //---//if GLMaterialLibrary1.Materials.Count <= 0 then


  if    ( Tlo_GLSkyBox = nil )
    and ( GLMaterialLibrary1.Materials.Count > 0 )then
    begin

      Tlo_GLSkyBox := GLSkyBox.TGLSkyBox.Create( Application );
      Tlo_GLSkyBox.Parent := Gra_GLScene.Objects;
      Tlo_GLSkyBox.Pickable := false;
      Tlo_GLSkyBox.MaterialLibrary := GLMaterialLibrary1;
      //Tlo_GLSkyBox.MatNameBack := 'Tło 1';
      Tlo_GLSkyBox.Scale.Scale( 0.05 );

      Tlo_GLSkyBox.MoveFirst();
      Tlo_GLSkyBox.Tag := -1;

    end;
  //---//if    ( Tlo_GLSkyBox = nil ) (...)


  if Tlo_GLSkyBox <> nil then
    begin

      if Tlo_GLSkyBox.Tag = -1 then
        Tlo_GLSkyBox.Tag := Random( GLMaterialLibrary1.Materials.Count ) + 1 // Za pierwszym razem losuje obrazek.
      else//if Tlo_GLSkyBox.Tag = -1 then
        begin

          if    ( Sender <> nil )
            and ( Sender = Tlo_Nastepne_BitBtn ) then
            Tlo_GLSkyBox.Tag := Tlo_GLSkyBox.Tag + 1
          else//if    ( Sender <> nil ) (...)
            Tlo_GLSkyBox.Tag := Tlo_GLSkyBox.Tag - 1;


          if Tlo_GLSkyBox.Tag > GLMaterialLibrary1.Materials.Count then
            Tlo_GLSkyBox.Tag := 0  // Brak obrazka.
          else//if Tlo_GLSkyBox.Tag > GLMaterialLibrary1.Materials.Count then
          if Tlo_GLSkyBox.Tag < 0 then
            Tlo_GLSkyBox.Tag := GLMaterialLibrary1.Materials.Count;

        end;
      //---//if Tlo_GLSkyBox.Tag = -1 then

      Tlo_GLSkyBox.MatNameBack := 'Tło ' + IntToStr( Tlo_GLSkyBox.Tag );

    end;
  //---//if Tlo_GLSkyBox <> nil then

end;//---//Tlo_BitBtnClick().

//Wieza_Poziomy_Ilosc_SpinEditKeyDown().
procedure TWieze_Hanoi.Wieza_Poziomy_Ilosc_SpinEditKeyDown( Sender : TObject; var Key : Word; Shift : TShiftState );
begin

  // Enter.
  if Key = 13 then
    begin

      Key := 0;
      Nowa_Gra_BitBtnClick( Sender );

    end;
  //---//if Key = 13 then

  //// Escape.
  //if Key = 27 then
  //  begin
  //    // Bez tego - gdy jest około 30+ poziomów to zgłasza wyjątek gdzieś tutaj.
  //    Key := 0;
  //    forma_glowna.Close();
  //
  //  end;
  ////---//if Key = 27 then

end;//---//Wieza_Poziomy_Ilosc_SpinEditKeyDown().

//Funkcja Wieza_Elementy__Zwolnij_Wszystkie().
procedure TWieze_Hanoi.Wieza_Elementy__Zwolnij_Wszystkie();
var
  i : integer;
begin

  if   ( wieza_elementy_list = nil )
    or (  not Assigned( wieza_elementy_list )  ) then
    Exit;

  for i := wieza_elementy_list.Count - 1 downto 0 do
    begin

      TGLSceneObject(wieza_elementy_list[ i ]).Free();
      wieza_elementy_list.Delete( i );

    end;
  //---//for i := wieza_elementy_list.Count - 1 downto 0 do

end;//---//Funkcja Wieza_Elementy__Zwolnij_Wszystkie().

//Funkcja Wieza_Element__Animacja_Ruchu().
procedure TWieze_Hanoi.Wieza_Element__Animacja_Ruchu( delta_czasu_f : double );
begin

  if not wieza_poziom_animacja_ruchu_r.animacja_trwa then
    Exit;


  if wieza_poziom_animacja_ruchu_r.wieza_poziom_animacja_ruchu_etap = wpare_Unoszenie then
    begin

      if TGLTorus(wieza_elementy_list[ wieza_poziom_animacja_ruchu_r.id_wieza_poziom_wybrany ]).Position.Y < wieza_poziom_animacja_ruchu_r.unoszenie_do_y then
        TGLTorus(wieza_elementy_list[ wieza_poziom_animacja_ruchu_r.id_wieza_poziom_wybrany ]).Move( 1.2 * delta_czasu_f )
      else//if TGLTorus(wieza_elementy_list[ wieza_poziom_animacja_ruchu_r.id_wieza_poziom_wybrany ]).Position.Y < wieza_poziom_animacja_ruchu_r.unoszenie_do_y then
        wieza_poziom_animacja_ruchu_r.wieza_poziom_animacja_ruchu_etap := wpare_Przesuwanie;

    end
  else//if wieza_poziom_animacja_ruchu_r.wieza_poziom_animacja_ruchu_etap = wpare_Unoszenie then
  if wieza_poziom_animacja_ruchu_r.wieza_poziom_animacja_ruchu_etap = wpare_Przesuwanie then
    begin

      if Abs( TGLTorus(wieza_elementy_list[ wieza_poziom_animacja_ruchu_r.id_wieza_poziom_wybrany ]).Position.X - wieza_poziom_animacja_ruchu_r.docelowe_x ) > 0.05 then
        begin

          if TGLTorus(wieza_elementy_list[ wieza_poziom_animacja_ruchu_r.id_wieza_poziom_wybrany ]).Position.X > wieza_poziom_animacja_ruchu_r.docelowe_x then
            TGLTorus(wieza_elementy_list[ wieza_poziom_animacja_ruchu_r.id_wieza_poziom_wybrany ]).Slide( 1.5 * delta_czasu_f )
          else//if TGLTorus(wieza_elementy_list[ wieza_poziom_animacja_ruchu_r.id_wieza_poziom_wybrany ]).Position.X > wieza_poziom_animacja_ruchu_r.docelowe_x then
            TGLTorus(wieza_elementy_list[ wieza_poziom_animacja_ruchu_r.id_wieza_poziom_wybrany ]).Slide( -1.5 * delta_czasu_f );

        end
      else//if Abs( TGLTorus(wieza_elementy_list[ wieza_poziom_animacja_ruchu_r.id_wieza_poziom_wybrany ]).Position.X - wieza_poziom_animacja_ruchu_r.docelowe_x ) > 0.05 then
        begin

          wieza_poziom_animacja_ruchu_r.wieza_poziom_animacja_ruchu_etap := wpare_Opadanie;
          TGLTorus(wieza_elementy_list[ wieza_poziom_animacja_ruchu_r.id_wieza_poziom_wybrany ]).Position.X := wieza_poziom_animacja_ruchu_r.docelowe_x;

        end;
      //---//if Abs( TGLTorus(wieza_elementy_list[ wieza_poziom_animacja_ruchu_r.id_wieza_poziom_wybrany ]).Position.X - wieza_poziom_animacja_ruchu_r.docelowe_x ) > 0.05 then

    end
  else//if wieza_poziom_animacja_ruchu_r.wieza_poziom_animacja_ruchu_etap = wpare_Przesuwanie then
  if wieza_poziom_animacja_ruchu_r.wieza_poziom_animacja_ruchu_etap = wpare_Opadanie then
    begin

      if TGLTorus(wieza_elementy_list[ wieza_poziom_animacja_ruchu_r.id_wieza_poziom_wybrany ]).Position.Y > wieza_poziom_animacja_ruchu_r.docelowe_y then
        TGLTorus(wieza_elementy_list[ wieza_poziom_animacja_ruchu_r.id_wieza_poziom_wybrany ]).Move( -1.2 * delta_czasu_f )
      else//if TGLTorus(wieza_elementy_list[ wieza_poziom_animacja_ruchu_r.id_wieza_poziom_wybrany ]).Position.Y > wieza_poziom_animacja_ruchu_r.docelowe_y then
        begin

          wieza_poziom_animacja_ruchu_r.wieza_poziom_animacja_ruchu_etap := wpare_Brak;
          wieza_poziom_animacja_ruchu_r.animacja_trwa := false;
          TGLTorus(wieza_elementy_list[ wieza_poziom_animacja_ruchu_r.id_wieza_poziom_wybrany ]).Position.Y := wieza_poziom_animacja_ruchu_r.docelowe_y;

          Zwyciestwo_Sprawdz();

          if czy_demo_g then
            demo_ostatni_krok_czas_g := Now();

        end;
      //---//if TGLTorus(wieza_elementy_list[ wieza_poziom_animacja_ruchu_r.id_wieza_poziom_wybrany ]).Position.Y > wieza_poziom_animacja_ruchu_r.docelowe_y then

    end;
  //---//if wieza_poziom_animacja_ruchu_r.wieza_poziom_animacja_ruchu_etap = wpare_Opadanie then

end;//---//Funkcja Wieza_Element__Animacja_Ruchu().

//Funkcja Ruchy_Ilosc_Etykieta_Wartosc_Wpisz().
procedure TWieze_Hanoi.Ruchy_Ilosc_Etykieta_Wartosc_Wpisz();
begin

  Ruchy_Ilosc_Label.Caption :=
    'Ilość ruchów: ' + Trim(  FormatFloat( '### ### ### ### ### ### ### ### ### ### ### ##0', ruchy_ilosc_g )  ) +
    ' / ' +
    Trim(  FormatFloat( '### ### ### ### ### ### ### ### ### ### ### ##0', ruchy_ilosc__minimalna_potrzebna_g )  );

  if ruchy_ilosc__minimalna_potrzebna_g <> 0 then
    Ruchy_Ilosc_Label.Caption := Ruchy_Ilosc_Label.Caption +
      ' (' + Trim(  FormatFloat( '### ### ### ### ### ### ### ### ### ### ### ##0', 100 * ruchy_ilosc_g / ruchy_ilosc__minimalna_potrzebna_g )  ) + '%)';

  Ruchy_Ilosc_Label.Caption := Ruchy_Ilosc_Label.Caption +
    '.';

end;//---//Funkcja Ruchy_Ilosc_Etykieta_Wartosc_Wpisz().

//Funkcja Platforma_Zaznaczenie_Ustaw().
procedure TWieze_Hanoi.Platforma_Zaznaczenie_Ustaw( const platforma_strona_f : TPlatforma_Strona );

  //Funkcja Platforma_Zaznaczenie_Zmien() w Platforma_Zaznaczenie_Ustaw().
  procedure Platforma_Zaznaczenie_Zmien( zt_gl_cube_f : TGLCube );
  begin

    if   ( platforma_strona_f = ps_Brak )
      or ( zt_gl_cube_f.Tag <> integer(platforma_strona_f) ) then
      zt_gl_cube_f.Material.MaterialOptions := zt_gl_cube_f.Material.MaterialOptions - [ GLMaterial.moNoLighting ]
    else//if platforma_strona_f = ps_Brak then
      zt_gl_cube_f.Material.MaterialOptions := zt_gl_cube_f.Material.MaterialOptions + [ GLMaterial.moNoLighting ];

  end;//---//Funkcja Platforma_Zaznaczenie_Zmien() w Platforma_Zaznaczenie_Ustaw().

begin//Funkcja Platforma_Zaznaczenie_Ustaw().

  Platforma_Zaznaczenie_Zmien( Platforma_Lewa_GLCube );
  Platforma_Zaznaczenie_Zmien( Platforma_Prawa_GLCube );
  Platforma_Zaznaczenie_Zmien( Platforma_Srodkowa_GLCube );

end;//---//Funkcja Platforma_Zaznaczenie_Ustaw().

//Funkcja Platki__Dodaj_Jeden().
procedure TWieze_Hanoi.Platki__Dodaj_Jeden();
var
  x_l,
  z_l
    : single;
  zt_platek : TPlatek;
begin

  if   ( platki_list = nil )
    or (  not Assigned( platki_list )  ) then
    Exit;

  if Gra_GLCadencer.CurrentTime - platek__ostatnio_utworzony_czas_g < platek_dodanie_sekundy_c then
    Exit;

  platek__ostatnio_utworzony_czas_g := Gra_GLCadencer.CurrentTime;

  zt_platek := TPlatek.Create( Application );
  zt_platek.Parent := Gra_GLScene.Objects;

  x_l := 2 * 2 * Platforma_Lewa_GLCube.Position.X * Random( 100 ) * 0.01 - 2 * Platforma_Lewa_GLCube.Position.X;
  z_l := 2 * 2 * Platforma_Lewa_GLCube.Position.X * Random( 100 ) * 0.01 - 2 * Platforma_Lewa_GLCube.Position.X;

  if    ( wieza_poziomy_ilosc_g > 19 )
    and ( Gra_GLCamera.SceneScale <> 0 ) then
    zt_platek.Position.AsVector := GLVectorGeometry.VectorMake(  x_l, 1 + 1 / Abs( Gra_GLCamera.SceneScale ), z_l  )
  else//if    ( wieza_poziomy_ilosc_g > 19 ) (...)
  if wieza_poziomy_ilosc_g > 10 then
    zt_platek.Position.AsVector := GLVectorGeometry.VectorMake( x_l, 2 + Gra_GLCamera.Position.Y, z_l )
  else//if wieza_poziomy_ilosc_g > 10 then
    zt_platek.Position.AsVector := GLVectorGeometry.VectorMake( x_l, 1.1 + Gra_GLCamera.Position.Y, z_l );


  zt_platek.opadanie_do_y := Platforma_Lewa_GLCube.Position.Y - Platforma_Lewa_GLCube.CubeHeight * 0.5 + zt_platek.strona_1_gl_disk.OuterRadius * 0.5;

  if zt_platek.Position.Z > 0 then
    zt_platek.opadanie_do_y := zt_platek.opadanie_do_y + 0.2 * Random( 100 ) * 0.01
  else//if zt_platek.Position.Z > 0 then
    zt_platek.opadanie_do_y := zt_platek.opadanie_do_y - Random( 100 ) * 0.01;


  platki_list.Add( zt_platek );

end;//---//Funkcja Platki__Dodaj_Jeden().

//Funkcja Platki__Zwolnij_Jeden().
procedure TWieze_Hanoi.Platki__Zwolnij_Jeden( pocisk_f : TPlatek  );
begin

  // Usuwać tylko w jednym miejscu. !!!
  // Wywołanie tej funkcji w kliku miejscach może coś zepsuć.

  if   ( platki_list = nil )
    or (  not Assigned( platki_list )  )
    or ( pocisk_f = nil ) then
    Exit;

  platki_list.Remove( pocisk_f );
  FreeAndNil( pocisk_f );

end;//---//Funkcja Platki__Zwolnij_Jeden().

//Funkcja Platki__Zwolnij_Wszystkie().
procedure TWieze_Hanoi.Platki__Zwolnij_Wszystkie();
var
  i : integer;
begin

  if   ( platki_list = nil )
    or (  not Assigned( platki_list )  ) then
    Exit;

  for i := platki_list.Count - 1 downto 0 do
    begin

      TPlatek(platki_list[ i ]).Free();
      platki_list.Delete( i );

    end;
  //---//for i := platki_list.Count - 1 downto 0 do

end;//---//Funkcja Platki__Zwolnij_Wszystkie().

//Funkcja Platki__Ruch().
procedure TWieze_Hanoi.Platki__Ruch( delta_czasu_f : double );
const
  sila_wspolczynnik_staly__obrot_c_l : double = 0.015; // delta_czasu_f
  sila_wspolczynnik_staly__przesuniecie_c_l : double = 0.025; // delta_czasu_f
var
  i,
  obrot_rodzaj,
  przesuniecie_rodzaj
    : integer;
  obrot_sila,
  przesuniecie_sila
    : real;
begin

  if   ( platki_list = nil )
    or (  not Assigned( platki_list )  ) then
    Exit;


  if platki_list.Count > 1000 then
    Platki__Zwolnij_Jeden( TPlatek(platki_list[ 0 ]) );


  if Gra_GLCadencer.CurrentTime - podmuch__ostatni_czas_g > podmuch__kolejny_sekundy_g then
    begin

      podmuch__ostatni_czas_g := Gra_GLCadencer.CurrentTime;
      podmuch__kolejny_sekundy_g := Random( 10 );

      przesuniecie_sila := Random( 25 ); // Siła podmuchu.
      przesuniecie_rodzaj := Random( 4 ) + 1;

    end
  else//if Gra_GLCadencer.CurrentTime - podmuch__ostatni_czas_g > podmuch__kolejny_sekundy_g then
    przesuniecie_rodzaj := 0;



  for i := 0 to platki_list.Count - 1 do
    //if TPlatek(platki_list[ i ]).Position.Y > Platforma_Lewa_GLCube.Position.Y - Platforma_Lewa_GLCube.CubeHeight * 0.5 + TPlatek(platki_list[ i ]).strona_1_gl_disk.OuterRadius * 0.5 then
    if TPlatek(platki_list[ i ]).Position.Y > TPlatek(platki_list[ i ]).opadanie_do_y then
      begin

        TPlatek(platki_list[ i ]).Lift( -platek_opadanie_szybkosc_c * delta_czasu_f );
        //TPlatek(platki_list[ i ]).Turn( 10 * delta_czasu_f );


        case przesuniecie_rodzaj of
          1 : GetOrCreateInertia( TPlatek(platki_list[ i ]) ).ApplyForce(   sila_wspolczynnik_staly__przesuniecie_c_l, GLVectorGeometry.VectorMake(  przesuniecie_sila * Random( 100 ) * 0.01, 0, 0  )   ); // Lewo.
          2 : GetOrCreateInertia( TPlatek(platki_list[ i ]) ).ApplyForce(   sila_wspolczynnik_staly__przesuniecie_c_l, GLVectorGeometry.VectorMake(  -przesuniecie_sila * Random( 100 ) * 0.01, 0, 0  )   ); // Prawo.
          3 : GetOrCreateInertia( TPlatek(platki_list[ i ]) ).ApplyForce(   sila_wspolczynnik_staly__przesuniecie_c_l, GLVectorGeometry.VectorMake(  0, przesuniecie_sila * Random( 100 ) * 0.005, 0  )   ); // Góra.
          4 : GetOrCreateInertia( TPlatek(platki_list[ i ]) ).ApplyForce(   sila_wspolczynnik_staly__przesuniecie_c_l, GLVectorGeometry.VectorMake(  0, -przesuniecie_sila * Random( 100 ) * 0.005, 0  )   ); // Dół.
          end;
        //---//case przesuniecie_rodzaj of


        //TPlatek(platki_list[ i ]).strony_kontener_gl_dummy_cube.Turn( 50 * delta_czasu_f );
        //TPlatek(platki_list[ i ]).strony_kontener_gl_dummy_cube.Roll( 50 * delta_czasu_f );
        //TPlatek(platki_list[ i ]).strony_kontener_gl_dummy_cube.Pitch( 50 * delta_czasu_f );

        obrot_sila := Random( 5000 ); // Siła obrotu.
        obrot_rodzaj := Random( 6 ) + 1;

        case obrot_rodzaj of
          1 : GetOrCreateInertia( TPlatek(platki_list[ i ]).strony_kontener_gl_dummy_cube ).ApplyTorque( sila_wspolczynnik_staly__obrot_c_l, 0, obrot_sila, 0 ); // Lewo.
          2 : GetOrCreateInertia( TPlatek(platki_list[ i ]).strony_kontener_gl_dummy_cube ).ApplyTorque( sila_wspolczynnik_staly__obrot_c_l, 0, -obrot_sila, 0 ); // Prawo.
          3 : GetOrCreateInertia( TPlatek(platki_list[ i ]).strony_kontener_gl_dummy_cube ).ApplyTorque( sila_wspolczynnik_staly__obrot_c_l, 0, 0, obrot_sila ); // Przód w dół.
          4 : GetOrCreateInertia( TPlatek(platki_list[ i ]).strony_kontener_gl_dummy_cube ).ApplyTorque( sila_wspolczynnik_staly__obrot_c_l, 0, 0, -obrot_sila ); // Przód w górę.
          5 : GetOrCreateInertia( TPlatek(platki_list[ i ]).strony_kontener_gl_dummy_cube ).ApplyTorque( sila_wspolczynnik_staly__obrot_c_l, obrot_sila, 0, 0 ); // Beczka (góra w) prawo.
          6 : GetOrCreateInertia( TPlatek(platki_list[ i ]).strony_kontener_gl_dummy_cube ).ApplyTorque( sila_wspolczynnik_staly__obrot_c_l, -obrot_sila, 0, 0 ); // Beczka (góra w) lewo.
          end;
        //---//case obrot_rodzaj of

      end
    else//if TPlatek(platki_list[ i ]).Position.Y > TPlatek(platki_list[ i ]).opadanie_do_y then
      begin

        case przesuniecie_rodzaj of
          1 : GetOrCreateInertia( TPlatek(platki_list[ i ]) ).ApplyForce(   sila_wspolczynnik_staly__przesuniecie_c_l, GLVectorGeometry.VectorMake(  przesuniecie_sila * Random( 100 ) * 0.01 * 0.3, 0, 0  )   ); // Lewo.
          2 : GetOrCreateInertia( TPlatek(platki_list[ i ]) ).ApplyForce(   sila_wspolczynnik_staly__przesuniecie_c_l, GLVectorGeometry.VectorMake(  -przesuniecie_sila * Random( 100 ) * 0.01 * 0.3, 0, 0  )   ); // Prawo.
          end;
        //---//case przesuniecie_rodzaj of

        if przesuniecie_rodzaj in [ 1, 2 ] then
          begin

            obrot_sila := Random( 5000 ) * 0.2; // Siła obrotu.
            obrot_rodzaj := Random( 6 ) + 1;

            case obrot_rodzaj of
              1 : GetOrCreateInertia( TPlatek(platki_list[ i ]).strony_kontener_gl_dummy_cube ).ApplyTorque( sila_wspolczynnik_staly__obrot_c_l, 0, obrot_sila, 0 ); // Lewo.
              2 : GetOrCreateInertia( TPlatek(platki_list[ i ]).strony_kontener_gl_dummy_cube ).ApplyTorque( sila_wspolczynnik_staly__obrot_c_l, 0, -obrot_sila, 0 ); // Prawo.
              3 : GetOrCreateInertia( TPlatek(platki_list[ i ]).strony_kontener_gl_dummy_cube ).ApplyTorque( sila_wspolczynnik_staly__obrot_c_l, 0, 0, obrot_sila ); // Przód w dół.
              4 : GetOrCreateInertia( TPlatek(platki_list[ i ]).strony_kontener_gl_dummy_cube ).ApplyTorque( sila_wspolczynnik_staly__obrot_c_l, 0, 0, -obrot_sila ); // Przód w górę.
              5 : GetOrCreateInertia( TPlatek(platki_list[ i ]).strony_kontener_gl_dummy_cube ).ApplyTorque( sila_wspolczynnik_staly__obrot_c_l, obrot_sila, 0, 0 ); // Beczka (góra w) prawo.
              6 : GetOrCreateInertia( TPlatek(platki_list[ i ]).strony_kontener_gl_dummy_cube ).ApplyTorque( sila_wspolczynnik_staly__obrot_c_l, -obrot_sila, 0, 0 ); // Beczka (góra w) lewo.
              end;
            //---//case obrot_rodzaj of

          end;
        //---//if przesuniecie_rodzaj in [ 1, 2 ] then

      end;
    //---//if TPlatek(platki_list[ i ]).Position.Y > TPlatek(platki_list[ i ]).opadanie_do_y then

end;//---//Funkcja Platki__Ruch().

//Funkcja Kamera_Wspolrzedne_Wypisz().
procedure TWieze_Hanoi.Kamera_Wspolrzedne_Wypisz();
begin

  if Kamera_Wspolrzedne_Label = nil then
    Exit;

  if Gra_GLCamera = nil then
    begin

      Kamera_Wspolrzedne_Label.Caption :=
        'Kamera <?>.';

      Exit;

    end;
  //---//if Gra_GLCamera = nil then


  Kamera_Wspolrzedne_Label.Caption :=
    'Kamera' + #13 +
    'x ' + Trim(  FormatFloat( '### ### ##0.00', Gra_GLCamera.Position.X )  ) + ';' + #13 +
    'y ' + Trim(  FormatFloat( '### ### ##0.00', Gra_GLCamera.Position.Y )  ) + ';' + #13 +
    's ' + Trim(  FormatFloat( '### ### ##0.00', Gra_GLCamera.SceneScale )  ) + '.';

end;//---//Funkcja Kamera_Wspolrzedne_Wypisz().

//Funkcja Fajerwerki_Animuj().
procedure TWieze_Hanoi.Fajerwerki_Animuj();
begin

  if   ( GLFireFXManager1.Disabled )
    or (  DateUtils.MilliSecondsBetween( Now(), fajerwerki_ostatnie_wywolanie_czas_g ) < 3000  ) then
    Exit;


  if Fajerwerki_Gl_Dummy_Cube.Tag = 0 then
    begin

      Fajerwerki_Gl_Dummy_Cube.Tag := 1;

      GLFireFXManager1.IsotropicExplosion
        (
          0.5,
          0.5,
          1,
          100
        );


    end
  else//if Fajerwerki_Gl_Dummy_Cube.Tag = 0 then
    begin

      Fajerwerki_Gl_Dummy_Cube.Tag := 0;

      GLFireFXManager1.RingExplosion
        (
          1,
          0,
          1,
          GLVectorGeometry.AffineVectorMake( 2, 0, 1 ),
          GLVectorGeometry.AffineVectorMake( 2, 0, 1 ),
          50
        );

    end;
  //---//if Fajerwerki_Gl_Dummy_Cube.Tag = 0 then


  Fajerwerki_Gl_Dummy_Cube.TagFloat := Fajerwerki_Gl_Dummy_Cube.TagFloat + 1;

  if Fajerwerki_Gl_Dummy_Cube.TagFloat >= 10 then
    begin

      GLFireFXManager1.Disabled := true;
      Fajerwerki_Gl_Dummy_Cube.TagFloat := 0;

    end;
  //---//if Fajerwerki_Gl_Dummy_Cube.TagFloat >= 10 then


  fajerwerki_ostatnie_wywolanie_czas_g := Now();

end;//---//Funkcja Fajerwerki_Animuj().

//Funkcja Demo__Animuj().
procedure TWieze_Hanoi.Demo__Animuj();
var
  i : integer;
  key : word;
begin

  if   ( not czy_demo_g )
    or ( wieza_poziom_animacja_ruchu_r.animacja_trwa )
    or (
             ( czy_demo_g )
         and (  DateUtils.MilliSecondsBetween( Now(), demo_ostatni_krok_czas_g ) < Demo_Oczekiwanie_Milisekund_SpinEdit.Value  )
       ) then
    Exit;


  if platforma_strona_wybrana_g <> ps_Brak then
    begin

      // Odznacza zaznaczenie platformy.

      key := Ord(  IntToStr( integer(platforma_strona_wybrana_g) )[ 1 ]  );
      Gra_GLSceneViewerKeyDown( Demo_BitBtn, key, [] );

    end;
  //---//if platforma_strona_wybrana_g <> ps_Brak then



  if   ( demo_kroki_list = nil )
    or (  not Assigned( demo_kroki_list )  ) then
    Exit;


  if czy_demo_wikipedia_g then
    czy_demo_wykonano_ruch_g := false
  else//if czy_demo_wikipedia_g then
    if demo_kroki_list.Count <= 0 then
      begin

        demo__wieza_poziomy_ilosc_g := 0; // Ilość poziomów na platformie na danym etapie animacji dema.

        for i := 0 to wieza_elementy_list.Count - 1 do
          begin

            if integer(demo__platforma_strona_g) = TGLTorus(wieza_elementy_list[ i ]).Tag then
              inc( demo__wieza_poziomy_ilosc_g );

          end;
        //---//for i := 0 to wieza_elementy_list.Count - 1 do

        if demo__wieza_poziomy_ilosc_g > 0 then
          Demo__Etap_Przelicz()
        else//if demo__wieza_poziomy_ilosc_g > 0 then
          Demo_BitBtnClick( nil ); //???

        Exit;

      end;
    //---//if demo_kroki_list.Count <= 0 then


  Gra_GLSceneViewerKeyDown( Demo_BitBtn, TDemo_Krok(demo_kroki_list[ 0 ]).platforma_strona__z, [] );
  Gra_GLSceneViewerKeyDown( Demo_BitBtn, TDemo_Krok(demo_kroki_list[ 0 ]).platforma_strona__do, [] );


  if    ( czy_demo_wikipedia_g )
    and ( demo_kroki_list.Count <= 2 )
    and ( czy_demo_wykonano_ruch_g ) then
    Demo__Kroki__Zwolnij_Wszystkie();


  if demo_kroki_list.Count > 0 then // Zakończenie dema (zwycięstwo) zwalnia wszystkie kroki.
    begin

      TDemo_Krok(demo_kroki_list[ 0 ]).Free();
      demo_kroki_list.Delete( 0 );

    end
  else//if demo_kroki_list.Count > 0 then
    if czy_demo_wikipedia_g then
      Demo__Etap_Przelicz();


  demo_ostatni_krok_czas_g := Now();

  if not czy_demo_wikipedia_g then
    Demo_ProgressBar.StepIt(); //???

end;//---//Funkcja Demo__Animuj().

//Funkcja Demo__Etap_Przelicz().
procedure TWieze_Hanoi.Demo__Etap_Przelicz();

  //Funkcja Platforma_Obok_Wyznacz() w Demo__Etap_Przelicz().
  function Platforma_Obok_Wyznacz( const platforma_strona_f, kierunek_f : TPlatforma_Strona ) : TPlatforma_Strona;
  var
    zti : integer;
  begin

    zti := integer(platforma_strona_f);

    if kierunek_f = ps_Lewa then
      begin

        dec( zti );

        if zti <= integer(ps_Brak) then
          zti := integer(ps_Prawa)

      end
    else//if kierunek_f = ps_Lewa then
      begin

        inc( zti );

        if zti > integer(High( TPlatforma_Strona )) then
          zti := integer(ps_Lewa);

      end;
    //---//if kierunek_f = ps_Lewa then

    Result := TPlatforma_Strona(zti);

  end;//---//Funkcja Platforma_Obok_Wyznacz() w Demo__Etap_Przelicz().

var
  zt_demo_krok : TDemo_Krok;
  platforma_strona_poczatkowa,
  zt_platforma_strona
    : TPlatforma_Strona;
begin//Funkcja Demo__Etap_Przelicz().

  if czy_demo_wikipedia_g then
    begin

      // Wikipedia.

      platforma_strona_poczatkowa := demo__wikipedia__wieza_poziom_najmniejszy__platforma_strona_g;

      demo__wikipedia__wieza_poziom_najmniejszy__platforma_strona_g := Platforma_Obok_Wyznacz( platforma_strona_poczatkowa, demo__wikipedia__wieza_poziom_najmniejszy__przenoszenie_kierunek_g );

      // Przenosi najniższy poziom wieży.
      zt_demo_krok := TDemo_Krok.Create( platforma_strona_poczatkowa, demo__wikipedia__wieza_poziom_najmniejszy__platforma_strona_g );
      demo_kroki_list.Add( zt_demo_krok );


      zt_platforma_strona := Platforma_Obok_Wyznacz( demo__wikipedia__wieza_poziom_najmniejszy__platforma_strona_g, demo__wikipedia__wieza_poziom_najmniejszy__przenoszenie_kierunek_g );

      // Któryś z poniższych ruchów powinien być możliwy do wykonania.
      zt_demo_krok := TDemo_Krok.Create( platforma_strona_poczatkowa, zt_platforma_strona );
      demo_kroki_list.Add( zt_demo_krok );

      zt_demo_krok := TDemo_Krok.Create( zt_platforma_strona, platforma_strona_poczatkowa );
      demo_kroki_list.Add( zt_demo_krok );

    end
  else//if czy_demo_wikipedia_g then
    begin

      // Mój.

      if demo__platforma_strona_g = ps_Lewa then
        begin

          if demo__wieza_poziomy_ilosc_g mod 2 = 0 then
            begin

              // Parzysta ilość poziomów.

              zt_demo_krok := TDemo_Krok.Create( ps_Lewa, ps_Srodkowa );
              demo_kroki_list.Add( zt_demo_krok );

              zt_demo_krok := TDemo_Krok.Create( ps_Lewa, ps_Prawa );
              demo_kroki_list.Add( zt_demo_krok );

              zt_demo_krok := TDemo_Krok.Create( ps_Srodkowa, ps_Prawa );
              demo_kroki_list.Add( zt_demo_krok );

            end
          else//if demo__wieza_poziomy_ilosc_g mod 2 = 0 then
            begin

              // Nieparzysta ilość poziomów

              zt_demo_krok := TDemo_Krok.Create( ps_Lewa, ps_Prawa );
              demo_kroki_list.Add( zt_demo_krok );

              zt_demo_krok := TDemo_Krok.Create( ps_Lewa, ps_Srodkowa );
              demo_kroki_list.Add( zt_demo_krok );

              zt_demo_krok := TDemo_Krok.Create( ps_Prawa, ps_Srodkowa );
              demo_kroki_list.Add( zt_demo_krok );

            end;
          //---//if demo__wieza_poziomy_ilosc_g mod 2 = 0 then

        end
      else//if demo__platforma_strona_g = ps_Lewa then
        begin

          if demo__wieza_poziomy_ilosc_g mod 2 = 0 then
            begin

              // Parzysta ilość poziomów.

              zt_demo_krok := TDemo_Krok.Create( ps_Prawa, ps_Srodkowa );
              demo_kroki_list.Add( zt_demo_krok );

              zt_demo_krok := TDemo_Krok.Create( ps_Prawa, ps_Lewa );
              demo_kroki_list.Add( zt_demo_krok );

              zt_demo_krok := TDemo_Krok.Create( ps_Srodkowa, ps_Lewa );
              demo_kroki_list.Add( zt_demo_krok );

            end
          else//if demo__wieza_poziomy_ilosc_g mod 2 = 0 then
            begin

              // Nieparzysta ilość poziomów

              zt_demo_krok := TDemo_Krok.Create( ps_Prawa, ps_Lewa );
              demo_kroki_list.Add( zt_demo_krok );

              zt_demo_krok := TDemo_Krok.Create( ps_Prawa, ps_Srodkowa );
              demo_kroki_list.Add( zt_demo_krok );

              zt_demo_krok := TDemo_Krok.Create( ps_Lewa, ps_Srodkowa );
              demo_kroki_list.Add( zt_demo_krok );

            end;
          //---//if demo__wieza_poziomy_ilosc_g mod 2 = 0 then

        end;
      //---//if demo__platforma_strona_g = ps_Lewa then

    end;
  //---//if czy_demo_wikipedia_g then

end;//---//Funkcja Demo__Etap_Przelicz().

//Funkcja Demo__Kroki__Zwolnij_Wszystkie().
procedure TWieze_Hanoi.Demo__Kroki__Zwolnij_Wszystkie();
var
  i : integer;
begin

  if   ( demo_kroki_list = nil )
    or (  not Assigned( demo_kroki_list )  ) then
    Exit;

  for i := demo_kroki_list.Count - 1 downto 0 do
    begin

      TDemo_Krok(demo_kroki_list[ i ]).Free();
      demo_kroki_list.Delete( i );

    end;
  //---//for i := demo_kroki_list.Count - 1 downto 0 do

end;//---//Funkcja Demo__Kroki__Zwolnij_Wszystkie().

//Funkcja Zwyciestwo_Sprawdz().
procedure TWieze_Hanoi.Zwyciestwo_Sprawdz();
var
  i,
  zti
    : integer;
begin

  zti := 0;

  for i := 0 to wieza_elementy_list.Count - 1 do
    begin

      if TGLTorus(wieza_elementy_list[ i ]).Tag = integer(platforma_strona_docelowa_zwyciestwo_g) then
        inc( zti );

    end;
  //---//for i := 0 to wieza_elementy_list.Count - 1 do


  if zti = wieza_poziomy_ilosc_g then
    begin

      // Zwycięstwo.

      if czy_demo_g then
        Demo_BitBtnClick( nil );

      if platforma_strona_docelowa_zwyciestwo_g = ps_Lewa then
        platforma_strona_docelowa_zwyciestwo_g := ps_Prawa
      else//if platforma_strona_docelowa_zwyciestwo_g = ps_Lewa then
        platforma_strona_docelowa_zwyciestwo_g := ps_Lewa;


      Fajerwerki_Gl_Dummy_Cube.TagFloat := 0; // Jeżeli gracz zakończy układanie wieży nim upłynie czas fajerwerków.


      GLFireFXManager1.Disabled := false;

    end;
  //---//if zti = wieza_poziomy_ilosc_g then

end;//---//Funkcja Zwyciestwo_Sprawdz().

//Gra_GLCadencerProgress().
procedure TWieze_Hanoi.Gra_GLCadencerProgress( Sender : TObject; const deltaTime, newTime : Double );
begin

  if Platki_CheckBox.Checked then
    begin

      Platki__Dodaj_Jeden();

      Platki__Ruch( deltaTime );

    end
  else//if Platki_CheckBox.Checked then
    begin

      if platki_list.Count > 0 then
        Platki__Zwolnij_Wszystkie();

    end;
  //---//if Platki_CheckBox.Checked then


  Wieza_Element__Animacja_Ruchu( deltaTime );

  Wieza_Poziom_Numer_TimerTimer( Sender );

  Fajerwerki_Animuj();

  Demo__Animuj();


  {$IFDEF lazarus}
  // W lazarusie panel o programie się nie odświeża.

  if DateUtils.MilliSecondsBetween( Now(), ostatnie_odswiezenie_okna_czas_g ) > 1500 then
    begin

      ostatnie_odswiezenie_okna_czas_g := Now();

      //if O_Programie_Panel <> nil then
      //  O_Programie_Panel.Repaint();
      //
      //if O_Programie_Splitter <> nil then
      //  O_Programie_Splitter.Repaint();

      if forma_glowna <> nil then
        begin

          if forma_glowna.BorderStyle = bsNone then // W delphi przy pełnym ekranie elementy poza Gra_GLSceneViewer znikają.
            O_Programie_Splitter.Visible := O_Programie_Panel.Width > 1
          else//if forma_glowna.BorderStyle = bsNone then
            if not O_Programie_Splitter.Visible then
              O_Programie_Splitter.Visible := true;

          O_Programie_Panel.Visible := O_Programie_Splitter.Visible;


          forma_glowna.Repaint();

        end;
      //---//if forma_glowna <> nil then

    end;
  //---//if DateUtils.MilliSecondsBetween( Now(), ostatnie_odswiezenie_okna_czas_g ) > 1500 then
  {$ENDIF}

end;//---//Gra_GLCadencerProgress().

//Gra_GLSceneViewerMouseDown().
procedure TWieze_Hanoi.Gra_GLSceneViewerMouseDown( Sender : TObject; Button : TMouseButton; Shift : TShiftState; X, Y : Integer );
var
  i,
  id_wieza_poziom__docelowy, // Indeks na liście poziomów wieży pierwszego znalezionego poziomu wieży na podeście docelowym.
  id_wieza_poziom__wybrany, // Indeks na liście poziomów wieży najwyższego poziomu wieży na wybranym podeście.
  wieza_poziom__docelowy__ilosc, // Ile jest poziomów wieży na podeście docelowym.
  wieza_poziom__wybrany__ilosc // Ile jest poziomów wieży na wybranym do przeniesienia podeście.
    : integer;

  //wieza_poziom_najwyzszy__docelowy__promien, // Promień najwyższego poziomu wieży na podeście docelowym.
  wieza_poziom_najwyzszy__docelowy__wielkosc, // Wielkość najwyższego poziomu wieży na podeście docelowym.
  wieza_poziom_najwyzszy__docelowy__y, // Współrzędna Y (wysokości) najwyższego poziomu wieży na podeście docelowym.
  //wieza_poziom_najwyzszy__wybrany__promien, // Promień najwyższego poziomu wieży na wybranym podeście.
  wieza_poziom_najwyzszy__srodkowy__y, // Współrzędna Y (wysokości) najwyższego poziomu wieży na podeście środkowym.
  wieza_poziom_najwyzszy__wybrany__wielkosc, // Wielkość najwyższego poziomu wieży na wybranym podeście.
  wieza_poziom_najwyzszy__wybrany__y // Współrzędna Y (wysokości) najwyższego poziomu wieży na wybranym podeście.
    : single;

  zt_gl_scene_object : TGLSceneObject;
begin

  if   ( not czy_demo_g )
    or (
             ( czy_demo_g ) // W trakcie trwania dema sterowanie inne niż wywołane przez demo jest nieaktywne.
         and ( Sender <> nil )
         and ( Sender is TBitBtn )
         and ( Sender = Demo_BitBtn )
       ) then
    // Dobrze.
  else//if    ( czy_demo_g ) (...)
    Exit;


  zt_gl_scene_object := nil;


  if ( Gra_GLSceneViewer.Buffer.GetPickedObject( x, y ) is TGLSceneObject ) then
    zt_gl_scene_object := ( Gra_GLSceneViewer.Buffer.GetPickedObject( x, y ) as TGLSceneObject );


  if    ( zt_gl_scene_object <> nil )
    and (  zt_gl_scene_object.Tag >= integer(Low( TPlatforma_Strona ))  )
    and (  zt_gl_scene_object.Tag <= integer(High( TPlatforma_Strona ))  ) then
    begin

      if platforma_strona_wybrana_g = ps_Brak then
        begin

          // Wskazanie platformy.

          wieza_poziom__wybrany__ilosc := 0;

          for i := 0 to wieza_elementy_list.Count - 1 do
            if zt_gl_scene_object.Tag = TGLTorus(wieza_elementy_list[ i ]).Tag then
              begin

                inc( wieza_poziom__wybrany__ilosc );
                Break;

              end;
            //---//if zt_gl_scene_object.Tag = TGLTorus(wieza_elementy_list[ i ]).Tag then


          if wieza_poziom__wybrany__ilosc > 0 then
            begin

              platforma_strona_wybrana_g := TPlatforma_Strona(zt_gl_scene_object.Tag);
              Platforma_Zaznaczenie_Ustaw( platforma_strona_wybrana_g );

            end;
          //---//if wieza_poziom__wybrany__ilosc > 0 then

        end
      else//if platforma_strona_wybrana_g = ps_Brak then
      if    ( platforma_strona_wybrana_g <> ps_Brak )
        and ( integer(platforma_strona_wybrana_g) = zt_gl_scene_object.Tag )then
        begin

          // Odznaczenie wskazanej platformy.

          platforma_strona_wybrana_g := ps_Brak;
          Platforma_Zaznaczenie_Ustaw( platforma_strona_wybrana_g );

        end
      else//if   ( platforma_strona_wybrana_g <> ps_Brak ) (...)
        begin

          // Próba przeniesienia poziomu wieży.

          if not wieza_poziom_animacja_ruchu_r.animacja_trwa then
            begin

              id_wieza_poziom__docelowy := -99;
              wieza_poziom__docelowy__ilosc := 0;
              wieza_poziom__wybrany__ilosc := 0;

              //wieza_poziom_najwyzszy__docelowy__promien := 0;
              wieza_poziom_najwyzszy__docelowy__wielkosc := 0;
              wieza_poziom_najwyzszy__docelowy__y := 0;
              //wieza_poziom_najwyzszy__wybrany__promien := 0;
              wieza_poziom_najwyzszy__wybrany__wielkosc := 0;
              wieza_poziom_najwyzszy__wybrany__y := 0;


              if Animacja_CheckBox.Checked then
                wieza_poziom_najwyzszy__srodkowy__y := -1;
              //  wieza_poziom_animacja_ruchu_r.unoszenie_do_y := -1;

              for i := 0 to wieza_elementy_list.Count - 1 do
                begin

                  if zt_gl_scene_object.Tag = TGLTorus(wieza_elementy_list[ i ]).Tag then
                    inc( wieza_poziom__docelowy__ilosc );

                  if    ( zt_gl_scene_object.Tag = TGLTorus(wieza_elementy_list[ i ]).Tag )
                    and ( TGLTorus(wieza_elementy_list[ i ]).Position.Y > wieza_poziom_najwyzszy__docelowy__y ) then
                    begin

                      if id_wieza_poziom__docelowy = -99 then
                        id_wieza_poziom__docelowy := i;

                      wieza_poziom_najwyzszy__docelowy__y := TGLTorus(wieza_elementy_list[ i ]).Position.Y;
                      //wieza_poziom_najwyzszy__docelowy__promien := TGLTorus(wieza_elementy_list[ i ]).MajorRadius;
                      wieza_poziom_najwyzszy__docelowy__wielkosc := TGLTorus(wieza_elementy_list[ i ]).TagFloat;

                    end;
                  //---//if    ( zt_gl_scene_object.Tag = TGLTorus(wieza_elementy_list[ i ]).Tag ) (...)


                  if integer(platforma_strona_wybrana_g) = TGLTorus(wieza_elementy_list[ i ]).Tag then
                    inc( wieza_poziom__wybrany__ilosc );

                  if    ( integer(platforma_strona_wybrana_g) = TGLTorus(wieza_elementy_list[ i ]).Tag )
                    and ( TGLTorus(wieza_elementy_list[ i ]).Position.Y > wieza_poziom_najwyzszy__wybrany__y ) then
                    begin

                      id_wieza_poziom__wybrany := i;
                      wieza_poziom_najwyzszy__wybrany__y := TGLTorus(wieza_elementy_list[ i ]).Position.Y;
                      //wieza_poziom_najwyzszy__wybrany__promien := TGLTorus(wieza_elementy_list[ i ]).MajorRadius;
                      wieza_poziom_najwyzszy__wybrany__wielkosc := TGLTorus(wieza_elementy_list[ i ]).TagFloat;

                    end;
                  //---//if    ( integer(platforma_strona_wybrana_g) = TGLTorus(wieza_elementy_list[ i ]).Tag ) (...)


                  //if    ( Animacja_CheckBox.Checked )
                  //  and ( wieza_poziom_animacja_ruchu_r.unoszenie_do_y < TGLTorus(wieza_elementy_list[ i ]).Position.Y ) then
                  //  wieza_poziom_animacja_ruchu_r.unoszenie_do_y := TGLTorus(wieza_elementy_list[ i ]).Position.Y;

                  if    ( Animacja_CheckBox.Checked )
                    and ( TGLTorus(wieza_elementy_list[ i ]).Tag = integer(ps_Srodkowa) )
                    and ( wieza_poziom_najwyzszy__srodkowy__y < TGLTorus(wieza_elementy_list[ i ]).Position.Y ) then
                    wieza_poziom_najwyzszy__srodkowy__y := TGLTorus(wieza_elementy_list[ i ]).Position.Y;

                end;
              //---//for i := 0 to wieza_elementy_list.Count - 1 do


              if   ( wieza_poziom__docelowy__ilosc = 0 )
                //or ( wieza_poziom_najwyzszy__wybrany__promien < wieza_poziom_najwyzszy__docelowy__promien )then
              or ( wieza_poziom_najwyzszy__wybrany__wielkosc < wieza_poziom_najwyzszy__docelowy__wielkosc )then
                begin

                  // Można wykonać ruch.

                  if Animacja_CheckBox.Checked then
                    begin

                      wieza_poziom_animacja_ruchu_r.animacja_trwa := true;
                      wieza_poziom_animacja_ruchu_r.id_wieza_poziom_wybrany := id_wieza_poziom__wybrany;

                    end;
                  //---//if Animacja_CheckBox.Checked then


                  czy_demo_wykonano_ruch_g := true;

                  if    ( czy_demo_g )
                    and ( czy_demo_wikipedia_g ) then
                    Demo_ProgressBar.StepIt();


                  inc( ruchy_ilosc_g );
                  Ruchy_Ilosc_Etykieta_Wartosc_Wpisz();

                  platforma_strona_wybrana_g := ps_Brak;
                  Platforma_Zaznaczenie_Ustaw( platforma_strona_wybrana_g );

                  i := TGLTorus(wieza_elementy_list[ id_wieza_poziom__wybrany ]).Tag; // Tutaj tymczasowo jako kopia strony wybranego poziomu wieży do przesunięcia.
                  TGLTorus(wieza_elementy_list[ id_wieza_poziom__wybrany ]).Tag := zt_gl_scene_object.Tag;

                  if id_wieza_poziom__docelowy <> -99 then // Na podeście docelowym są już jakieś poziomy wieży (przepisuje wartość z pierwszego znalezionego poziomu wieży na podeście docelowym).
                    wieza_poziom_animacja_ruchu_r.docelowe_x := TGLTorus(wieza_elementy_list[ id_wieza_poziom__docelowy ]).Position.X
                  else // Jeżeli na podeście docelowym nie ma żadnych poziomów wieży przepisuje wartość z odpowiedniego podestu.
                  if TGLTorus(wieza_elementy_list[ id_wieza_poziom__wybrany ]).Tag = Platforma_Lewa_GLCube.Tag then
                    wieza_poziom_animacja_ruchu_r.docelowe_x := Platforma_Lewa_GLCube.Position.X
                  else
                  if TGLTorus(wieza_elementy_list[ id_wieza_poziom__wybrany ]).Tag = Platforma_Prawa_GLCube.Tag then
                    wieza_poziom_animacja_ruchu_r.docelowe_x := Platforma_Prawa_GLCube.Position.X
                  else
                  if TGLTorus(wieza_elementy_list[ id_wieza_poziom__wybrany ]).Tag = Platforma_Srodkowa_GLCube.Tag then
                    wieza_poziom_animacja_ruchu_r.docelowe_x := Platforma_Srodkowa_GLCube.Position.X;

                  wieza_poziom_animacja_ruchu_r.docelowe_y := TGLTorus(wieza_elementy_list[ id_wieza_poziom__wybrany ]).MinorRadius + TGLTorus(wieza_elementy_list[ id_wieza_poziom__wybrany ]).MinorRadius * wieza_poziom__docelowy__ilosc * 2;


                  if not Animacja_CheckBox.Checked then
                    begin

                      TGLTorus(wieza_elementy_list[ id_wieza_poziom__wybrany ]).Position.X := wieza_poziom_animacja_ruchu_r.docelowe_x;
                      TGLTorus(wieza_elementy_list[ id_wieza_poziom__wybrany ]).Position.Y := wieza_poziom_animacja_ruchu_r.docelowe_y;

                      Zwyciestwo_Sprawdz();

                    end
                  else//if not Animacja_CheckBox.Checked then
                    begin

                      if    ( i <> integer(ps_Srodkowa) ) // Wybrany.
                        and ( TGLTorus(wieza_elementy_list[ id_wieza_poziom__wybrany ]).Tag <> integer(ps_Srodkowa) ) // Docelowy.
                        and ( wieza_poziom_najwyzszy__srodkowy__y >= wieza_poziom_najwyzszy__wybrany__y )
                        and ( wieza_poziom_najwyzszy__srodkowy__y >= wieza_poziom_najwyzszy__docelowy__y ) then
                        wieza_poziom_animacja_ruchu_r.unoszenie_do_y := wieza_poziom_najwyzszy__srodkowy__y + TGLTorus(wieza_elementy_list[ id_wieza_poziom__wybrany ]).MinorRadius * 2
                      else//if    ( i <> integer(ps_Srodkowa) ) (...)
                        if wieza_poziom_najwyzszy__wybrany__y > wieza_poziom_najwyzszy__docelowy__y then
                          wieza_poziom_animacja_ruchu_r.unoszenie_do_y := wieza_poziom_najwyzszy__wybrany__y
                        else//if wieza_poziom_najwyzszy__wybrany__y > wieza_poziom_najwyzszy__docelowy__y then
                          wieza_poziom_animacja_ruchu_r.unoszenie_do_y := wieza_poziom_najwyzszy__docelowy__y + TGLTorus(wieza_elementy_list[ id_wieza_poziom__wybrany ]).MinorRadius * 2;


                      wieza_poziom_animacja_ruchu_r.unoszenie_do_y := wieza_poziom_animacja_ruchu_r.unoszenie_do_y + TGLTorus(wieza_elementy_list[ id_wieza_poziom__wybrany ]).MinorRadius * 2;

                      wieza_poziom_animacja_ruchu_r.wieza_poziom_animacja_ruchu_etap := wpare_Unoszenie;

                    end;
                  //---//if not Animacja_CheckBox.Checked then

                end
              else//if   ( wieza_poziom__docelowy__ilosc = 0 ) (...)
                begin

                  // Nie można wykonać ruchu.

                end;
              //---//if   ( wieza_poziom__docelowy__ilosc = 0 ) (...)

            end;
          //---//if not wieza_poziom_animacja_ruchu_r.animacja_trwa then

        end;
      //---//if   ( platforma_strona_wybrana_g <> ps_Brak ) (...)

    end;
  //---//if    ( zt_gl_scene_object <> nil ) (...)


  Gra_GLSceneViewer.SetFocus();

end;//---//Gra_GLSceneViewerMouseDown().

//Gra_GLSceneViewerKeyDown().
procedure TWieze_Hanoi.Gra_GLSceneViewerKeyDown( Sender : TObject; var Key : Word; Shift : TShiftState );
const
  kamera_skok_c_l : single = 0.01;
var
  x_l,
  y_l
    : integer;
  kamera_skok_l : single;
begin

  // Esc.
  if    ( Key = 27 )
    and ( platforma_strona_wybrana_g <> ps_Brak )then
    begin

      // Odznaczenie wskazanej platformy.

      platforma_strona_wybrana_g := ps_Brak;
      Platforma_Zaznaczenie_Ustaw( platforma_strona_wybrana_g );

      Key := 0;

    end;
  //---//if    ( Key = 27 ) (...)


  if   ( not czy_demo_g )
    or (
             ( czy_demo_g ) // W trakcie trwania dema sterowanie inne niż wywołane przez demo jest nieaktywne.
         and ( Sender <> nil )
         and ( Sender is TBitBtn )
         and ( Sender = Demo_BitBtn )
       ) then
    if   (  Key = Ord( '1' )  )
      or (  Key = Ord( '2' )  )
      or (  Key = Ord( '3' )  ) then
      begin

        if Key = Ord( '1' ) then
          begin

            x_l := Round(  Gra_GLSceneViewer.Buffer.WorldToScreen( Platforma_Lewa_GLCube.Position.AsAffineVector ).X  );
            y_l := Round(  Gra_GLSceneViewer.Buffer.WorldToScreen( Platforma_Lewa_GLCube.Position.AsAffineVector ).Y  );

          end
        else//if Key = Ord( '1' ) then
        if Key = Ord( '2' ) then
          begin

            x_l := Round(  Gra_GLSceneViewer.Buffer.WorldToScreen( Platforma_Srodkowa_GLCube.Position.AsAffineVector ).X  );
            y_l := Round(  Gra_GLSceneViewer.Buffer.WorldToScreen( Platforma_Srodkowa_GLCube.Position.AsAffineVector ).Y  );

          end
        else//if Key = Ord( '2' ) then
        if Key = Ord( '3' ) then
          begin

            x_l := Round(  Gra_GLSceneViewer.Buffer.WorldToScreen( Platforma_Prawa_GLCube.Position.AsAffineVector ).X  );
            y_l := Round(  Gra_GLSceneViewer.Buffer.WorldToScreen( Platforma_Prawa_GLCube.Position.AsAffineVector ).Y  );

          end;
        //---//if Key = Ord( '3' ) then


        y_l := Gra_GLSceneViewer.Height - y_l;

        Gra_GLSceneViewerMouseDown( Sender, Controls.mbLeft, Shift, x_l, y_l );


        Key := 0;

      end;
    //---//if Key = Ord( '1' ) then


  if Key = Ord( 'E' ) then
    begin

      Pelny_Ekran_BitBtnClick( Sender );
      Key := 0;

    end;
  //---//if Key = Ord( 'E' ) then


  if Key = 188 then // ,
    begin

      Tlo_BitBtnClick( Tlo_Poprzednie_BitBtn );
      Key := 0;

    end;
  //---//if Key = 188 then

  if Key = 190 then // .
    begin

      Tlo_BitBtnClick( Tlo_Nastepne_BitBtn );
      Key := 0;

    end;
  //---//if Key = 190 then


  if ssShift in Shift then
    kamera_skok_l := kamera_skok_c_l * 10
  else//if ssShift in Shift then
    kamera_skok_l := kamera_skok_c_l;


  if Key = Ord( 'W' ) then
    begin

      Gra_GLCamera.Position.Y := Gra_GLCamera.Position.Y + kamera_skok_l;
      Key := 0;

    end;
  //---//if Key = Ord( 'W' ) then

  if Key = Ord( 'S' ) then
    begin

      Gra_GLCamera.Position.Y := Gra_GLCamera.Position.Y - kamera_skok_l;
      Key := 0;

    end;
  //---//if Key = Ord( 'S' ) then


  if Key = Ord( 'A' ) then
    begin

      Gra_GLCamera.Position.X := Gra_GLCamera.Position.X + kamera_skok_l;
      Key := 0;

    end;
  //---//if Key = Ord( 'A' ) then

  if Key = Ord( 'D' ) then
    begin

      Gra_GLCamera.Position.X := Gra_GLCamera.Position.X - kamera_skok_l;
      Key := 0;

    end;
  //---//if Key = Ord( 'D' ) then


  if Key = Ord( 'R' ) then
    begin

      Gra_GLCamera.SceneScale := Gra_GLCamera.SceneScale + kamera_skok_l;
      Key := 0;

    end;
  //---//if Key = Ord( 'R' ) then

  if Key = Ord( 'F' ) then
    begin

      Gra_GLCamera.SceneScale := Gra_GLCamera.SceneScale - kamera_skok_l;
      Key := 0;

    end;
  //---//if Key = Ord( 'F' ) then


  Kamera_Wspolrzedne_Wypisz();

end;//---//Gra_GLSceneViewerKeyDown().

//Gra_GLSceneViewerDblClick().
procedure TWieze_Hanoi.Gra_GLSceneViewerDblClick( Sender : TObject );
begin

  if O_Programie_Panel.Width <> 1 then
    O_Programie_Panel.Width := 1
  else//if O_Programie_Panel.Width <> 1 then
    if forma_glowna.Width > 350 then
      O_Programie_Panel.Width := 300
    else//if forma_glowna.Width > 350 then
      O_Programie_Panel.Width := Ceil( forma_glowna.Width * 0.5 );

end;//---//Gra_GLSceneViewerDblClick().

//Gra_GLSceneViewerMouseMove().
procedure TWieze_Hanoi.Gra_GLSceneViewerMouseMove( Sender : TObject; Shift : TShiftState; X, Y : Integer );
var
  czy_podstawiono_wartosc : boolean;
  i : integer;
  najmniejszy_pozom_wiezy_na_platformie : single;
  zt_gl_scene_object : TGLSceneObject;
begin

  // Wywoływane w ten sposób (Gra_GLSceneViewer.OnMouseMove) za bardzo spowalnia działanie programu.

  zt_gl_scene_object := nil;

  if ( Gra_GLSceneViewer.Buffer.GetPickedObject( x, y ) is TGLSceneObject ) then
    zt_gl_scene_object := ( Gra_GLSceneViewer.Buffer.GetPickedObject( x, y ) as TGLSceneObject );

  Wieza_Poziom_Numer_GLHUDText.Text := '';

  if zt_gl_scene_object <> nil then
    begin

      if zt_gl_scene_object is TGLTorus then
        Wieza_Poziom_Numer_GLHUDText.Text := Trim(  FormatFloat( '### ### ##0', TGLTorus(zt_gl_scene_object).TagFloat )  )
      else//if zt_gl_scene_object is TGLTorus then
        if zt_gl_scene_object is TGLCube then
          begin

            czy_podstawiono_wartosc := false;

            for i := 0 to wieza_elementy_list.Count - 1 do
              if    ( zt_gl_scene_object.Tag = TGLTorus(wieza_elementy_list[ i ]).Tag )
                and (
                         ( not czy_podstawiono_wartosc )
                      or ( najmniejszy_pozom_wiezy_na_platformie > TGLTorus(wieza_elementy_list[ i ]).TagFloat )
                    ) then
                begin

                  najmniejszy_pozom_wiezy_na_platformie := TGLTorus(wieza_elementy_list[ i ]).TagFloat;

                  if not czy_podstawiono_wartosc then
                    czy_podstawiono_wartosc := true;

                end;
              //---//if    ( zt_gl_scene_object.Tag = TGLTorus(wieza_elementy_list[ i ]).Tag )


            //Wieza_Poziom_Numer_GLHUDText.Text := Trim(  FormatFloat( '### ### ##0', TGLTorus(zt_gl_scene_object).Tag )  );

            if czy_podstawiono_wartosc then
              Wieza_Poziom_Numer_GLHUDText.Text := Trim(  FormatFloat( '### ### ##0', najmniejszy_pozom_wiezy_na_platformie )  )
            else//if czy_podstawiono_wartosc then
              Wieza_Poziom_Numer_GLHUDText.Text := '-';

          end;
        //---//if zt_gl_scene_object is TGLCube then

    end;
  //---//if zt_gl_scene_object <> nil then

  Wieza_Poziom_Numer_GLHUDSprite.Visible := Wieza_Poziom_Numer_GLHUDText.Text <> '';

  if Wieza_Poziom_Numer_GLHUDSprite.Visible then
    begin

      Wieza_Poziom_Numer_GLWindowsBitmapFont.EnsureString( Wieza_Poziom_Numer_GLHUDText.Text );
      Wieza_Poziom_Numer_GLHUDSprite.Width := Length( Wieza_Poziom_Numer_GLHUDText.Text ) * 23;
      Wieza_Poziom_Numer_GLHUDSprite.Position.X := 5 + Wieza_Poziom_Numer_GLHUDSprite.Width * 0.5;

    end;
  //---//Wieza_Poziom_Numer_GLHUDSprite.Visible

end;//---//Gra_GLSceneViewerMouseMove().

//Wieza_Poziom_Numer_TimerTimer().
procedure TWieze_Hanoi.Wieza_Poziom_Numer_TimerTimer( Sender : TObject );
var
  mysz_pozycja : GLCrossPlatform.TPoint;
begin

  if DateUtils.MilliSecondsBetween( Now(), wieza_poziom_numer_ostatnie_wywolanie_czas_g ) < 500 then
    Exit;

  //Wieza_Poziom_Numer_Timer.Enabled := false;


  mysz_pozycja := forma_glowna.ScreenToClient( Mouse.CursorPos );

  Gra_GLSceneViewerMouseMove( Sender, [], mysz_pozycja.X, mysz_pozycja.Y );


  //Wieza_Poziom_Numer_Timer.Enabled := true;

  wieza_poziom_numer_ostatnie_wywolanie_czas_g := Now();


  if not GLFireFXManager1.Disabled then
    begin

      GLFireFXManager1.InnerColor.RandomColor();
      GLFireFXManager1.OuterColor.RandomColor();

    end;
  //---//if not GLFireFXManager1.Disabled then

end;//---//Wieza_Poziom_Numer_TimerTimer().

end.
