unit gsMineSweeperGame.fmain;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  System.Math.Vectors,
  FMX.MaterialSources,
  FMX.Objects3D,
  FMX.Controls3D,
  FMX.Viewport3D,
  FMX.Types3D,
  FMX.Ani,
  FMX.Media,
  FMX.Controls.Presentation,
  FMX.StdCtrls,
  FMX.Objects,
  Execute.TransparentTexture,
  gs.game.mineSweeper,
  gs.mineSweeper.FMX,
  gs.mineSweeper.FMX3D;

type
  TForm64 = class(TForm)
    Viewport3D1: TViewport3D;
    Camera1: TCamera;
    SphereInner: TSphere;
    FloatAnimation1: TFloatAnimation;
    FloatAnimation2: TFloatAnimation;
    Plane2: TPlane;
    Plane1: TPlane;
    Light1: TLight;
    FloatAnimation3: TFloatAnimation;
    FloatAnimation4: TFloatAnimation;
    FloatAnimation5: TFloatAnimation;
    FloatAnimation_CoolZoomLanding: TFloatAnimation;
    Light2: TLight;
    MediaPlayer1: TMediaPlayer;
    FloatAnimation7: TFloatAnimation;
    Plane_ClickToContinue: TPlane;
    FloatAnimation_ScaleX: TFloatAnimation;
    FloatAnimation_ScaleY: TFloatAnimation;
    FloatAnimation_Appear: TFloatAnimation;
    Dummy_Scene1: TDummy;
    FloatAnimationGoBackward_X: TFloatAnimation;
    FloatAnimationGoBackward_Z: TFloatAnimation;
    FloatAnimation_Disappear: TFloatAnimation;
    Dummy_SceneMenu: TDummy;
    Plane_Menu_Play: TPlane;
    FloatAnimation_SceneMenu_appear: TFloatAnimation;
    FloatAnimation8: TFloatAnimation;
    FloatAnimation9: TFloatAnimation;
    Plane_Menu_Options: TPlane;
    FloatAnimation10: TFloatAnimation;
    FloatAnimation11: TFloatAnimation;
    Plane_Menu_Quit: TPlane;
    FloatAnimation12: TFloatAnimation;
    FloatAnimation13: TFloatAnimation;
    Plane_Menu_Credits: TPlane;
    FloatAnimation14: TFloatAnimation;
    FloatAnimation15: TFloatAnimation;
    Dummy_AllScenes1And2: TDummy;
    Selection1: TSelection;
    Button_QuitToMenu: TButton;
    FloatAnimation_allScene_GoAway: TFloatAnimation;
    FloatAnimation_allScene_ComeBack: TFloatAnimation;
    TextureMaterialSource1: TTextureMaterialSource;
    TextureMaterialSource2: TTextureMaterialSource;
    TextureMaterialSource3: TTextureMaterialSource;
    procedure Viewport3D1MouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; var Handled: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure Plane_ClickToContinueClick(Sender: TObject);
    procedure FloatAnimation_DisappearFinish(Sender: TObject);
    procedure Plane_Menu_QuitClick(Sender: TObject);
    procedure Plane_Menu_PlayClick(Sender: TObject);
    procedure Button_QuitToMenuClick(Sender: TObject);
  private
    { Private declarations }
    procedure internalDeployGame;
  public

    { Public declarations }
    MaterialBackground,
    MaterialTitle,
    MaterialOhCaptainMyCaptain,
    MaterialClickToContinue : TLightMaterialSource;

    Material_Menu_Play,
    Material_Menu_Options,
    Material_Menu_Credits,
    Material_Menu_Quit : TLightMaterialSource;

    mineSweeper3d : TGSFMXMineSweeper3D;

    procedure OnWinGame(sender : TObject);
    procedure OnLostGame(sender : TObject);

  end;

var
  Form64: TForm64;

implementation


{$R *.fmx}

procedure TForm64.FloatAnimation_DisappearFinish(Sender: TObject);
begin
  Plane_ClickToContinue.Visible := true;
end;

procedure TForm64.FormCreate(Sender: TObject);
begin
  Selection1.Visible := false;
  Selection1.Enabled := false;

  MaterialBackground := TLightMaterialSource.Create(Self);
  MaterialTitle := TLightMaterialSource.Create(Self);
  MaterialOhCaptainMyCaptain := TLightMaterialSource.Create(Self);
  MaterialClickToContinue := TLightMaterialSource.Create(Self);


  MaterialBackground.Texture.LoadFromFile('..\..\Assets\Mine sweeper color backround 3.png');

  MaterialTitle.Texture.LoadFromFile('..\..\Assets\Game Title.png');

  MaterialClickToContinue.Texture.LoadFromFile('..\..\Assets\Mine sweeper ClickToContinueBig.png');

  MaterialOhCaptainMyCaptain.Texture.LoadFromFile('..\..\Assets\Mine sweeper Mascotte 1.png');


  //Connect materials : landing page.
  SphereInner.MaterialSource := MaterialBackground;
  Plane1.MaterialSource := MaterialTitle;
  Plane2.MaterialSource := MaterialOhCaptainMyCaptain;
  Plane_ClickToContinue.MaterialSource := MaterialClickToContinue;

  //Materials : Game Menu.
  Material_Menu_Play := TLightMaterialSource.Create(Self);
  Material_Menu_Play.Texture.LoadFromFile('..\..\Assets\Mine sweeper Menu 1 - PLAY.png');
  Material_Menu_Options := TLightMaterialSource.Create(Self);
  Material_Menu_Options.Texture.LoadFromFile('..\..\Assets\Mine sweeper Menu 1 - OPTIONS.png');
  Material_Menu_Quit := TLightMaterialSource.Create(Self);
  Material_Menu_Quit.Texture.LoadFromFile('..\..\Assets\Mine sweeper Menu 1 - QUIT.png');
  Material_Menu_Credits := TLightMaterialSource.Create(Self);
  Material_Menu_Credits.Texture.LoadFromFile('..\..\Assets\Mine sweeper Menu 1 - CREDITS.png');

  Plane_Menu_Play.MaterialSource := Material_Menu_Play;
  Plane_Menu_Options.MaterialSource := Material_Menu_Options;
  Plane_Menu_Quit.MaterialSource := Material_Menu_Quit;
  Plane_Menu_Credits.MaterialSource := Material_Menu_Credits;

  SphereInner.Opacity := 0; //Could be made in property, but it became invisible.
  Plane2.Opacity := 0;
  Plane1.Opacity := 0;
  Plane_ClickToContinue.Opacity := 0;
  Dummy_SceneMenu.Position.Z := -100;
  Dummy_SceneMenu.Visible := false;

  //Music !
  MediaPlayer1.FileName := '..\..\Assets\steampunk-pirates-289789.mp3';
  MediaPlayer1.Volume := 0;
  MediaPlayer1.Play;
end;

procedure TForm64.internalDeployGame;
begin
  //get fully functional 3d FMX control.
  mineSweeper3d := TGSFMXMineSweeper3D.Create(Self);
  mineSweeper3d.MaterialUnknown := TLightMaterialSource.Create(Self); //TextureMaterialSource3;
  mineSweeper3d.MaterialOdd := TLightMaterialSource.Create(Self); //TextureMaterialSource1;

  TLightMaterialSource(mineSweeper3d.MaterialUnknown).Texture.LoadFromFile('..\..\..\mineSweeperGame\Assets\captain_icon_64x64.png');
  TLightMaterialSource(mineSweeper3d.MaterialOdd).Texture.LoadFromFile('..\..\..\mineSweeperGame\Assets\mine 64x64 red.png');

  mineSweeper3d.addMaterialForNumber('..\..\..\mineSweeperGame\Assets\mine 64x64 1.png'); //First  -> 1
  mineSweeper3d.addMaterialForNumber('..\..\..\mineSweeperGame\Assets\mine 64x64 2.png'); //Second -> 2
  mineSweeper3d.addMaterialForNumber('..\..\..\mineSweeperGame\Assets\mine 64x64 3.png'); //third  -> 3
  mineSweeper3d.addMaterialForNumber('..\..\..\mineSweeperGame\Assets\mine 64x64 4.png'); //and so on on until 8.
  mineSweeper3d.addMaterialForNumber('..\..\..\mineSweeperGame\Assets\mine 64x64 5.png');
  mineSweeper3d.addMaterialForNumber('..\..\..\mineSweeperGame\Assets\mine 64x64 6.png');
  mineSweeper3d.addMaterialForNumber('..\..\..\mineSweeperGame\Assets\mine 64x64 7.png');
  mineSweeper3d.addMaterialForNumber('..\..\..\mineSweeperGame\Assets\mine 64x64 8.png');
  mineSweeper3d.addMaterialForNumber('..\..\..\mineSweeperGame\Assets\wave-1.png'); //Different Wave style for "uknown texture"
  mineSweeper3d.addMaterialForNumber('..\..\..\mineSweeperGame\Assets\wave-2.png');
  mineSweeper3d.addMaterialForNumber('..\..\..\mineSweeperGame\Assets\wave-3.png');

  mineSweeper3d.OnGameWin := OnWinGame;
  mineSweeper3d.OnGameLost := OnLostGame;

  mineSweeper3d.Opacity := 0;

  //bound it in our TSelection.
  Viewport3D1.AddObject(mineSweeper3d);
  Viewport3D1.Repaint;

  TAnimator.AnimateFloat(mineSweeper3d,'Opacity',1,2.5);
  TAnimator.AnimateFloat(mineSweeper3d,'RotationAngle.Y',360,2);
  TAnimator.AnimateFloat(Camera1,'Position.z',-6);
end;

procedure TForm64.OnLostGame(sender: TObject);
begin
  ShowMessage('YO HO HO ! Lost !');
  Button_QuitToMenuClick(sender);
end;

procedure TForm64.OnWinGame(sender: TObject);
begin
  ShowMessage('YO HA HA ! Win !');
  Button_QuitToMenuClick(sender);
end;

procedure TForm64.Plane_ClickToContinueClick(Sender: TObject);
begin
  Dummy_SceneMenu.Visible := true;
  Plane_ClickToContinue.OnClick := nil;
  FloatAnimationGoBackward_X.Enabled := true;
  FloatAnimationGoBackward_Z.Enabled := true;
  FloatAnimation_Disappear.Enabled := true;
  FloatAnimation_SceneMenu_appear.Enabled := true;
end;

procedure TForm64.Plane_Menu_PlayClick(Sender: TObject);
begin
  FloatAnimation_allScene_GoAway.Enabled := false;
  FloatAnimation_allScene_GoAway.Enabled := true;
  Selection1.Visible := true;
  Selection1.Enabled := true;
  internalDeployGame;
end;

procedure TForm64.Button_QuitToMenuClick(Sender: TObject);
begin
  Viewport3D1.RemoveObject(mineSweeper3d);
  FloatAnimation_allScene_ComeBack.Enabled := false;
  FloatAnimation_allScene_ComeBack.Enabled := true;
  Selection1.Visible := false;
  Selection1.Enabled := false;
  TAnimator.AnimateFloat(Camera1,'Position.z',-18);
end;

procedure TForm64.Plane_Menu_QuitClick(Sender: TObject);
begin
  Close;
end;

procedure TForm64.Viewport3D1MouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; var Handled: Boolean);
begin
  {$IFDEF DEBUG}
  Camera1.Position.Z := Camera1.Position.Z + WheelDelta/10;
  Caption := Camera1.Position.Z.ToString;
//  Light2.Position.Z := Light2.Position.Z + WheelDelta/5;
//  Caption := Light2.Position.Z.ToString;
  {$ENDIF}
end;

end.
