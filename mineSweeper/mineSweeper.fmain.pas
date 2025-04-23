unit mineSweeper.fmain;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Math.Vectors,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.Layouts,
  FMX.StdCtrls,
  FMX.Controls.Presentation,
  FMX.Viewport3D,
  FMX.Ani,
  FMX.Objects,
  FMX.Controls3D,
  FMX.Objects3D,
  FMX.Types3D,
  FMX.MaterialSources,
  Execute.TransparentTexture,
  FMX.Edit,
  FMX.EditBox,
  FMX.NumberBox,
  gs.game.mineSweeper,
  gs.mineSweeper.FMX2D,
  gs.mineSweeper.FMX3D;

type
  TForm63 = class(TForm)
    Viewport3D1: TViewport3D;
    Camera1: TCamera;
    LightMaterialSource1: TLightMaterialSource;
    Light1: TLight;
    FloatAnimation8: TFloatAnimation;
    FloatAnimation9: TFloatAnimation;
    Selection2: TSelection;
    Rectangle1: TRectangle;
    Rectangle2: TRectangle;
    NumberBox1: TNumberBox;
    NumberBox2: TNumberBox;
    NumberBox3: TNumberBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Selection3: TSelection;
    Rectangle3: TRectangle;
    Label5: TLabel;
    Rectangle4: TRectangle;
    Button2: TButton;
    Button1: TButton;
    DummyXY: TDummy;
    DummyXZ: TDummy;
    SpeedButton1: TSpeedButton;
    Rectangle5: TRectangle;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Viewport3D1MouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; var Handled: Boolean);
    procedure Viewport3D1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Single);
    procedure FormCreate(Sender: TObject);
    procedure Viewport3D1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure SpeedButton1Click(Sender: TObject);
  private
    FGameOver : Boolean; //Prevent multi dialog display.
    FDM : TPoint3D; //Mouse Down Memory.
    FDUM : TPoint3D; //Dummy's Memory.
    { Private declarations }
    procedure OnWinGame(sender : TObject);
    procedure OnLostGame(sender : TObject);
  public
    { Public declarations }
    mineSweeper3d : TGSFMXMineSweeper3D;
  end;

var
  Form63: TForm63;

implementation

{$R *.fmx}

procedure TForm63.Button1Click(Sender: TObject);
begin
  //Create a selection.
  var l := TSelection.Create(Self);
  AddObject(l);
  l.SetBounds(100,100,500,500);

  //get fully functional 2d FMX control.
  var lc := TGSFMXMineSweeper2D.Create(Self);
  //bound it in our TSelection.
  l.AddObject(lc);
  lc.Margins.Create(Rectf(2,2,2,2));
  lc.Align := TAlignLayout.Client;
end;

procedure TForm63.Button2Click(Sender: TObject);
begin
  FGameOver := False;
  if Assigned(mineSweeper3d) then
    Viewport3D1.RemoveObject(mineSweeper3d);
  //get fully functional 3d FMX control.
  mineSweeper3d := TGSFMXMineSweeper3D.Create(Self);
  mineSweeper3d.MaterialUnknown := TLightMaterialSource.Create(Self);
  mineSweeper3d.MaterialOdd := TLightMaterialSource.Create(Self);
  TLightMaterialSource(mineSweeper3d.MaterialUnknown).Texture.LoadFromFile('..\..\..\mineSweeperGame\Assets\captain_icon_64x64.png');
  TLightMaterialSource(mineSweeper3d.MaterialOdd).Texture.LoadFromFile('..\..\..\mineSweeperGame\Assets\mine 64x64 red.png');

  mineSweeper3d.addMaterialForNumber('..\..\..\mineSweeperGame\Assets\mine 64x64 1.png'); //First  -> 1
  mineSweeper3d.addMaterialForNumber('..\..\..\mineSweeperGame\Assets\mine 64x64 2.png'); //Second -> 2
  mineSweeper3d.addMaterialForNumber('..\..\..\mineSweeperGame\Assets\mine 64x64 3.png'); //third  -> 3
  mineSweeper3d.addMaterialForNumber('..\..\..\mineSweeperGame\Assets\mine 64x64 4.png'); //and son on until 8.
  mineSweeper3d.addMaterialForNumber('..\..\..\mineSweeperGame\Assets\mine 64x64 5.png');
  mineSweeper3d.addMaterialForNumber('..\..\..\mineSweeperGame\Assets\mine 64x64 6.png');
  mineSweeper3d.addMaterialForNumber('..\..\..\mineSweeperGame\Assets\mine 64x64 7.png');
  mineSweeper3d.addMaterialForNumber('..\..\..\mineSweeperGame\Assets\mine 64x64 8.png');
  mineSweeper3d.addMaterialForNumber('..\..\..\mineSweeperGame\Assets\wave-1.png'); //Different Wave style for "uknown texture"
  mineSweeper3d.addMaterialForNumber('..\..\..\mineSweeperGame\Assets\wave-2.png');
  mineSweeper3d.addMaterialForNumber('..\..\..\mineSweeperGame\Assets\wave-3.png');

  mineSweeper3d.OnGameWin := OnWinGame;
  mineSweeper3d.OnGameLost := OnLostGame;

  //bound it in our TSelection.
  Viewport3D1.AddObject(mineSweeper3d);
  Viewport3D1.Repaint;

  Selection2.Visible := true;
end;

procedure TForm63.FormCreate(Sender: TObject);
begin
  Selection2.Visible := False;
end;

procedure TForm63.OnLostGame(sender: TObject);
begin
  if not(FGameOver) then begin
    FGameOver := true;
    ShowMessage('LOST')
  end;
end;

procedure TForm63.OnWinGame(sender: TObject);
begin
  if not(FGameOver) then begin
    FGameOver := true;
    ShowMessage('WIN !');
  end;
end;

procedure TForm63.SpeedButton1Click(Sender: TObject);
begin
  Button2Click(Sender);
  mineSweeper3d.ColumnCount := Round(NumberBox1.Value);
  mineSweeper3d.RowCount := round(NumberBox2.Value);
  mineSweeper3d.MineCount := round(NumberBox3.Value);
end;

procedure TForm63.Viewport3D1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  FDM.X := X;
  FDM.Y := Y;
  FDUM.X := DummyXZ.RotationAngle.x;
  FDUM.Y := DummyXY.RotationAngle.y;
end;

procedure TForm63.Viewport3D1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Single);
begin
  if TShiftStateItem.ssLeft in Shift then begin
      DummyXY.RotationAngle.y := FDUM.Y - (FDM.X - x);
      DummyXZ.RotationAngle.x := FDUM.X + (FDM.Y - Y);
  end;
end;

procedure TForm63.Viewport3D1MouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; var Handled: Boolean);
begin
  Camera1.Position.Z := Camera1.Position.Z + WheelDelta/50;
end;

end.
