unit gs.mineSweeper.FMX3D;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Math.Vectors,
  System.Generics.Collections,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Layouts,
  FMX.StdCtrls,
  FMX.Controls.Presentation,
  FMX.Ani,
  FMX.Viewport3D,
  FMX.Objects3D,
  FMX.MaterialSources,
  FMX.Controls3D,
  Execute.TransparentTexture,
  gs.mineSweeper.FMX, //Interface
  gs.game.mineSweeper; //Logic.

Type
  //Impl.
  TGSMineSweeperGeneratorFMX3D = class(TInterfacedObject, iMineSweeperGenerator)
  private
    //Entry point of viewport. All components will be childs of it.
    FEntry : TControl3D;
    //Ref FROM childs controls TO coords "grid" points.
    FWidgetRef : TDictionary<TControl3d,TPoint>;
    //Ref FROM coords "Grid" points TO Controls. (Yes, it is a string, but we do not need more here)
    FWidgetPos : TDictionary<String,TControl3d>;
  protected
    //iMineSweeperGenerator
    procedure Generate(aLogicMineSweeper : iGSLogicalMineSweeper);
  public
    Constructor Create; virtual;
    Destructor Destroy; Override;

    //AlogicMineSweeper is Mine Sweeper engine (pureliy not graphical, logic only).
    //aEntry : a control which be the parent en our grid.
    procedure build(aLogicMineSweeper : iGSLogicalMineSweeper;
                    aEntry : TControl3D);
  end;


  TGSFMXMineSweeper3D = Class(TDummy)
  private
    FMaterialUnknown: TMaterialSource;
    FMaterialOdd: TMaterialSource;
    FMaterialsNumbers : Array of TLightMaterialSource;
    FOnGameLost: TNotifyEvent;
    FOnGameWin: TNotifyEvent;
    procedure SetMaterialUnknown(const Value: TMaterialSource);

    function GetColCount: Integer;
    procedure SetColCount(const Value: Integer);
    function GetRowCount: Integer;
    procedure SetRowCount(const Value: Integer);
    function GetMinecount: integer;
    procedure SetMineCount(const Value: integer);
  protected
    FLogicalMineSweeper : iGSLogicalMineSweeper;
    FMineSweeperGenerator : iMineSweeperGenerator;

    procedure internalBuild(colcount,rowcount,mineCount : Integer);

    procedure internalCommonMineSweeperClickManager(Sender : TObject);

    //FLogicalMineSweeper's events.
    procedure internalOnCellEvent(Sender: TObject; X, Y: Integer; Content: TMineSweeperContentItemKind; Value: String);
    procedure internalOnGameOver(Sender: TObject; Victory: Boolean);
  public
    constructor Create(AOwner: TComponent); override;

    procedure addMaterialForNumber(filename : String);

  published
    property MaterialUnknown : TMaterialSource read FMaterialUnknown write SetMaterialUnknown;
    property MaterialOdd : TMaterialSource read FMaterialOdd write FMaterialOdd;

    property OnGameWin : TNotifyEvent read FOnGameWin write FOnGameWin;
    property OnGameLost : TNotifyEvent read FOnGameLost write FOnGameLost;

    property ColumnCount : Integer read GetColCount write SetColCount;
    property RowCount : Integer read GetRowCount write SetRowCount;
    property MineCount : integer read GetMinecount write SetMineCount;
  End;


implementation

{ TGSMineSweeperGeneratorFMX3D }

procedure TGSMineSweeperGeneratorFMX3D.build(
  aLogicMineSweeper: iGSLogicalMineSweeper; aEntry: TControl3D);
begin
  Assert(Assigned(aEntry));
  Assert(assigned(aLogicMineSweeper));
  FEntry := aEntry;
  FEntry.DeleteChildren;
  Generate(aLogicMineSweeper);
end;

constructor TGSMineSweeperGeneratorFMX3D.Create;
begin
  inherited;
  FWidgetRef := TDictionary<TControl3D,TPoint>.Create;
  FWidgetPos := TDictionary<String,TControl3d>.Create;
end;

destructor TGSMineSweeperGeneratorFMX3D.Destroy;
begin
  FreeAnDnil(FWidgetRef);
  FreeAnDnil(FWidgetPos);
  inherited;
end;

procedure TGSMineSweeperGeneratorFMX3D.Generate(
  aLogicMineSweeper: iGSLogicalMineSweeper);
begin
  var cl := TCamera.Create(FEntry);
//  Viewport3D1.AddObject(cl); //Change owner;

//  Viewport3D1.BeginUpdate;
  var langle := 4.0;
  var lDist := 10;
  var lScale := 0.5;

  var lposW := aLogicMineSweeper.getWidth/2;
  var lposH := aLogicMineSweeper.GetHeight/2;
  FWidgetRef.Clear;

  for var i := 0 to aLogicMineSweeper.getWidth-1 do
    for var j := 0 to aLogicMineSweeper.getHeight-1 do begin
      var l := TPlane.Create(FEntry);
      FWidgetRef.Add(l,Point(i,j)); //Associate.
      FWidgetPos.Add(format('%d|%d',[i,j]),l); //Associate.
      l.SubdivisionsHeight := 2;
      l.SubdivisionsWidth := 2;

      //Animation for mouse overing.
      var lf := TFloatAnimation.Create(nil);
      lf.PropertyName := 'Scale.X';
      lf.Trigger := 'IsMouseOver=true';
      lf.TriggerInverse := 'IsMouseOver=false';
      lf.StartValue := lScale;
      lf.StopValue := lScale*2;
      lf.Enabled := true;
      l.AddObject(lf);
      lf := TFloatAnimation.Create(nil);
      lf.PropertyName := 'Scale.Y';
      lf.Trigger := 'IsMouseOver=true';
      lf.TriggerInverse := 'IsMouseOver=false';
      lf.StartValue := lScale;
      lf.StopValue := lScale*2;
      lf.Enabled := true;
      l.AddObject(lf);

      //Animation for revealing.
      lf := TFloatAnimation.Create(nil);
      lf.PropertyName := 'RotationAngle.Y';
      lf.StartValue := 0;
      lf.StopValue := 360;
      lf.Duration := 0.5;
      lf.Enabled := False;
      l.AddObject(lf);
      l.TagObject := lf; //more easy to spot.


      l.Scale.Point := Point3D(lScale,lScale,lScale);
      cl.ResetRotationAngle;
      //Object will pass to camera transformation model.
      cl.AddObject(l);
      l.Position.z := lDist;
      cl.RotationAngle.Point := Point3D(langle*(j-lposH),langle*(i-lposW),0);
      var lp := l.AbsolutePosition; //get position from camera model to absolute one.
      var lr := cl.RotationAngle.Point;
      FEntry.AddObject(l); //Change owner;
      l.Position.Point := Tpoint3d(lp); //Reapply absolute position.
      l.RotationAngle.Point := lr; //-lr is funny too.
    end;
end;

{ TGSFMXMineSweeper3D }

procedure TGSFMXMineSweeper3D.addMaterialForNumber(filename: String);
begin
  assert(FileExists(filename));
  var lindex := length(FMaterialsNumbers);
  SetLength(FMaterialsNumbers,lindex+1);
  FMaterialsNumbers[lindex] := TLightMaterialSource.Create(Self);
  FMaterialsNumbers[lindex].Texture.LoadFromFile(filename);
end;


constructor TGSFMXMineSweeper3D.Create(AOwner: TComponent);
begin
  inherited;
  internalBuild(DefaultSideAndMineCount*2,DefaultSideAndMineCount*2,DefaultSideAndMineCount);
end;


function TGSFMXMineSweeper3D.GetColCount: Integer;
begin
  result := FLogicalMineSweeper.GetWidth;
end;

function TGSFMXMineSweeper3D.GetMinecount: integer;
begin
  result := FLogicalMineSweeper.GetMineCount;
end;

function TGSFMXMineSweeper3D.GetRowCount: Integer;
begin
  result := FLogicalMineSweeper.GetHeight;
end;

procedure TGSFMXMineSweeper3D.internalBuild(colcount,rowcount,mineCount : Integer);
begin
  assert(colcount>0);
  assert(rowCount>0);
  assert(minecount>-1);

  FLogicalMineSweeper := nil;  //interface based.
  FMineSweeperGenerator := nil;

  FLogicalMineSweeper := TGSMineSweeper.Create( colCount,
                                                rowcount,
                                                mineCount);
  TGSMineSweeper(FLogicalMineSweeper).OnCellEvent := internalOnCellEvent;
  TGSMineSweeper(FLogicalMineSweeper).OnGameOver := internalOnGameOver;
  FMineSweeperGenerator := TGSMineSweeperGeneratorFMX3D.Create;

  TGSMineSweeperGeneratorFMX3D(FMineSweeperGenerator).build(FLogicalMineSweeper,Self);

  var ll := TGSMineSweeperGeneratorFMX3D(FMineSweeperGenerator).FWidgetRef;

  //Assign event
  for var l in ll do
    l.Key.OnClick := internalCommonMineSweeperClickManager;

  //(re)Assign material
  SetMaterialUnknown(FMaterialUnknown);
end;

procedure TGSFMXMineSweeper3D.internalCommonMineSweeperClickManager(
  Sender: TObject);
begin
  Assert(Sender is TPlane);

  var lp := TPlane(Sender);
  var aPoint : TPoint;
  TGSMineSweeperGeneratorFMX3D(FMineSweeperGenerator).FWidgetRef.TryGetValue(lp,aPoint);

  FLogicalMineSweeper.RevealAt(aPoint.X,aPoint.Y);
end;

procedure TGSFMXMineSweeper3D.internalOnCellEvent(Sender: TObject; X,
  Y: Integer; Content: TMineSweeperContentItemKind; Value: String);
begin
  var lControl : TControl3D;
  TGSMineSweeperGeneratorFMX3D(FMineSweeperGenerator).FWidgetPos.TryGetValue(format('%d|%d',[X,Y]),lcontrol);

  Assert(lControl is TPlane);

  //Play an animation for reveal nicely the selected plane.
  //note : Do better for find attached animation ;)
  TFloatAnimation(lcontrol.TagObject).Delay := X/ 10; //Column delayed.
  TFloatAnimation(lcontrol.TagObject).Enabled := true;

  case Content of
    emptyCell: begin
      TPlane(lControl).MaterialSource := FMaterialsNumbers[8+Random(3)];
    end;
    CountingCell: begin
      var lb := Value.ToInteger;
      assert(length(FMaterialsNumbers)>=lb);
      TPlane(lControl).MaterialSource := FMaterialsNumbers[lb-1];
    end;
    MineCell: TPlane(lControl).MaterialSource := MaterialOdd;
  end;


end;

procedure TGSFMXMineSweeper3D.internalOnGameOver(Sender: TObject;
  Victory: Boolean);
begin
  if Victory then begin
    if Assigned(FOnGameWin) then
      FOnGameWin(self);
  end
  else
    if Assigned(FOnGameLost) then
      FOnGameLost(self);
end;


procedure TGSFMXMineSweeper3D.SetColCount(const Value: Integer);
begin
  var lcol := FLogicalMineSweeper.GetWidth;
  if lcol<>value then begin
    var lrow := FLogicalMineSweeper.GetHeight;
    var lmcount := FLogicalMineSweeper.GetMineCount;
    internalBuild(value,lrow,lmcount);
  end;
end;

procedure TGSFMXMineSweeper3D.SetMaterialUnknown(const Value: TMaterialSource);
begin
  FMaterialUnknown := Value;
  for var ref in TGSMineSweeperGeneratorFMX3D(FMineSweeperGenerator).FWidgetRef do
    if ref.Key is TPlane then
      TPlane(ref.Key).MaterialSource := FMaterialUnknown;
end;

procedure TGSFMXMineSweeper3D.SetMineCount(const Value: integer);
begin
  var lmcount := FLogicalMineSweeper.GetMineCount;
  if lmcount<>value then begin
    var lcol := FLogicalMineSweeper.GetWidth;
    var lrow := FLogicalMineSweeper.GetHeight;
    internalBuild(lcol,lrow,value);
  end;
end;

procedure TGSFMXMineSweeper3D.SetRowCount(const Value: Integer);
begin
  var lrow := FLogicalMineSweeper.GetHeight;
  if lrow<>value then begin
    var lcol := FLogicalMineSweeper.GetWidth;
    var lmcount := FLogicalMineSweeper.GetMineCount;
    internalBuild(lcol,value,lmcount);
  end;
end;

end.
