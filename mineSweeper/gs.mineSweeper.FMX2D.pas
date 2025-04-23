unit gs.mineSweeper.FMX2D;

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
  gs.mineSweeper.FMX, //Interface
  gs.game.mineSweeper; //Logic.

Type
  //Impl.
  TGSMineSweeperGeneratorFMX2D = class(TInterfacedObject, iMineSweeperGenerator)
  private
  protected
    FCurrentGrid : TGridPanelLayout;
    //iMineSweeperGenerator
    procedure Generate(aLogicMineSweeper : iGSLogicalMineSweeper);
  public
    procedure build(aLogicMineSweeper : iGSLogicalMineSweeper;
                    aGridPanelBase : TGridPanelLayout);
  end;

  TGSFMXMineSweeper2D = Class(TGridPanelLayout)
  private
  protected
    FLogicalMineSweeper : iGSLogicalMineSweeper;
    FMineSweeperGenerator : iMineSweeperGenerator;
    procedure internalCommonMineSweeperClickManager(Sender : TObject);
    procedure internalBuild;

    //FLogicalMineSweeper's events.
    procedure internalOnCellEvent(Sender: TObject; X, Y: Integer; Content: TMineSweeperContentItemKind; Value: String);
    procedure internalOnGameOver(Sender: TObject; Victory: Boolean);
    procedure internalRefreshLayout;
  public
    constructor Create(AOwner: TComponent); override;
  published
//    property ColumnCount : Integer read GetCol
  End;

implementation

{ TMineSweeperGeneratorFMX2D }

procedure TGSMineSweeperGeneratorFMX2D.build(
                aLogicMineSweeper : iGSLogicalMineSweeper;
                aGridPanelBase : TGridPanelLayout);
begin
  assert(Assigned(aGridPanelBase));
  assert(Assigned(aLogicMineSweeper));
  FCurrentGrid := aGridPanelBase;
  Generate(aLogicMineSweeper);
end;

procedure TGSMineSweeperGeneratorFMX2D.Generate(aLogicMineSweeper : iGSLogicalMineSweeper);
begin
  Assert(assigned(aLogicMineSweeper));
  Assert(Assigned(FCurrentGrid));

  FCurrentGrid.HitTest := false;

  FCurrentGrid.RowCollection.BeginUpdate;
  FCurrentGrid.ColumnCollection.BeginUpdate;
  try

    FCurrentGrid.RowCollection.Clear;
    FCurrentGrid.ColumnCollection.Clear;

    for var i := 0 to aLogicMineSweeper.getWidth-1 do begin
      var lc := FCurrentGrid.ColumnCollection.add;
      lc.SizeStyle := TGridPanelLayout.TSizeStyle.Percent;
      lc.Value := FCurrentGrid.Width/aLogicMineSweeper.getWidth;
    end;

    for var i := 0 to aLogicMineSweeper.getHeight-1 do begin
      var lr := FCurrentGrid.RowCollection.add;
      lr.SizeStyle := TGridPanelLayout.TSizeStyle.Percent;
      lr.Value := FCurrentGrid.Height/aLogicMineSweeper.getHeight;
    end;

    for var i := 0 to aLogicMineSweeper.getWidth-1 do
      for var j := 0 to aLogicMineSweeper.getHeight-1 do begin
         var aButton := TButton.Create(FCurrentGrid);
         aButton.Parent := FCurrentGrid; //magic: place in the next empty cell
         aButton.Visible := true;
         aButton.Align := TAlignLayout.Client;
         aButton.Margins.Create(Rectf(5,5,5,5));
         aButton.StyledSettings := aButton.StyledSettings - [TStyledSetting.Family];
         aButton.StyledSettings := aButton.StyledSettings - [TStyledSetting.Size];
         aButton.StyledSettings := aButton.StyledSettings - [TStyledSetting.FontColor];
         aButton.TextSettings.Font.Family := 'Font Awesome 6 Free Solid';
         aButton.TextSettings.Font.Size := aButton.TextSettings.Font.Size*4;
         aButton.Text := 'Btn ' + IntToStr(i);
      end;

  finally
    FCurrentGrid.RowCollection.EndUpdate;
    FCurrentGrid.ColumnCollection.EndUpdate;
  end;

end;

{ TGSFMXMineSweeper2D }

constructor TGSFMXMineSweeper2D.Create(AOwner: TComponent);
begin
  inherited;
  FLogicalMineSweeper := TGSMineSweeper.Create( DefaultSideAndMineCount*2,
                                                DefaultSideAndMineCount*2,
                                                DefaultSideAndMineCount);
  TGSMineSweeper(FLogicalMineSweeper).OnCellEvent := internalOnCellEvent;
  TGSMineSweeper(FLogicalMineSweeper).OnGameOver := internalOnGameOver;
  FMineSweeperGenerator := TGSMineSweeperGeneratorFMX2D.Create;
  internalBuild;
end;

procedure TGSFMXMineSweeper2D.internalBuild;
begin
  Assert(Assigned(FMineSweeperGenerator));
  Assert(Assigned(FLogicalMineSweeper));
  TGSMineSweeperGeneratorFMX2D(FMineSweeperGenerator).build(FLogicalMineSweeper,Self);
  //Assign event
  for var i := 0 to controls.Count-1 do
    if Controls.Items[i] is TButton then begin
      TButton(Controls.Items[i]).OnClick := internalCommonMineSweeperClickManager;
    end;
  internalRefreshLayout;
end;

procedure TGSFMXMineSweeper2D.internalCommonMineSweeperClickManager(
  Sender: TObject);
begin
  Assert(Sender is TButton);

  //Button
  var lb := TButton(Sender);

  //Button index in grid panel.
  var lindex := ControlCollection.IndexOf(lb);
  //Control (Button's owner) index.
  var lcontrolItem := ControlCollection.Items[lindex];

  FLogicalMineSweeper.RevealAt(lcontrolItem.Column,lcontrolItem.Row);
end;

procedure TGSFMXMineSweeper2D.internalOnCellEvent(Sender: TObject; X,
  Y: Integer; Content: TMineSweeperContentItemKind; Value: String);
begin
//  ShowMessage(format('%d %d %s',[X,Y,value]));
  var ci := ControlCollection.Controls[X,Y];
  var lval := FLogicalMineSweeper.GetCellValue(X,Y);
  var lState := FLogicalMineSweeper.GetCellState(X,Y);
  var lkind := FLogicalMineSweeper.GetCellContentKind(X,Y);


  if lState = TMineSweeperCellState.csRevealed then begin
    case lkind of
      TMineSweeperContentItemKind.emptyCell : begin
        TButton(ci).TextSettings.Font.Size := 1;
        TButton(ci).TextSettings.FontColor := TAlphaColorRec.Green;
        TButton(ci).AnimateFloat('TextSettings.Font.Size',TButton(ci).TextSettings.Font.Size*20,0.3,TAnimationType.InOut,TInterpolationType.Bounce);
        Randomize;
        case random(10) of
          0: TButton(ci).Text := 'poo';
          1: TButton(ci).Text := 'check';
          2: TButton(ci).Text := 'thumbs-up';
          3: TButton(ci).Text := 'wand-magic-sparkles';
          4: TButton(ci).Text := 'hippo';
          5: TButton(ci).Text := 'ghost';
          6: TButton(ci).Text := 'mug-hot';
          7: TButton(ci).Text := 'bug';
          8: TButton(ci).Text := 'gamepad';
          9: TButton(ci).Text := 'fish';
        end;
      end;
      TMineSweeperContentItemKind.CountingCell : begin
        TButton(ci).TextSettings.Font.Size := 1;
        TButton(ci).TextSettings.FontColor := TAlphaColorRec.Blue;
        TButton(ci).Text := lval;
        TButton(ci).AnimateFloat('TextSettings.Font.Size',TButton(ci).TextSettings.Font.Size*20,0.5,TAnimationType.InOut);
      end;
      TMineSweeperContentItemKind.MineCell : begin
        TButton(ci).Text := 'bomb';
        var ls := TButton(ci).TextSettings.Font.Size;
        TButton(ci).TextSettings.Font.Size := TButton(ci).TextSettings.Font.Size*10;
        TButton(ci).AnimateFloat('TextSettings.Font.Size',ls,0.3,TAnimationType.Out,TInterpolationType.Bounce);
        TButton(ci).AnimateColor('TextSettings.FontColor',TAlphaColorRec.Red,1,TAnimationType.InOut);
      end;
    end;
  end;

end;

procedure TGSFMXMineSweeper2D.internalOnGameOver(Sender: TObject;
  Victory: Boolean);
begin
  var lmessage := 'YOU WIN';
  if Not Victory then begin
    lmessage := 'YOU LOOSE';
  end;
  showMessage(lmessage+' - GAME OVER');
  FLogicalMineSweeper.RevealAllMines;
end;

procedure TGSFMXMineSweeper2D.internalRefreshLayout;
begin
  Assert(FLogicalMineSweeper.getWidth=ColumnCollection.Count);
  Assert(FLogicalMineSweeper.getheight=RowCollection.Count);

  for var X := 0 to FLogicalMineSweeper.getWidth - 1 do
  begin
    for var Y := 0 to FLogicalMineSweeper.getHeight - 1 do
    begin
      var FCellState := FLogicalMineSweeper.GetCellState(X, Y);
      var ci := ControlCollection.Controls[X,Y];

      ///...
      var lval := FLogicalMineSweeper.GetCellValue(X,Y);
      if FCellState = TMineSweeperCellState.csRevealed then begin
        if lval = 'M' then
          TButton(ci).Text := 'bomb';
      end
      else begin
        TButton(ci).Text := 'smile'; //FLogicalMineSweeper.GetCellValue(X,Y);
      end;

    end;
  end;
end;

end.
