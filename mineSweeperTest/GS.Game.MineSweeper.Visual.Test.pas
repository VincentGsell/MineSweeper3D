unit GS.Game.MineSweeper.Visual.Test;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Layouts, FMX.Objects,
  gs.Game.MineSweeper;

type
  TFrameMineSweeperVisualTest = class(TFrame)
    Selection1: TSelection;
    GridPanelLayout1: TGridPanelLayout;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    Button9: TButton;
    Button10: TButton;
    Button11: TButton;
    Button12: TButton;
    Button13: TButton;
    Button14: TButton;
    Button15: TButton;
    Button16: TButton;
    Button17: TButton;
    Button18: TButton;
    Button19: TButton;
    Button20: TButton;
    Button21: TButton;
    Button22: TButton;
    Button23: TButton;
    Button24: TButton;
    Button25: TButton;
    Button26: TButton;
    procedure ButtonClick(Sender: TObject);
  private
    FGameInstance : TGSMineSweeper;
    { Private declarations }
  protected
    procedure internalRefreshLayout; virtual;
  public
    { Public declarations }

    procedure Init;

    procedure OnCellEvent(Sender: TObject; X, Y: Integer; Content: TMineSweeperContentItemKind; Value: String);
    procedure OnGameOver(Sender: TObject; Victory: Boolean);
  end;

implementation

{$R *.fmx}

{ TFrameMineSweeperVisualTest }

procedure TFrameMineSweeperVisualTest.ButtonClick(Sender: TObject);
begin
  Assert(Sender is TButton);

  //Button
  var lb := TButton(Sender);

  //Button index in grid panel.
  var lindex := GridPanelLayout1.ControlCollection.IndexOf(lb);
  //Control (Button's owner) index.
  var lcontrolItem := GridPanelLayout1.ControlCollection.Items[lindex];

  FGameInstance.RevealAt(lcontrolItem.Column,lcontrolItem.Row);
end;

procedure TFrameMineSweeperVisualTest.Init;
begin
  FGameInstance := TGSMineSweeper.Create(5, 5, 5);
  FGameInstance.OnCellEvent := OnCellEvent;
  FGameInstance.OnGameOver := OnGameOver;

  InternalRefreshLayout;

end;


procedure TFrameMineSweeperVisualTest.internalRefreshLayout;
begin
  Assert(FGameInstance.Width=GridPanelLayout1.ColumnCollection.Count);
  Assert(FGameInstance.height=GridPanelLayout1.RowCollection.Count);

  for var X := 0 to FGameInstance.Width - 1 do
  begin
    for var Y := 0 to FGameInstance.Height - 1 do
    begin
      var FCellType := FGameInstance.GetCellContentKind(X, Y);
      var FCellState := FGameInstance.GetCellState(X, Y);

      var ci := GridPanelLayout1.ControlCollection.Controls[X,Y];
      TButton(ci).Text := FGameInstance.GetCellValue(X,Y);
    end;
  end;
end;

procedure TFrameMineSweeperVisualTest.OnCellEvent(Sender: TObject; X,
  Y: Integer; Content: TMineSweeperContentItemKind; Value: String);
begin
//  ShowMessage(format('%d %d %s',[X,Y,value]));
  var ci := GridPanelLayout1.ControlCollection.Controls[X,Y];
  TButton(ci).Text := FGameInstance.GetCellValue(X,Y);
end;

procedure TFrameMineSweeperVisualTest.OnGameOver(Sender: TObject;
  Victory: Boolean);
begin
  showMessAGE('GAME OVER');
end;

end.
