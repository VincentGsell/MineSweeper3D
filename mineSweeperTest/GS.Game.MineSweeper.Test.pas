unit GS.Game.MineSweeper.Test;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.ScrollBox,
  FMX.Memo, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts, FMX.Memo.Types,
  GS.Game.MineSweeper, FMX.Objects;

type
  TFrmMineSweeperTest = class(TForm)
    btnRunTests: TButton;
    memResults: TMemo;
    Layout1: TLayout;
    Button1: TButton;
    procedure btnRunTestsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    FTestCount: Integer;
    FPassCount: Integer;
    FGameInstance: TGSMineSweeper;
    FCellEvents: TStringList;
    FGameOverCalled: Boolean;
    FGameOverResult: Boolean;

    procedure Log(const AMessage: string);
    procedure LogTestResult(const ATestName: string; AResult: Boolean);
    procedure Assert(const ATestName: string; ACondition: Boolean; const AMessage: string = '');

    procedure OnCellEvent(Sender: TObject; X, Y: Integer; Content: TMineSweeperContentItemKind; Value: String);
    procedure OnGameOver(Sender: TObject; Victory: Boolean);

    { Test Cases }
    procedure TestCreateGame;
    procedure TestBasicReveal;
    procedure TestMineReveal;
    procedure TestFlagToggle;
    procedure TestQuestionToggle;
    procedure TestAutoReveal;
    procedure TestGameWinCondition;
    procedure TestGameLoseCondition;
  public
    { Public declarations }
    destructor Destroy; override;

  end;

var
  FrmMineSweeperTest: TFrmMineSweeperTest;

implementation

uses GS.mineSweeper.FMX2D;

{$R *.fmx}

procedure TFrmMineSweeperTest.FormCreate(Sender: TObject);
begin
  FCellEvents := TStringList.Create;
  memResults.Lines.Clear;
  Caption := 'MineSweeper - Test de non régression';
end;

destructor TFrmMineSweeperTest.Destroy;
begin
  FCellEvents.Free;
  inherited;
end;

procedure TFrmMineSweeperTest.Log(const AMessage: string);
begin
  memResults.Lines.Add(AMessage);
  Application.ProcessMessages;
end;

procedure TFrmMineSweeperTest.LogTestResult(const ATestName: string; AResult: Boolean);
begin
  Inc(FTestCount);
  if AResult then
  begin
    Inc(FPassCount);
    Log(Format('[✓] %s: RÉUSSI', [ATestName]));
  end
  else
    Log(Format('[✗] %s: ÉCHEC', [ATestName]));
end;

procedure TFrmMineSweeperTest.Assert(const ATestName: string; ACondition: Boolean; const AMessage: string = '');
begin
  LogTestResult(ATestName, ACondition);
  if not ACondition and (AMessage <> '') then
    Log('    → ' + AMessage);
end;

procedure TFrmMineSweeperTest.OnCellEvent(Sender: TObject; X, Y: Integer; 
  Content: TMineSweeperContentItemKind; Value: String);
var
  ContentStr: string;
begin
  case Content of
    emptyCell: ContentStr := 'Empty';
    CountingCell: ContentStr := 'Count';
    MineCell: ContentStr := 'Mine';
  end;
  
  FCellEvents.Add(Format('Cell(%d,%d): %s, Value=%s', [X, Y, ContentStr, Value]));
end;

procedure TFrmMineSweeperTest.OnGameOver(Sender: TObject; Victory: Boolean);
begin
  FGameOverCalled := True;
  FGameOverResult := Victory;
end;

procedure TFrmMineSweeperTest.TestCreateGame;
begin
  // Create a small game board for testing
  if Assigned(FGameInstance) then
    FreeAndNil(FGameInstance);

  FGameInstance := TGSMineSweeper.Create(5, 5, 5);
  FGameInstance.OnCellEvent := OnCellEvent;
  FGameInstance.OnGameOver := OnGameOver;

  FCellEvents.Clear;
  FGameOverCalled := False;
  
  Assert('Création du jeu', 
    (FGameInstance.Width = 5) and 
    (FGameInstance.Height = 5) and 
    (FGameInstance.MineCount = 5) and 
    not FGameInstance.GameOver);
end;

procedure TFrmMineSweeperTest.TestBasicReveal;
var
  X, Y: Integer;
  FoundRevealableCell: Boolean;
  CellType: TMineSweeperContentItemKind;
  CellState: TMineSweeperCellState;
begin
  TestCreateGame;
  FCellEvents.Clear;
  
  // Force game setup (place mines)
  FGameInstance.RevealAt(0, 0);
  
  // Find a cell that can be revealed safely
  FoundRevealableCell := False;
  for X := 0 to FGameInstance.Width - 1 do
  begin
    for Y := 0 to FGameInstance.Height - 1 do
    begin
      CellType := FGameInstance.GetCellContentKind(X, Y);
      CellState := FGameInstance.GetCellState(X, Y);
      
      if (CellType <> MineCell) and (CellState <> csRevealed) then
      begin
        FoundRevealableCell := True;
        Break;
      end;
    end;
    if FoundRevealableCell then
      Break;
  end;
  
  if FoundRevealableCell then
  begin
    FCellEvents.Clear;
    FGameInstance.RevealAt(X, Y);
    
    Assert('Révélation d''une cellule',
      (FCellEvents.Count > 0) and
      (FGameInstance.GetCellState(X, Y) = csRevealed));
  end
  else
    Assert('Révélation d''une cellule', False, 'Aucune cellule révélable trouvée - Essayez un autre test');
end;

procedure TFrmMineSweeperTest.TestMineReveal;
var
  X, Y: Integer;
  FoundMine: Boolean;
begin
  TestCreateGame;
  FCellEvents.Clear;
  FGameOverCalled := False;
  
  // Force game setup (place mines)
  FGameInstance.RevealAt(0, 0);
  
  // Find a mine cell
  FoundMine := False;
  for X := 0 to FGameInstance.Width - 1 do
  begin
    for Y := 0 to FGameInstance.Height - 1 do
    begin
      if FGameInstance.GetCellContentKind(X, Y) = MineCell then
      begin
        FoundMine := True;
        Break;
      end;
    end;
    if FoundMine then
      Break;
  end;
  
  if FoundMine then
  begin
    FCellEvents.Clear;
    FGameInstance.RevealAt(X, Y);
    
    Assert('Révélation d''une mine',
      FGameOverCalled and
      not FGameOverResult and
      FGameInstance.GameOver and
      (FCellEvents.Count > 0));
  end
  else
    Assert('Révélation d''une mine', False, 'Aucune mine trouvée - Réessayez le test');
end;

procedure TFrmMineSweeperTest.TestFlagToggle;
begin
  TestCreateGame;
  
  FCellEvents.Clear;
  FGameInstance.ToggleFlagAt(1, 1);
  
  Assert('Poser un drapeau', FGameInstance.GetCellState(1, 1) = csFlagged);
  
  FCellEvents.Clear;
  FGameInstance.ToggleFlagAt(1, 1);
  
  Assert('Enlever un drapeau', FGameInstance.GetCellState(1, 1) = csUnknown);
end;

procedure TFrmMineSweeperTest.TestQuestionToggle;
begin
  TestCreateGame;
  
  FCellEvents.Clear;
  FGameInstance.ToggleQuestionAt(2, 2);
  
  Assert('Poser un point d''interrogation', FGameInstance.GetCellState(2, 2) = csQuestion);

  FCellEvents.Clear;
  FGameInstance.ToggleQuestionAt(2, 2);

  Assert('Enlever un point d''interrogation', FGameInstance.GetCellState(2, 2) = csUnknown);
end;

procedure TFrmMineSweeperTest.TestAutoReveal;
var
  X, Y: Integer;
  EmptyCellFound: Boolean;
  RevealCount: Integer;
begin
  TestCreateGame;
  
  // Force game setup (place mines)
  FGameInstance.RevealAt(0, 0);
  
  // Try to find an empty cell
  EmptyCellFound := False;
  for X := 0 to FGameInstance.Width - 1 do
  begin
    for Y := 0 to FGameInstance.Height - 1 do
    begin
      if (FGameInstance.GetCellContentKind(X, Y) = emptyCell) and
         (FGameInstance.GetCellState(X, Y) <> csRevealed) then
      begin
        EmptyCellFound := True;
        Break;
      end;
    end;
    if EmptyCellFound then
      Break;
  end;
  
  if EmptyCellFound then
  begin
    FCellEvents.Clear;
    RevealCount := FCellEvents.Count;
    FGameInstance.RevealAt(X, Y);
    
    // Auto-reveal should trigger multiple cell events
    Assert('Révélation automatique', 
      (FCellEvents.Count > RevealCount + 1), 
      'La révélation automatique devrait révéler plusieurs cellules');
  end
  else
    Assert('Révélation automatique', False, 'Aucune cellule vide trouvée - Réessayez le test');
end;

procedure TFrmMineSweeperTest.TestGameWinCondition;
var
  X, Y: Integer;
begin
  // Create a tiny game board with one mine for easy testing
  if Assigned(FGameInstance) then
    FreeAndNil(FGameInstance);
    
  FGameInstance := TGSMineSweeper.Create(2, 2, 1);
  FGameInstance.OnCellEvent := OnCellEvent;
  FGameInstance.OnGameOver := OnGameOver;
  
  // Force game setup
  FGameInstance.RevealAt(0, 0);
  
  // Reveal all non-mine cells
  FGameOverCalled := False;
  
  for X := 0 to 1 do
    for Y := 0 to 1 do
      if FGameInstance.GetCellContentKind(X, Y) <> MineCell then
        FGameInstance.RevealAt(X, Y);
  
  Assert('Condition de victoire', 
    FGameOverCalled and 
    FGameOverResult and 
    FGameInstance.GameOver and 
    FGameInstance.GameWon);
end;

procedure TFrmMineSweeperTest.TestGameLoseCondition;
var
  X, Y: Integer;
  FoundMine: Boolean;
begin
  TestCreateGame;
  
  // Force game setup
  FGameInstance.RevealAt(0, 0);
  
  // Find a mine cell
  FoundMine := False;
  for X := 0 to FGameInstance.Width - 1 do
  begin
    for Y := 0 to FGameInstance.Height - 1 do
    begin
      if FGameInstance.GetCellContentKind(X, Y) = MineCell then
      begin
        FoundMine := True;
        Break;
      end;
    end;
    if FoundMine then
      Break;
  end;
  
  if FoundMine then
  begin
    FCellEvents.Clear;
    FGameOverCalled := False;
    
    FGameInstance.RevealAt(X, Y);
    
    Assert('Condition de défaite', 
      FGameOverCalled and 
      not FGameOverResult and 
      FGameInstance.GameOver and 
      not FGameInstance.GameWon);
  end
  else
    Assert('Condition de défaite', False, 'Aucune mine trouvée - Réessayez le test');
end;

procedure TFrmMineSweeperTest.btnRunTestsClick(Sender: TObject);
begin
  memResults.Lines.Clear;
  Log('=== Démarrage des tests de non-régression ===');
  Log('');
  
  FTestCount := 0;
  FPassCount := 0;
  
  TestCreateGame;
  TestBasicReveal;
  TestFlagToggle;
  TestQuestionToggle;
  TestAutoReveal;
  TestMineReveal;
  TestGameWinCondition;
  TestGameLoseCondition;
  
  Log('');
  Log(Format('=== Tests terminés: %d/%d tests réussis (%.1f%%) ===', 
    [FPassCount, FTestCount, (FPassCount / FTestCount) * 100]));
    
  if Assigned(FGameInstance) then
    FreeAndNil(FGameInstance);
end;

procedure TFrmMineSweeperTest.Button1Click(Sender: TObject);
begin
  var l := TGSFMXMineSweeper2D.Create(Self);
  var lselect := TSelection.Create(Self);
  lselect.AddObject(l);
  l.Align := TAlignLayout.Client;
  AddObject(lselect);
  lselect.SetBounds(100,100,400,400);
end;

end.
