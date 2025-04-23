unit gs.game.mineSweeper;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections;

const
  DefaultSideAndMineCount = 5;

type
  TMineSweeperContentItemKind = (emptyCell, CountingCell, MineCell);

  TMineSweeperCellState = (csUnknown, csFlagged, csQuestion, csRevealed);

  TCellEvent = procedure(Sender: TObject; X, Y: Integer; Content: TMineSweeperContentItemKind; Value: String) of object;
  TGameOverEvent = procedure(Sender: TObject; Victory: Boolean) of object;

  TGSMineSweeperCell = class
  private
    FX, FY: Integer;
    FHasMine: Boolean;
    FState: TMineSweeperCellState;
    FAdjacentMines: Integer;
  public
    constructor Create(X, Y: Integer);
    property X: Integer read FX;
    property Y: Integer read FY;
    property HasMine: Boolean read FHasMine write FHasMine;
    property State: TMineSweeperCellState read FState write FState;
    property AdjacentMines: Integer read FAdjacentMines write FAdjacentMines;
  end;

  iGSLogicalMineSweeper = interface
    procedure NewGame;
    procedure RevealAt(X, Y: Integer);
    procedure ToggleFlagAt(X, Y: Integer);
    procedure ToggleQuestionAt(X, Y: Integer);
    function GetCellContentKind(X, Y: Integer): TMineSweeperContentItemKind;
    function GetCellValue(X, Y: Integer): String;
    function GetCellState(X, Y: Integer): TMineSweeperCellState;
    procedure RevealAllMines;
    function GetHeight: Integer;
    function GetMineCount: Integer;
    function GetWidth: Integer;
  end;

  TGSMineSweeper = class(TinterfacedObject, iGSLogicalMineSweeper)
  private
    FWidth: Integer;
    FHeight: Integer;
    FMineCount: Integer;
    FCells: TObjectList<TGSMineSweeperCell>;
    FGameOver: Boolean;
    FGameStarted: Boolean;
    FRemainingCells: Integer;
    FOnCellEvent: TCellEvent;
    FOnGameOver: TGameOverEvent;
    FinternalButtonClick: TNotifyEvent;

    function GetCell(X, Y: Integer): TGSMineSweeperCell;
    function IsValidCell(X, Y: Integer): Boolean;
    procedure CalculateAdjacentMines;
    procedure RevealCell(Cell: TGSMineSweeperCell);
    procedure AutoRevealAdjacentCells(X, Y: Integer);
    procedure SetupGame;
    function GetGameWon: Boolean;

    function GetHeight: Integer;
    function GetMineCount: Integer;
    function GetWidth: Integer;

  public
    constructor Create(Width, Height, MineCount: Integer);
    destructor Destroy; override;

    procedure NewGame;
    procedure RevealAt(X, Y: Integer);
    procedure ToggleFlagAt(X, Y: Integer);
    procedure ToggleQuestionAt(X, Y: Integer);
    function GetCellContentKind(X, Y: Integer): TMineSweeperContentItemKind;
    function GetCellValue(X, Y: Integer): String;
    function GetCellState(X, Y: Integer): TMineSweeperCellState;
    procedure RevealAllMines;

    property Width: Integer read GetWidth;
    property Height: Integer read GetHeight;
    property MineCount: Integer read GetMineCount;
    property GameOver: Boolean read FGameOver;
    property GameWon: Boolean read GetGameWon;
    property OnCellEvent: TCellEvent read FOnCellEvent write FOnCellEvent;
    property OnGameOver: TGameOverEvent read FOnGameOver write FOnGameOver;

  end;


implementation

{ TGSMineSweeperCell }

constructor TGSMineSweeperCell.Create(X, Y: Integer);
begin
  inherited Create;
  FX := X;
  FY := Y;
  FHasMine := False;
  FState := csUnknown;
  FAdjacentMines := 0;
end;

{ TGSMineSweeper }

constructor TGSMineSweeper.Create(Width, Height, MineCount: Integer);
begin
  inherited Create;

  FWidth := Width;
  FHeight := Height;
  FMineCount := MineCount;

  if FMineCount >= (FWidth * FHeight) then
    FMineCount := (FWidth * FHeight) div 4; // Reasonable default if too many mines

  FCells := TObjectList<TGSMineSweeperCell>.Create(True); // Owned cells

  NewGame;
end;

destructor TGSMineSweeper.Destroy;
begin
  FCells.Free;
  inherited;
end;

function TGSMineSweeper.GetCell(X, Y: Integer): TGSMineSweeperCell;
var
  I: Integer;
begin
  Result := nil;

  if not IsValidCell(X, Y) then
    Exit;

  for I := 0 to FCells.Count - 1 do
  begin
    if (FCells[I].X = X) and (FCells[I].Y = Y) then
    begin
      Result := FCells[I];
      Break;
    end;
  end;
end;

function TGSMineSweeper.IsValidCell(X, Y: Integer): Boolean;
begin
  Result := (X >= 0) and (X < FWidth) and (Y >= 0) and (Y < FHeight);
end;

procedure TGSMineSweeper.NewGame;
begin
  FCells.Clear;
  FGameOver := False;
  FGameStarted := False;
  FRemainingCells := FWidth * FHeight - FMineCount;

  // Create cells
  for var Y := 0 to FHeight - 1 do
    for var X := 0 to FWidth - 1 do
      FCells.Add(TGSMineSweeperCell.Create(X, Y));

  SetupGame;
end;

procedure TGSMineSweeper.SetupGame;
var
  MinesPlaced, RandomIndex: Integer;
  Cell: TGSMineSweeperCell;
begin
  // Place mines randomly
  MinesPlaced := 0;
  while MinesPlaced < FMineCount do
  begin
    RandomIndex := Random(FCells.Count);
    Cell := FCells[RandomIndex];

    if not Cell.HasMine then
    begin
      Cell.HasMine := True;
      Inc(MinesPlaced);
    end;
  end;

  // Calculate adjacent mines for each cell
  CalculateAdjacentMines;

  FGameStarted := True;
end;

procedure TGSMineSweeper.CalculateAdjacentMines;
var
  Cell, Adjacent: TGSMineSweeperCell;
  I, DX, DY, AdjX, AdjY: Integer;
begin
  for I := 0 to FCells.Count - 1 do
  begin
    Cell := FCells[I];
    if not Cell.HasMine then
    begin
      // Check all 8 adjacent cells
      for DX := -1 to 1 do
        for DY := -1 to 1 do
        begin
          if (DX = 0) and (DY = 0) then
            Continue; // Skip the cell itself

          AdjX := Cell.X + DX;
          AdjY := Cell.Y + DY;

          Adjacent := GetCell(AdjX, AdjY);
          if (Adjacent <> nil) and Adjacent.HasMine then
            Cell.AdjacentMines := Cell.AdjacentMines + 1;
        end;
    end;
  end;
end;

procedure TGSMineSweeper.RevealCell(Cell: TGSMineSweeperCell);
var
  ContentKind: TMineSweeperContentItemKind;
  Value: String;
begin
  if (Cell = nil) or (Cell.State = csRevealed) or (Cell.State = csFlagged) or FGameOver then
    Exit;

  Cell.State := csRevealed;

  // Determine cell content and value
  if Cell.HasMine then
  begin
    ContentKind := MineCell;
    Value := 'M';
    FGameOver := True;

    RevealAllMines;

    if Assigned(FOnGameOver) then
      FOnGameOver(Self, False); // Game lost
  end
  else
  begin
    Dec(FRemainingCells);

    if Cell.AdjacentMines > 0 then
    begin
      ContentKind := CountingCell;
      Value := IntToStr(Cell.AdjacentMines);
    end
    else
    begin
      ContentKind := emptyCell;
      Value := '';
      // Auto-reveal adjacent cells for empty cells
      AutoRevealAdjacentCells(Cell.X, Cell.Y);
    end;

    // Check for win condition
    if FRemainingCells = 0 then
    begin
      FGameOver := True;

      RevealAllMines;

      if Assigned(FOnGameOver) then
        FOnGameOver(Self, True); // Game won
    end;
  end;

  // Trigger cell event
  if Assigned(FOnCellEvent) then
    FOnCellEvent(Self, Cell.X, Cell.Y, ContentKind, Value);
end;

procedure TGSMineSweeper.AutoRevealAdjacentCells(X, Y: Integer);
var
  DX, DY, AdjX, AdjY: Integer;
  Adjacent: TGSMineSweeperCell;
begin
  // Reveal all adjacent cells for an empty cell
  for DX := -1 to 1 do
    for DY := -1 to 1 do
    begin
      if (DX = 0) and (DY = 0) then
        Continue; // Skip the cell itself

      AdjX := X + DX;
      AdjY := Y + DY;

      Adjacent := GetCell(AdjX, AdjY);
      if (Adjacent <> nil) and (Adjacent.State = csUnknown) then
        RevealCell(Adjacent);
    end;
end;

procedure TGSMineSweeper.RevealAt(X, Y: Integer);
var
  Cell: TGSMineSweeperCell;
begin
  // Start the game on first reveal
  if not FGameStarted then
    SetupGame;

  Cell := GetCell(X, Y);
  if Cell <> nil then
    RevealCell(Cell);
end;

procedure TGSMineSweeper.ToggleFlagAt(X, Y: Integer);
var
  Cell: TGSMineSweeperCell;
begin
  if FGameOver then
    Exit;

  Cell := GetCell(X, Y);
  if (Cell <> nil) and (Cell.State <> csRevealed) then
  begin
    if Cell.State = csFlagged then
      Cell.State := csUnknown
    else
      Cell.State := csFlagged;

    // Notify of state change (optional)
    if Assigned(FOnCellEvent) then
      FOnCellEvent(Self, X, Y, GetCellContentKind(X, Y), GetCellValue(X, Y));
  end;
end;

procedure TGSMineSweeper.ToggleQuestionAt(X, Y: Integer);
var
  Cell: TGSMineSweeperCell;
begin
  if FGameOver then
    Exit;

  Cell := GetCell(X, Y);
  if (Cell <> nil) and (Cell.State <> csRevealed) then
  begin
    if Cell.State = csQuestion then
      Cell.State := csUnknown
    else
      Cell.State := csQuestion;

    // Notify of state change (optional)
    if Assigned(FOnCellEvent) then
      FOnCellEvent(Self, X, Y, GetCellContentKind(X, Y), GetCellValue(X, Y));
  end;
end;

function TGSMineSweeper.GetCellContentKind(X, Y: Integer): TMineSweeperContentItemKind;
var
  Cell: TGSMineSweeperCell;
begin
  Result := emptyCell; // Default

  Cell := GetCell(X, Y);
  if Cell = nil then
    Exit;

  if Cell.HasMine then
    Result := MineCell
  else if Cell.AdjacentMines > 0 then
    Result := CountingCell
  else
    Result := emptyCell;
end;

function TGSMineSweeper.GetCellValue(X, Y: Integer): String;
var
  Cell: TGSMineSweeperCell;

  function internalContent : String;
  begin
    if Cell.HasMine then
      Result := 'M'
    else if Cell.AdjacentMines > 0 then
      Result := IntToStr(Cell.AdjacentMines)
    else
      Result := '';
  end;
begin
  Result := '';

  Cell := GetCell(X, Y);
  if Cell = nil then
    Exit;

  case Cell.State of
    csUnknown: {$IFDEF DEBUG} Result := '{'+InternalContent+'}' {$ELSE} Result := ''{$ENDIF};
    csFlagged: Result := 'F';
    csQuestion: Result := '?';
    csRevealed:
      begin
        result := internalContent;
      end;
  end;
end;

function TGSMineSweeper.GetCellState(X, Y: Integer): TMineSweeperCellState;
var
  Cell: TGSMineSweeperCell;
begin
  Result := csUnknown; // Default

  Cell := GetCell(X, Y);
  if Cell <> nil then
    Result := Cell.State;
end;

procedure TGSMineSweeper.RevealAllMines;
var
  I: Integer;
  Cell: TGSMineSweeperCell;
begin
  for I := 0 to FCells.Count - 1 do
  begin
    Cell := FCells[I];
    //if Cell.HasMine and (Cell.State <> csRevealed) then
    if (Cell.State <> csRevealed) then
    begin
      Cell.State := csRevealed;

      var lvKind := getCellContentKind(Cell.X,Cell.Y);
      var lvValue := GetCellValue(Cell.X,Cell.Y);

      RevealCell(Cell);
      // Trigger cell event
      if Assigned(FOnCellEvent) then
         FOnCellEvent(Self, Cell.X, Cell.Y, lvKind, lvValue);
    end;

   end;
end;

function TGSMineSweeper.GetGameWon: Boolean;
begin
  Result := FGameOver and (FRemainingCells = 0);
end;

function TGSMineSweeper.GetHeight: Integer;
begin
  result := FHeight;
end;

function TGSMineSweeper.GetMineCount: Integer;
begin
  result := FMineCount;
end;

function TGSMineSweeper.GetWidth: Integer;
begin
  result := FWidth;
end;

end.
