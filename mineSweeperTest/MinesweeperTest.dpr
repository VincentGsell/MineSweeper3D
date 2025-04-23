program MineSweeperTest;

uses
  System.StartUpCopy,
  FMX.Forms,
  GS.Game.MineSweeper.Test in 'GS.Game.MineSweeper.Test.pas' {FrmMineSweeperTest},
  gs.game.mineSweeper in '..\mineSweeperLogic\gs.game.mineSweeper.pas',
  gs.mineSweeper.FMX2D in '..\mineSweeper\gs.mineSweeper.FMX2D.pas',
  gs.mineSweeper.FMX in '..\mineSweeper\gs.mineSweeper.FMX.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFrmMineSweeperTest, FrmMineSweeperTest);
  Application.Run;
end.
