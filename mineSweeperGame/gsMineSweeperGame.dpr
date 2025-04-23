program gsMineSweeperGame;

uses
  System.StartUpCopy,
  FMX.Forms,
  gsMineSweeperGame.fmain in 'gsMineSweeperGame.fmain.pas' {Form64},
  Execute.TransparentTexture in 'srcTiers\Execute.TransparentTexture.pas',
  gs.game.mineSweeper in '..\mineSweeperLogic\gs.game.mineSweeper.pas',
  gs.mineSweeper.FMX3D in '..\mineSweeper\gs.mineSweeper.FMX3D.pas',
  gs.mineSweeper.FMX in '..\mineSweeper\gs.mineSweeper.FMX.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm64, Form64);
  Application.Run;
end.
