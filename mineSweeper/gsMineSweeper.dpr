program gsMineSweeper;

uses
  System.StartUpCopy,
  FMX.Forms,
  mineSweeper.fmain in 'mineSweeper.fmain.pas' {Form63},
  gs.game.mineSweeper in '..\mineSweeperLogic\gs.game.mineSweeper.pas',
  gs.mineSweeper.FMX in 'gs.mineSweeper.FMX.pas',
  gs.mineSweeper.FMX2D in 'gs.mineSweeper.FMX2D.pas',
  gs.mineSweeper.FMX3D in 'gs.mineSweeper.FMX3D.pas',
  Execute.TransparentTexture in '..\mineSweeperGame\srcTiers\Execute.TransparentTexture.pas';

{$R *.res}

begin
  {$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := true;
  {$ENDIF}
  Application.Initialize;
  Application.CreateForm(TForm63, Form63);
  Application.Run;
end.
