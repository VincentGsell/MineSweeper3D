unit gs.mineSweeper.FMX;

interface

uses
  System.SysUtils,
  System.Classes,
  FMX.Types,
  GS.game.mineSweeper;

Type

  ///Interface to implement for GUI frontend target.
  //
  iMineSweeperGenerator = interface
    procedure Generate(aLogicMineSweeper : iGSLogicalMineSweeper);
  end;



implementation

end.
