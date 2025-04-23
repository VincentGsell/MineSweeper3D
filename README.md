# MineSweeper3D
MineSweeper3D - Revisiting mineSweeper to new dimension ;)

## 2025, April : MineSweeper Context
https://github.com/orgs/radprogrammer/discussions

## Revisiting the old MinseSweeper 
- 3D !
- Game "ambiance"
- Logic code architecture to Visual representation
--> 2 favour of visual fo one single "logic".
  - a 2D (gs.mineSweeper.FMX2D.pas)
  - a 3D (gs.mineSweeper.FMX3D.pas)
- Several apps :
  - gsMineSweeper : A sandbox for trying components in their environnement.
  - gsMineSweeperTest : Non regression test + sandBox.
  - gsMineSweeperGame : A 3D game with menu and animation  - Tentatively a "show" to apply component in a an app env. (WARINING : Very WIP)
 
## Technical note :
- Use FMX and FMX3d
- Use opensource FMX Shader frome Toth Paul - Thx to him - https://github.com/tothpaul/Delphi.proctree (unit Execute.TransparentTexture.pas) 
    - Due to the user af this unit, This app will (certainly)( not work on Mobile due to use this shader, but if anyone is interested, we can adpat it to make it compile on all platform.
    - Simple workaround consist to remove this unit and replace TLightMaterialSource by standart TTextureMaterialSource. This replacement will certainly break the visual of geMineSweeperGame.
- Use FMXAnimation techniques.

