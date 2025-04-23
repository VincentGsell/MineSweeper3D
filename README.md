[![](https://tokei.rs/b1/github/VincentGsell/MineSweeper3D?category=code)](https://github.com//VincentGsell/GS.Bus)
[![](https://tokei.rs/b1/github/VincentGsell/MineSweeper3D?category=files)](https://github.com//VincentGsell/GS.Bus)
[![](https://tokei.rs/b1/github/VincentGsell/MineSweeper3D?category=lines)](https://github.com//VincentGsell/GS.Bus)
[![](https://tokei.rs/b1/github/VincentGsell/MineSweeper3D?category=blanks)](https://github.com//VincentGsell/GS.Bus)
[![](https://tokei.rs/b1/github/VincentGsell/MineSweeper3D?category=comments)](https://github.com//VincentGsell/GS.Bus)

# MineSweeper3D
MineSweeper3D - Revisiting mineSweeper to new dimension ;)

![Alt text](/_projectDisplay/20250423_Minesweeper3D_VGS_SandBox_Preview.gif?raw=true "3D MineSweeper :)")
![Alt text](/_projectDisplay/20250423_Minesweeper3D_VGS_Game_Preview.png?raw=true "YAMS game app :)")

 - History
	- 20250423 : Initial.

- main features : 
 	- MineSweeper basic feature :
  - grids(width and heigh) and mine's count parameters
  - 2D and 3D component, with same logic behind.
  - SOLID design aproach.
  
 
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
  - gsMineSweeperGame : A 3D game "YAMS" aka "Yet Another Minesweeper" with menu and animation  - Tentatively a "show" to apply component in a an app env. (WARINING : Very WIP)
 
## Technical note :
- Use FMX and FMX3d
- Use opensource FMX Shader frome Toth Paul - Thx to him - https://github.com/tothpaul/Delphi.proctree (unit Execute.TransparentTexture.pas) 
    - Due to the user af this unit, This app will (certainly)( not work on Mobile due to use this shader, but if anyone is interested, we can adpat it to make it compile on all platform.
    - Simple workaround consist to remove this unit and replace TLightMaterialSource by standart TTextureMaterialSource. This replacement will certainly break the visual of geMineSweeperGame.
- Use FMXAnimation techniques.


- Credits :
  - Sponsored by GRID System (https://github.com/GRIDSystemSAS)
  
  - Music "steampunk-pirates-289789" by Crissa, on PixaBay (https://pixabay.com/music/main-title-steampunk-pirates-289789/)
  - Execute.TransparentTexture.pas unit by Paul Toth (https://github.com/tothpaul)
  - 🙌 Support the Project If you enjoy this work
  - Bitcoin (BTC): 31xvRCr3CQkG9TQVZMfFfCL288auWNthyt

Thank you for your support! 💛
