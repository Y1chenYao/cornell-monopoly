# Cornell Monopoly
#### Co-authors: Melanie Gao, Siyuan Shen, Yichen Yao

<p align="center">
  <img src="https://user-images.githubusercontent.com/68068854/191635438-4f88e0c5-fd56-49d1-bbe9-98583796e6ca.png" alt="screenshot" width="50%" />
</p>



## Installation

1. Please follow directions from [CS3110 OCaml setup guide](https://cs3110.github.io/textbook/chapters/preface/install.html).

2. Download the zip file and copy it to the linux subsystem. Cd to the corresponding folder. Unzip it in the commandline.

3. If there accidentally exists `_build` folder (which shouldn't happen), run `make clean`.

4. Run `make build`

5. Run `make play`, and you can enter number of players and player names. Game starts!

## Game rules

### How to win?

Each player initially have 50 BRB, 3.0 GPA, and 50 mood level. The goal of the game is to get to 4.3 GPA. A player is kicked out of the game when their mood level is 0.

### Some features

#### Event

At the event node, a player randomly picks an event, which may have positive or negative impact on their GPA, BRB, and mood. For example, their friend might buy them a smoothie (good!) or they might bomb a 3110 prelim (bad!).

#### Shop

Each shop has a unique set of gadgets that has positive influence on a player's GPA and mood. However, it costs brb, so a player may not be able to buy an item when stepped on a shop node.

#### Bus

Bus enables players to travel from different parts of the campus. A player has no chose but to be teleported.

#### Bridge

Alternatively, a player can choose to cross the bridge or not. Their decision may be influenced by the location of star.

#### Star

Star is generated at random location that is automatically picked up if the player walked on its path. Picking up a star boost the player's brb. The star does not disappear after picking up to enable another player to also pick up the star.

### Complete list of commands

roll: Roll a dice to begin stepping
inventory: Check the current player's inventory
buy item: buy the item if the item is valid and has enough gpa.
pass: leave the shop without buying an item.
Use item: Use the item if it is in the inventory.
quit: terminate the game

### Multiple maps to choose from

Cornell: a general map that captures the stressful life of Cornell.
Engineering quad: a more specific map with the common places to have CS classes.
