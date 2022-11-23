extensions [sound] ; SOUNDS EXTENSION

; -------------------------------------------------------------------------------
; GLOBALS
; -------------------------------------------------------------------------------

globals
[
  grass_color ; the color of the grass
  conn_count ; connection count
  ynodes ; for vertical initialization
  xnodes ; for horizontal initialization

  new-connections ; list of arr [node1, node2] representing new connections to be visualized
  created ; number of connections created
  destroyed ; number of connections deleted

  pspeed ; player speed
  velocity ; player velocity - used for momentum based movement
  max_spd ; maximum player speed - used to cap player speed for momentum based movement
  deltaX ; used for changing x velocity in momentum based movement
  deltaY ; used for changing y velocity in momentum based movement

  spacing ; spacing of the maze
  maze-size ; size of the maze
  patch-size_ ; size of the patches
  mazerate ; the rate the maze changes at

  game_status ; the status of the game- i.e. active/alive, game over, beat the level, etc.
  playagain ; used for initiating new level play
  equip ; currently equipped weapon
  attack_cooldown ; attack cooldown for the player

  ; variable by level:
  level ; current level
  minion_health ; health of the minions/ants
  boss_health ; health of the boss
  minion_count ; number of minions/ants in the level
  ant_speed ; speed of the ants

]

; -------------------------------------------------------------------------------
; BREEDS
; -------------------------------------------------------------------------------

breed [objbehinds objbehind] ; objects displayed behind the player
breed [players player] ; player
breed [objfronts objfront] ; objects displayed in front of the player
breed [enemies enemy] ; enemy minions
breed [projs proj] ; projectile

breed [uies uie] ; User Interface Element


; -------------------------------------------------------------------------------
; INSTANCE VARIABLES
; -------------------------------------------------------------------------------

patches-own
[
  wall? ; is the patch a wall
  wall-node? ; is the patch a wall-node (un-destroyable ends of the walls on the initial grid of wall-nodes)
  connections ; list of other wall-nodes the wall-node is connected to
  conn_id ; connection id of a wall - allows us to destroy only the wall patches in the selected connection to be destroyed
  destroy? ; is the patch part of a wall connection currently being destroyed
  wall-dir ; the direction that the wall is built ([1,0], [-1,0], [0,1], or [0,-1])
  distorted? ; whether the wall has been distorted yet

  score ; score of the patch relates to how close it is to the player in terms of maze traversal
  score_visited? ; used for determining patch scores in the set-pscores method: holds whether the patch has been visited by the algorithm
  prev_score_node ; used for determining patch scores in the set-pscores method: holds the previous patch that this patch is coming after
]

objbehinds-own
[
  ; objects that will appear behind the player
  obj_type ; holds the type of object
]

players-own
[
  max_health ; player maximum health
  health ; player healthbar
  healthbar ; visual healthbar agent
  facing ; shows the direction the player is facing
]

objfronts-own
[
  ; objects that will appear behind the player
  obj_type ; holds the type of object
]

enemies-own
[
  et ; enemy type
  max_health ; maximum health of the enemy
  health ; current health of the enemy
  trace-score ; score of the patch the enemy is on - used for tracing the path when close to the player
  curr_dir ; current direction the enemy is moving in
  spd ; used for adjusting minotaur movement speed
  healthbar ; enemy visual healthbar, for the boss
]

projs-own
[
  proj_type ; type of projectile
]



; -------------------------------------------------------------------------------
; LEVEL SETUP
; -------------------------------------------------------------------------------

; INITIALIZE A LEVEL
to init-level [level_no]

  ; stop the user from trying to enter a previous level if the current level is the first level
  ifelse level_no < 0
  [
    print "You are already at the first level!"
    ask player 0 [ move-to min-one-of patches with [(pxcor - 45) >= 10][distance myself] ]
    set level 0
  ]
  [
    ; if they are attempting to enter level 0, allow them to
    if level_no = 0
    [ init-level0 ]

    ; given there is no second level, stop the user from attempting to play it
    if level_no = 1
    [
      print "Second level design work in-progress..."
      ask player 0 [ move-to min-one-of patches with [165 - pxcor >= 10][distance myself] ]
      set level 0
    ]
  ]

end

; INITIALIZE LEVEL 0
to init-level0
  ca
  reset-ticks
  set boss_health 500
  set minion_health 50
  set minion_count 10
  sound:play-note "taiko drum" 30 128 .1 ; sound effect
  wait .1
  setup
end

; INITIALIZE LEVEL 1
to init-level1

  ; there is no level 1 yet..

end



; -------------------------------------------------------------------------------
; SETUP THE LABYRINTH AND INITIALIZE THE RELEVANT VARIABLES
; -------------------------------------------------------------------------------

; SET UP THE PATCHES AND AGENTS FOR GAMEPLAY
to setup

  ; clear and reset relevant global variables

  set playagain false ; initialize to false

  ; set maze variables accordingly to correctly setup the labyrinth
  set spacing 14
  set maze-size 15
  set patch-size_ 3
  ; create the world according to user presets
  resize-world 0 (spacing * maze-size) 0 (spacing * maze-size)
  set-patch-size patch-size_
  set mazerate 100 ; set the rate at which the labyrinth changes

  ; initialize connection variables to 0 because no connections have been made yet
  set conn_count 0
  set created 0
  set destroyed 0
  set new-connections list 0 0
  repeat 2 [set new-connections remove 0 new-connections]

  set grass_color green + 3 ; set the desired grass color
  ; reset all patch instance variables
  ask patches
  [
    ; all patches should be grass
    set-grass
    set wall-node? false
  ]
  ; initialize the maze
  init-maze



  ; setup player variables
  set pspeed 2 ; set player speed (for static movement)
  set attack_cooldown -1000 ; makes it so the player can attack immediately after spawning in
  set max_spd .5 ; set max player speed (for momentum based movement)
  set deltaX 0 ; set current x accel 0
  set deltaY 0 ; set current y accel 0
  set velocity list 0 0 ; set current velocity 0

  ; initialize the player
  create-players 1
  [
    set size spacing * patch-size_ / 3 ; set size relative to maze size
    set heading 0
    set shape "player-down"
    set max_health 100
    set health max_health ; health should start full
    set color blue
    move-to one-of patches with [not wall? and ; start at a grass patch in the labyrinth
      (pxcor > spacing and pxcor < spacing * xnodes and pycor > spacing and pycor < spacing * ynodes)
    ]
    ; create the white healbar
    hatch-uies 1
    [
      set shape "healthbar"
      set size 7
      set heading 0
      set color white
      setxy (xcor) (ycor + 5)
      create-link-with myself ; we want it to move with the player
      [ tie hide-link ]
    ]
    ; create the red dynamic healthbar
    hatch-uies 1
    [
      set shape "healthbar"
      set size 7
      set heading 0
      set color red
      setxy (xcor) (ycor + 5)
      ask myself [ set healthbar myself ] ; set this as the player healthbar so that it changes dynamically
      create-link-with myself ; we want it to move with the player
      [ tie hide-link ]
    ]

  ]
  ; initialize player weapons
  init-weapons

  ; initialize the enemies
  init-enemies

  ; tell the observer to set the perspective to follow to player
  follow player 0

  ; allow the game to start
  set game_status "alive"

end

; SET UP THE VISUAL WEAPONS
to init-weapons

  ; sword
  set equip "sword" ; the player should start with the sword equipped

  ; the player has two visual swords- one in front of his body and one behind, the one that shows is determined by his movement and where he is facing
  create-objbehinds 1
  [
    set obj_type "sword" ; setting object type as a sword
    set shape "sword"
    set size 5
    set heading 25
    setxy ([xcor] of player 0 + 2) ([ycor] of player 0 + .5) ; location placement relative to the player
    create-link-with player 0 ; should move with the player
    [ tie hide-link ]
    ht ; this sword starts hidden
  ]
  create-objfronts 1
  [
    set obj_type "sword" ; setting object type as a sword
    set shape "sword"
    set size 5
    set heading -25
    setxy ([xcor] of player 0 - 2) ([ycor] of player 0 + .5) ; location placement relative to the player
    create-link-with player 0 ; should move with the player
    [ tie hide-link ]
  ]

  create-objbehinds 1
  [
    set obj_type "bow" ; setting object type as a bow
    set shape "bow"
    set color white ; set bowstring color
    set size 5
    set heading 0
    setxy ([xcor] of player 0 + 2) ([ycor] of player 0 + .5) ; location placement relative to the player
    create-link-with player 0 ; should move with the player
    [ tie hide-link ]
    ht ; starts hidden - not the active weapon
  ]
  create-objfronts 1
  [
    set obj_type "bow" ; setting object type as a bow
    set shape "bow"
    set color white ; set bowstring color
    set size 5
    set heading 0
    setxy ([xcor] of player 0 - 2) ([ycor] of player 0 + .5) ; location placement relative to the player
    create-link-with player 0
    [ tie hide-link ] ; should move with the player
    ht ; starts hidden - not the active weapon
  ]

end

; SET UP THE ENEMIES
to init-enemies

  ; make the desired number of ants
  make-ants (minion_count)

  ifelse texturize ; set ant speed and ajust for slower gameplay if texturize is on
  [ set ant_speed .07 ] [set ant_speed .035]

  ; initialize the boss
  create-enemies 1
  [
    set et "minotaur" ; set its enemy type
    set max_health boss_health
    set health max_health ; health should start full
    ifelse texturize ; determine which boss sprite to use
    [ set shape "minotaur-down" ]
    [ set shape "boss-basic" set color brown]
    set size spacing * patch-size_ / 3 ; set size relative to the maze size
    set trace-score 10000 ; set an initially high trace score so it'll follow the player if it gets close
    ifelse texturize ; adjust for slower gameplay if texturize is on
    [ set spd 30 ] [set spd 70]; set its speed (this variable is actually inverse to its speed)
    set curr_dir list (1 - random 3) (1 - random 3) ; the boss moves by finding a direction and moving in that direction until it hits a wall or is near the player
    while [item 0 curr_dir = 0 and item 1 curr_dir = 0] [ set curr_dir list ((1 - random 3) / spd) ((1 - random 3) / spd) ] ; find a valid direction (not 0,0)
    set heading 0

    move-to one-of patches with [not wall? and ; set its initial position as a grass patch in the maze away from the player
      (pxcor > spacing and pxcor < spacing * xnodes and pycor > spacing and pycor < spacing * ynodes) and distance player 0 > 100]

    if texturize ; if the player selected to texturize the game, add an axe visual
    [
      hatch-objfronts 1
      [
        set obj_type "axe" ; set the object type to be an axe
        set shape "axe"
        set color black
        set size spacing * patch-size_ / 7 ; set size according to maze size (and by relation, minotaur size)
        set heading 20 ; adjust axe tilt
        setxy xcor + 4 ycor + .5 ; set relative positioning
        create-link-with myself ; link to the minotaur so it moves with it
        [ hide-link tie ]
      ]
    ]
    ; make the white healthbar for the boss
    hatch-uies 1
    [
      set shape "healthbar"
      set size 7
      set heading 0
      set color white
      setxy (xcor) (ycor + 7) ; set relative positioning
      create-link-with myself ; should move with the boss
      [ tie hide-link ]
    ]
    ; make the red (dynamic) healthbar for the boss
    hatch-uies 1
    [
      set shape "healthbar"
      set size 7
      set heading 0
      set color red
      setxy (xcor) (ycor + 7) ; set relative positioning
      create-link-with myself ; should move with the boss
      [ tie hide-link ]
      ask myself [set healthbar myself] ; set this agent as the boss' healthbar so it can be updated as such
    ]
  ]

end

; MAKE [n] ANTS
to make-ants [n]

  create-enemies n
  [
    set et "ant" ; set them to be ants with the right amount of health
    set health minion_health

    ifelse texturize ; determine visuals based off of selected preferences
    [ set shape "ant 2" ]
    [ set shape "default" ]

    set size spacing * patch-size_ / 7 ; set size based off of labyrinth size
    set color red - 1
    move-to one-of patches with [not wall? and ; start at a grass patch not close to the player
      (pxcor > spacing and pxcor < spacing * xnodes and pycor > spacing and pycor < spacing * ynodes) and distance player 0 > 20]
    set trace-score 10000 ; initialize the trace score as high so it follows the player when close
  ]

end

; INITIALIZE THE MAZE
to init-maze

  ; setting wall nodes as a grid
  ; note: wall-nodes don't change throughout game progression, walls form between them
  let xcurr spacing
  let ycurr spacing
  set xnodes 0
  set ynodes 0
  while [xcurr <= (world-width - spacing)] ; iterate horizontally
  [
    set ycurr spacing
    while [ycurr <= (world-height - spacing)] ; iterate vertically
    [
      ask patch xcurr ycurr ; set this patch to be a wall-node
      [
        set wall-node? true
        set-wall
        set connections list nobody nobody ; initialize the wall-nodes connections as none
        repeat 2 [set connections remove-item 0 connections]
      ]
      if (ycurr = spacing) [set ynodes ynodes + 1]
      set ycurr ycurr + spacing
    ]
    set xcurr xcurr + spacing
    set xnodes xnodes + 1
  ]

  ; connect the edge nodes to create the labyrinth border
  ask patches with [wall-node? and (count patches in-radius (spacing) with [wall-node?] <= 4)] ; ask the edge patches
  [
    ask other patches in-radius spacing with [wall-node? and count patches in-radius spacing with [wall-node?] <= 4 and dist self myself = spacing] ; ask other nearby edge patches
    [
      create-conn myself self
    ]
  ]

  ; randomly connect each wall-node to another to create the maze
  ask patches with [wall-node?]
  [
    ask one-of other patches in-radius spacing with [wall-node?] ; create one random wall
    [
      create-conn myself self
    ]
  ]

  ; visualize the connections created between wall nodes
  initialize-connections

  ; distort the maze if the user selected that preference
  if texturize
  [
    repeat 2 [grow-maze "none"]
    ask patches with [wall?]
    [ set distorted? true ]
  ]

end

; UPDATE THE CONNECTIONS VARIABLES
to create-conn [node1 node2]

  ; create a connection between the two nodes and update each accordingly
  ask node1
  [ set connections lput node2 connections ]
  ask node2
  [ set connections lput node1 connections ]

end

; VISUALIZE THE CREATED CONNECTIONS
to initialize-connections

  ask patches with [wall-node?] ; for each wall-node
  [
    let i 0
    while [i < length connections] ; look at each of its connections
    [
      ask item i connections
      [
        let dir list ((pxcor - [pxcor] of myself) / spacing) ((pycor - [pycor] of myself) / spacing) ; determine the direction of the connection: 1 0, -1 0, 0 1, 0 -1, 1 1, etc.                                                                               ; if you want to filter by horizontal-vertical, use the distance formula and make sure it is = to spacing
        connect (patch ([pxcor] of myself + item 0 dir) ([pycor] of myself + item 1 dir)) dir ; and draw the wall connection between the two
      ]
      set i i + 1
      set conn_count conn_count + 1 ; increment connection count
    ]
  ]

end

; RECURSIVELY TURN THE NEXT PATCH (IN THE SPECIFICED DIRECTION) INTO A WALL UNTIL YOU REACH ANOTHER WALL-NODE
to connect [ cpatch dir]

  ; connect two wall-nodes by turning all the patches in between them into walls
  ask cpatch
  [
    if not [wall-node?] of cpatch ; base case catch
    [
      ; update instance variables
      set wall-dir dir
      set-wall
      set conn_id conn_count ; set the connection id of each patch so that they can be accessed later as one connection

      ; recursive call to create the next connection
      connect (patch (pxcor + item 0 dir) (pycor + item 1 dir)) dir
    ]
  ]

end

; DISTORT THE EDGES
to grow-maze [ condition ]

  ; use the helper method to make more wall patches around the initial conenction
  ifelse condition = "not distorted"
  [
    ask patches with [wall? and not distorted?]
    [ grow-maze-cont ]
  ]
  [ ; condition = "none"
    ask patches with [wall?]
    [ grow-maze-cont ]
  ]

end

; DISTORT THE MAZE AROUND THE WALL THAT CALLED THIS
to grow-maze-cont

  if item 1 wall-dir != 0 ; if the connection is vertical
  [
    ; distort along the width
    carefully[
      ask one-of neighbors with [pycor = [pycor] of myself and not wall?]
      [
        set conn_id [conn_id] of myself ; set connection id to be the same so it can be accessed when destroying that connection
        set-wall
      ]
    ][]
  ]
  if item 0 wall-dir != 0; the connection is horizontal
  [
    ; distort along the length
    carefully[
      ask one-of neighbors with [pxcor = [pxcor] of myself and not wall?]
      [
        set conn_id [conn_id] of myself ; set connection id to be the same so it can be accessed when destroying that connection
        set-wall
      ]
    ][]
  ]

end

; BASIC DISTANCE REPORTER
to-report dist [p1 p2]

  ; reports the distance between two patches
  report sqrt(([pxcor] of p1 - [pxcor] of p2) ^ 2 + ([pycor] of p1 - [pycor] of p2) ^ 2)

end



; -------------------------------------------------------------------------------
; ACTIVE PLAY
; -------------------------------------------------------------------------------

;PLAY
to play

  ifelse game_status = "game over" ; detect for game over
  [
    ; do a quick animation where the screen turns to black and the turtles die
    ifelse any? patches with [pcolor != black]
    [
      turn-screen-black
    ]
    [
      ; once that animation is complete, start another by creating a new turtle in the shape of a skull
      ifelse not any? turtles
      [
        crt 1
        [
          set shape "skull"
          set color white
          set size 0
          set heading 0
          setxy 105 105 ; put it at the center of the screen
        ]
      ]
      [
        ; once the skull is created, make it grow until it reaches a specific size
        ifelse [size] of one-of turtles < 100
        [
          ask one-of turtles
          [ set size size + 1]
          wait .01
        ]
        [
          game-over-text
          ; play a game over noise
          sound:play-note "French Horn" 45 128 1
          wait 1.25
          sound:play-note "French Horn" 40 128 1
          wait 1.25
          sound:play-note "French Horn" 35 128 3
          set game_status "done"
        ]
      ]
    ]
  ]
  [
    ; otherwise, update the game
    move ; allow the player to move (momentum-based)
    evolve-labyrinth ; evolve the labyrinth
    ifelse game_status = "alive" ; if the game is still in the active-play stage
    [
      carefully
      [
        if ticks mod 500 = 0 [ set-pscores ] ; every 500 ticks, recompute the patch scores around the player to allow enemies to track him
        if ticks mod 1000 = 0 and count enemies with [et = "ant"] < minion_count ; every 1000 patches, if there are less than the desired number of minions, spawn one
        [ make-ants 1 ]
        ant-behavior ; update ant-behavior
        boss-behavior ; update boss-behavior
        update-stats ; update the player and boss stats
      ]
      []
    ]
    [
      ; if the player has one the game
      ifelse game_status = "win"
      [
        ask enemies ; kill the minions
        [ die ]
        post-game ; move to post-game scene
      ]
      [
        ; if the player is selecting the next level:
        if game_status = "empty"
        [
          ask one-of turtles with [shape = "door-left"] ; detect if they try to play the previous level
          [
            if any? players in-radius 6
            [
              set level (level - 1)
              set playagain true
            ]
          ]
          ask one-of turtles with [shape = "door-mid"] ; detect if they want to replay the current level
          [
            if any? players in-radius 6
            [
              set playagain true
            ]
          ]
          ask one-of turtles with [shape = "door-right"] ; detect if they want to play the next level
          [
            if any? players in-radius 6
            [
              set level (level + 1)
              set playagain true
            ]
          ]
          if playagain ; if they selected a new level, load them in
          [
            set playagain false
            init-level (level) ; call the function to load into the next level
          ]
        ]
      ]
    ]
    update-projs ; update projectiles
    update-apples ; update the apples
  ]
  tick

end

; ANIMATION TO TURN THE SCREEN BLACK
to turn-screen-black

  ask patches with [pcolor = black and any? neighbors with [pcolor != black]]
  [
    carefully
    [
      ask one-of neighbors with [pcolor != black]
      [
        set pcolor black
        ask turtles in-radius 5 [die]
      ]
    ][]
  ]
  wait .01

end

; VISUALLY MAKE GAME OVER TEXT APPEAR
to game-over-text

  crt 1
  [
    set shape "GA"
    set color red
    set heading 0
    set size 50
    setxy 87 160
  ]
  crt 1
  [
    set shape "ME"
    set color red
    set heading 0
    set size 50
    setxy 123 160
  ]
  crt 1
  [
    set shape "OV"
    set color red
    set heading 0
    set size 50
    setxy 89 51
  ]
  crt 1
  [
    set shape "ER"
    set color red
    set heading 0
    set size 50
    setxy 121 51
  ]

end

; POST GAME SCENE: NO ENEMIES, JUST A KEY TO COLLECT
to post-game

  let next false
  ask one-of turtles with [shape = "key"] ; rotate the key and detect if the player picks it up
  [
    rt .1
    if any? players in-radius 3
    [
      sound:play-note "Crystal" 80 40 3 ; play a sound
      set next true
      ask objfronts with [ shape = "apple" ] [ die ]
      die
    ]
  ]
  if next ; once the player has picked up the key, destroy the labyrinth signifying the level was won
  [
    reset-perspective ; allow the user to see the full screen
    wait .1

    ; now, starting at the player, turn all patches to grass
    let color_sel grass_color + .01
    ask player 0
    [
      ask patch-here
      [ set pcolor color_sel]

      while [any? patches with [pcolor != color_sel]]
      [
        ask patches with [pcolor = color_sel and any? neighbors with [pcolor != color_sel]]
        [
          carefully
          [
            ask one-of neighbors with [pcolor != color_sel]
            [
              set pcolor color_sel
              set wall? false
            ]
          ][]
        ]
        wait .01
      ]
    ]
    set game_status "empty" ; move to the next transition in terms of game_status: detecting which level the player wants to play next
    setup-ui ; setup the UI for next level selection
  ]

end

; SET UP UI FOR NEXT LEVEL SELECTION
to setup-ui

  if abs([ycor] of player 0 - 45) < 10 ; if the player is in range of one of the doors, move him
  [
    ask player 0 [ move-to min-one-of patches with [abs(pycor - 45) >= 10][distance myself] ]
  ]
  create-objbehinds 1 ; create a door in the bottom-left: previous level
  [
    set shape "door-left"
    set heading 0
    set size 20
    setxy 45 45
  ]
  create-objfronts 1 ; text for prev level
  [
    set shape "prev"
    set heading 0
    set size 10
    setxy 39 57
  ]
  create-objfronts 1 ; text for prev level
  [
    set shape "level"
    set heading 0
    set size 10
    setxy 51 57
  ]
  create-objbehinds 1 ; create a door in the middle: replay level
  [
    set shape "door-mid"
    set heading 0
    set size 20
    setxy 105 45
  ]
  create-objfronts 1 ; text for replay level
  [
    set shape "replay p1"
    set heading 0
    set size 10
    setxy 95 57
  ]
  create-objfronts 1 ; text for replay level
  [
    set shape "replay p2"
    set heading 0
    set size 10
    setxy 103 57
  ]
  create-objfronts 1 ; text for replay level
  [
    set shape "level"
    set heading 0
    set size 10
    setxy 111 57
  ]
  create-objbehinds 1 ; create a door in the bottom-right: next level
  [
    set shape "door-right"
    set heading 0
    set size 20
    setxy 165 45
  ]
  create-objfronts 1 ; text for next level
  [
    set shape "next"
    set heading 0
    set size 10
    setxy 159 57
  ]
  create-objfronts 1 ; text for next level
  [
    set shape "level"
    set heading 0
    set size 10
    setxy 171 57
  ]

end

; UPDATE APPLES TO DETECT PLAYERS NEARBY
to update-apples

  ask objfronts with [ shape = "apple" ] ; detect nearby players
  [
    if any? players in-radius 3
    [
      ask player 0 [
        sound:play-note "Applause" 60 256 .3 ; eat apple noise
        set health health + 20 ; give them 20 health but keep their health <= 100
        if health > 100
        [
          set health 100
        ]
      ]
      die ; kill the apple, it has been "eaten"
    ]
  ]

end

; UPDATE PLAYER AND BOSS STATS
to update-stats

  ; update player stats
  ask player 0
  [
    if health <= 0 ; if the player is dead
    [
      set health 0 ; set health to 0 for organizational purposes
      set game_status "game over" ; set the game status to game over
      ask patch 105 105 [ set pcolor black ] ; setup for the game over animation
      ask healthbar [ ht ] ; hide the health bar
      reset-perspective ; reset the perspective
    ]
    ask healthbar ; update the healthbar to reflect the current health of the player
    [
      set size (7 * ([health] of myself / [max_health] of myself)) ; adjust size
      ; untie, move the healthbar accordingly, and then retie the link
      ask link-with player 0 [untie]
      setxy ([xcor] of player 0 - 3 * (1 - ([health] of myself / [max_health] of myself))) (ycor)
      ask link-with player 0 [tie]
    ]
  ]
  ; update the boss stats
  ask one-of enemies with [et = "minotaur"]
  [
    if health <= 0 ; if the boss is dead
    [
      sound:play-note "Gunshot" 20 128  1 ; play a noise
      ask link-neighbors [die] ; kill the healthbars
      hatch-objfronts 1 ; spawn the key
      [
        set shape "key"
        set color grass_color
        set size 5
      ]
      set game_status "win" ; set the game status to win
      die ; die
    ]
    ask healthbar ; update the healthbar to reflect boss health
    [
      set size (7 * ([health] of myself / [max_health] of myself)) ; set size accordingly
      ; untie, move the healthbar, and retie the link as needed
      ask link-with myself [untie]
      setxy ([xcor] of myself - 3 * (1 - ([health] of myself / [max_health] of myself))) ([ycor] of myself + 7)
      ask link-with myself [tie]
    ]
  ]

end



; -------------------------------------------------------------------------------
; ENEMY BEHAVIOR
; -------------------------------------------------------------------------------

; SET SCORES FOR EACH PATCH IN TERMS OF DISTANCE BY MAZE TRAVERSAL FROM THE PLAYER
to set-pscores

  let pathdist 1 ; how far the algorithm moves with each step
  ask patches ; set all the patches to unvisited with an unviable score
  [
    set score_visited? false
    set score -1
  ]
  ask enemies
  [ set trace-score 1000 ]

  ; setup the queue
  let q list 0 0
  repeat 2 [set q remove-item 0 q]

  ; find the source patch and add the nearest patch in the middle of the path to the queue
  let source ([patch-here] of player 0)
  let closest_source (min-one-of patches with [not wall? and not wall-node? and (pxcor mod (spacing / 2) = 0 or pycor mod (spacing / 2) = 0)][distance source])
  ask closest_source
  [
    set score 0 ; set the score to be 0
    set score_visited? true ; mark the patch as visited
    set prev_score_node self ; this is the first node
  ]
  set q lput closest_source q ; add it to the queue

  while [ length q > 0 ] ; while there are items in the queue
  [
    let curr_patch item 0 q ; pop a patch from the queue
    set q remove-item 0 q

    ask curr_patch
    [
      set score_visited? true ; mark it as visited
      set score ([score] of prev_score_node) + 1 ; set its score to be one more than the patch that passed it in
      ; put nearby unvisited patches in the middle of the path into the queue
      ask patches in-radius pathdist with [distance myself = pathdist and not score_visited? and not wall? and not wall-node? and (pxcor mod (spacing / 2) = 0 or pycor mod (spacing / 2) = 0)] ; adjust for if you chance spacing
      [
        if distance player 0 < 50 ; only add it to the queue if it is close to the player
        [
          set prev_score_node myself ; assign the previous node
          set q lput self q ; add it to the queue
        ]
      ]
    ]
  ]

end

; DETERMINE BOSS BEHAVIOR
to boss-behavior

  ask enemies with [et = "minotaur"]
  [
    ; first check for incoming projectiles
    if any? projs in-radius 4
    [
      ask one-of projs in-radius 4
      [
        ifelse proj_type = "arrow" ; hit by an arrow
        [
          ask myself [set health health - 25]
        ]
        [ ; hit by a sword slash
          ask myself [set health health - 50]
        ]
        die ; ask the projectile to die
      ]
    ]
    ; attacking
    if ticks mod 3 = 0
    [
      if any? players in-radius 20 ; if the player is within 20 patches, attack the player
      [
        if ticks mod 3 ^ 6 = 0 ; this allows for time between attacks
        [
          hatch-projs 1 ; create a slash projectile
          [
            set size 20
            set shape "slash"
            set color yellow
            set proj_type "axeslash" ; set the projectile type accordingly
            ask myself ; set direction to be towards the player
            [
              if (towards player 0 >= 45) and (towards player 0 <= 135) ; right
              [ask myself [ set heading 90 ]]
              if (towards player 0 >= 135) and (towards player 0 <= 225) ; down
              [ask myself [ set heading 180 ]]
              if (towards player 0 >= 225) and (towards player 0 <= 315) ; left
              [ask myself [ set heading 270 ]]
              if (towards player 0 >= 315) or (towards player 0 <= 45) ; up
              [ask myself [ set heading 0 ]]
            ]
            fd 5 ; start forward a few patches
          ]
        ]
      ]
      ; movement
      ifelse (distance player 0 < 50) ; if the player is in range, follow it
      [
        boss-follow
      ]
      [ ; if the player is out of range, move randomly
        random-movement
      ]
    ]
  ]

end

; BOSS BEHAVIOR TO FOLLOW THE PLAYER
to boss-follow

  ; boss slowly follows the player, destroying walls in the way
  let next_patch patch-at-heading-and-distance (towards player 0) (1) ; get the next patch towards tha player
  ifelse not [wall?] of next_patch ; if the next patch is not a wall, move there
  [
    set heading 0
    boss-shape patch-here next_patch ; update the boss sprite
    setxy (xcor + ([pxcor] of next_patch - xcor) / spd) (ycor + ([pycor] of next_patch - ycor) / spd) ; move
  ]
  [
    ; if the next patch is a wall patch
    ask next_patch
    [
      ifelse not wall-node? ; if its not a wall-node, destroy it
      [
        set destroy? true
        make-newpath true ; make a new path to replace the one being destroyed
      ]
      [ ask myself [setxy (xcor + 1 - random 3) (ycor + 1 - random 3) ]] ; if it is a wall node, move around it
    ]
  ]

end

; UPDATE THE BOSS SHAPE ACCORDING TO MOVEMENT
to boss-shape [prev_p next_p]

  if texturize ; only update if the user has that option selected
  [
    ifelse ([pxcor] of next_p - [pxcor] of prev_p > 0)
    [
      set shape "minotaur-right"
    ]
    [
      ifelse ([pxcor] of next_p - [pxcor] of prev_p < 0)
      [
        set shape "minotaur-left"
      ]
      [
        ifelse ([pycor] of next_p - [pycor] of prev_p > 0)
        [
          set shape "minotaur-up"
        ]
        [
          if ([pycor] of next_p - [pycor] of prev_p < 0)
          [
            set shape "minotaur-down"
          ]
        ]
      ]
    ]
  ]

end

; RANDOM MOVEMENT FOR ALL ENEMIES
to random-movement

  if (et = "ant") ; if its an ant, physically turn around when you hit a wall
  [
    while [([wall?] of patch-ahead 1)] ; turn randomly until you're away from a wall
    [ rt random 360 ]

    ;if [wall?] of patch-here [rt 180 fd 1 show myself] ; if youre on a wall for some reason, turn around
    fd ant_speed ; move forward
  ]
  if (et = "minotaur") ; if its the boss, adjust the current direction it is moving in
  [
    ; while the current direction isn't valid, generate a new one
    while [[wall?] of patch (xcor + item 0 curr_dir) (ycor + item 1 curr_dir) or (item 0 curr_dir = 0 and item 1 curr_dir = 0)]
    [ set curr_dir list (1 - random 3) (1 - random 3) ]
    boss-shape (patch-here) (patch (xcor + item 0 curr_dir) (ycor + item 1 curr_dir)) ; adjust boss shape according to movement
    setxy (xcor + item 0 curr_dir / spd) (ycor + item 1 curr_dir / spd) ; and move there

  ]

end

; DETERMINE ANT BEHAVIOR
to ant-behavior

  ; ask all the ants to check if they're dead
  ask enemies with [et = "ant"]
  [
    if health <= 0 ; if they are dead
    [
      ;sound:play-drum "Side Stick" 40 ; make a noise
      if random 3 < 1 ; chance to drop an apple
      [
        hatch-objfronts 1
        [
          set heading 0
          set shape "apple"
          set color red
          set size 4
        ]
      ]
      die ; die
    ]
    if [wall?] of patch-here ; kill ants that get stuck in walls or walls build over them
    [ die ]
  ]

  if ticks mod 3 = 0 ; update collision detection every 3 ticks
  [
    ask enemies with [et = "ant"][
      if any? projs in-radius 3
      [
        ask one-of projs in-radius 3
        [
          ifelse proj_type = "arrow" ; hit by an arrow
          [ ask myself [set health health - 25] ]
          [ ask myself [set health health - 50] ] ; hit by a sword slash
          die ; kill the projectile
        ]
      ]
      if any? players in-radius 1 ; if the player is in the radius, deal slow damage to it
      [
        ask player 0
        [ set health health - .1 ]
      ]
    ]
    ; ask ants near the player to trace the patch score path
    if (any? enemies with [distance player 0 < 5])
    [
      ask enemies with [distance player 0 < 5]
      [
        face player 0
        fd ant_speed
      ]
    ]
    if (any? enemies with [distance player 0 < 50 and distance player 0 >= 5])
    [
      ask enemies with [distance player 0 < 50 and distance player 0 >= 5 and et = "ant"]
      [
        follow-player
      ]
    ]
    ; random movement- enemies far from the player move randomly
    if (any? enemies with [distance player 0 >= 50])
    [
      ask enemies with [distance player 0 >= 50 and et = "ant"]
      [
        random-movement
      ]
    ]
  ]

end

; ANT BEHAVIOR FOR FOLLOWING THE PLAYER
to follow-player

  ; get the next patch

  let dest min-one-of patches in-radius (spacing / 4) with [score >= 0] [score]
  ifelse dest != nobody
  [
    face dest ; face that patch
    ifelse (no-walls-between .1 heading and not [wall?] of patch-ahead .1) ; and if you can move forward, do it
    [
      fd ant_speed
      set trace-score [score] of dest ; update the ant's trace score
    ]
    [ ; if it is a wall, turn around and move forward a bit
      rt 180
      repeat 3 [if not [wall?] of patch-ahead 1 [fd 1]]
    ]
  ]
  [ random-movement ]

end

; REPORTER FOR IF THERE ARE ANY WALLS BETWEEN THE CURRENT PATCH AND THE ONE AT A DISTANCE AND ANGLE
to-report no-walls-between [dist_ angle]

  let i 1
  while [i <= dist_] ; iterate through every patch between the one the agent is on and the destination patch
  [
    if [wall?] of ([patch-right-and-ahead angle i] of self) ; check if there is a wall, if there is report false
    [ report false ]
    set i (i + 1)
  ]
  report true ; if the loop doesn't return false at some point, return true

end



; -------------------------------------------------------------------------------
; LABYRINTH MANAGEMENT
; -------------------------------------------------------------------------------

; CHANGE THE LABYRINTH
to evolve-labyrinth


  if game_status = "alive" ; only make new connections if the game is active
  [ make-newpath false ]; make a new connection

  if game_status != "game_over" and game_status != "empty" and game_status != "done" ; make sure the game isn't in one of these states
  [ if any? patches with [wall?] [destroy-labyrinth] ] ; destroy a connections

  ;if conn_count <

end

; DESTROY SOME WALL-NODE CONNECTIONS
to destroy-labyrinth

  ; ask 20 walls
  if create-or-destroy? ; odds of destroying is calculated with the create-or-destroy? method
  [
    ask one-of patches with [wall? and not wall-node?]
    [
      set destroyed destroyed + 1 ; incrmement the number of destroyed connections
      set conn_count conn_count - 1 ; decrease the connection count

      ; assign the two nearest wall-nodes as those with the connection
      let wallnode1 nobody
      let wallnode2 nobody
      ask min-one-of patches with [wall-node?] [distance myself][
        set wallnode1 self
      ]
      ask min-one-of patches with [wall-node? and not (self = wallnode1)] [distance myself][
        set wallnode2 self
      ]

      ; remove the connection betweeen the two selected wall nodes
      if member? wallnode2 [connections] of wallnode1
      [
        ask wallnode1 [set connections remove wallnode2 connections]
        ask wallnode2 [set connections remove wallnode1 connections]

      ]
      ; set the wall to destroy itself and the walls around it with the same connection id
      set destroy? true
    ]
  ]

  ; ask non wall-node (and non edge-node) wall patches with destroy selected to destroy themselves and spread the destruction to nearby walls of the same connection id
  ask patches with [wall? and destroy? and not wall-node? and not (pxcor = spacing or pxcor = spacing * xnodes or pycor = spacing or pycor = spacing * ynodes) ]
  [
    ; slow down wall growth for visual aesthetics
    if ticks mod 10 = 0
    [
      set-grass
      ask neighbors with [wall? and not wall-node? and (conn_id = [conn_id] of myself)]
      [
        set destroy? true
      ]
    ]
  ]

end

; MAKE NEW CONNECTIONS BETWEEN NODES
to make-newpath [ override ]

  ; ask 20 wall-nodes
  if create-or-destroy? or override ; odds of creating a new connection is calculated with the create-or-destroy? method
  [
    ; choose a wall-node with the lowest number of connections (and making sure it doesnt make a wall through the player)
    ask min-one-of patches with [wall-node? and (pxcor != [xcor] of player 0 and pycor != [ycor] of player 0)] [length connections]
    [
      ; select a nearby wall-node to create a connection with
      ask one-of other patches in-radius spacing with [wall-node?]
      [
        set created created + 1 ; increment the created nodes count
        set conn_count conn_count + 1 ; increment the connection count
        create-conn myself self ; create the connection between the two nodes
        set new-connections lput (list myself self) new-connections ; add the two wall nodes for the new connection to this list so we can visualize the new connections
      ]
    ]
  ]

  ; slow down for animation
  if ticks mod 10 = 0
  [
    vis-new-connections ; now we visualize all the new wall connections
  ]

end

; VISUALIZE / REALIZE THE NEW CONNECTIONS
to vis-new-connections

  let i 0
  while [i < length new-connections] ; iterate through the new connections
  ;if length new-connections > 0
  [
    set i random (length new-connections)
    ; select the current connection we want to visualize
    let curr_pair (item i new-connections)
    ask item 0 curr_pair
    [
      ; get the direction from the first wall-node in the connection to the second
      let dir list (([pxcor] of (item 1 curr_pair) - [pxcor] of (item 0 curr_pair)) / spacing) (([pycor] of (item 1 curr_pair) - [pycor] of (item 0 curr_pair)) / spacing) ; 1 0, -1 0, 0 1, 0 -1, 1 1, etc.
      while [(item 0 dir) = 0 and (item 1 dir) = 0]
      [
        set dir list (([pxcor] of (item 1 curr_pair) - [pxcor] of (item 0 curr_pair)) / spacing) (([pycor] of (item 1 curr_pair) - [pycor] of (item 0 curr_pair)) / spacing) ; 1 0, -1 0, 0 1, 0 -1, 1 1, etc.
      ]
      ; check to make sure it isnt fully built: check the final patch that would need to be created
      ifelse(not [wall?] of patch ([pxcor] of (item 1 curr_pair) - item 0 dir) ([pycor] of (item 1 curr_pair) - item 1 dir))
      [
        ; make progress from bridging a wall from item 0 to item 1 until the wall is complete
        ask (get-next-patch (item 0 curr_pair) (dir)) ; access the next patch in the given direction
        [
          if not [wall-node?] of self ; check to make sure it isn't a wall-node
          [
            ; initialize the wall instance variables
            set wall-dir dir
            set-wall
            let firstpatch patch ([pxcor] of (item 0 curr_pair) + item 0 dir) ([pycor] of (item 0 curr_pair) + item 1 dir) ; get the first patch of the connection
            ifelse ([wall?] of firstpatch)
            [ set conn_id ([conn_id] of firstpatch)] ; if the first patch is a wall, use that connection id
            [
              ; if not, then this is the first patch
              set conn_id conn_count ; set the connection id of the new wall
            ]
          ]
        ]
      ]
      [ ; if the connection fully built, remove it from the new connections list
        set new-connections remove-item i new-connections
        set i i - 1 ; also decrement i so that we dont skip the next connection
      ]
    ]
   set i i + 1
  ]

  ; if the wall is texturized, distort it
  if texturize
  [
    repeat 2 [grow-maze ("not distorted")]
    ask patches with [wall?]
    [ set distorted? true ]
  ]
end

; REPORT WHETHER OR NOT TO CREATE OR DESTROY A CONNECTION
to-report create-or-destroy?
  report (random-float ((100 / mazeRate) * (5000 / maze-size))) < 1
end

; REPORTER TO RETURN THE NEXT WALL PATCH IN A GIVEN DIRECTION - used for making new walls
to-report get-next-patch [curr_patch dir]

  ifelse [wall?] of curr_patch ; if the current patch is a wall
  [
    ; get-next patch of the next patch in the specified direction
    report get-next-patch (patch ([pxcor] of curr_patch + item 0 dir) ([pycor] of curr_patch + item 1 dir)) dir
  ]
  [ ; base case
    report curr_patch
  ]
end

; SET THE PATCH AS A WALL PATCH
to set-wall

  set wall? true
  ; color
  ifelse texturize
  [ set pcolor random-float 10 ]
  [ set pcolor 1 ]

end

; SET THE PATCH AS A GRASS PATCH
to set-grass

  ; reset instance variables
  set wall? false
  set distorted? false
  set wall-dir list 0 0
  set destroy? false
  set pcolor grass_color

end



; -------------------------------------------------------------------------------
; PLAYER MOVEMENT
; -------------------------------------------------------------------------------

; FUNCTION TO CLEAR THE SCREEN
to clear

  ask patches[
    set pcolor green
    set wall? false;
  ]

end

; REPORT IF THE PATCH AHEAD IS A WALL
to-report check-ahead [dir]

  if not [wall?] of patch (xcor + item 0 dir) (ycor + item 1 dir)
  [ report true ]
  report false

end

; UPDATE MOVEMENT FOR MOMENTUM BASED MOVEMENT
to move

  if game_status = "alive" or game_status = "win" or game_status = "empty" ; only allow movement in one of these game states
  [
    if momentum
    [
      if (item 0 velocity < max_spd or deltaX < 0) and (item 0 velocity > -1 * max_spd or deltaX > 0) ; if we can increment x velocity, do it
      [set velocity replace-item 0 velocity (item 0 velocity + deltaX)]
      if (item 1 velocity < max_spd or deltaY < 0) and (item 1 velocity > -1 * max_spd or deltaY > 0) ; if we can increment y velocity, do it
      [set velocity replace-item 1 velocity (item 1 velocity + deltaY)]

      ask player 0 ; move in the given direction unless there is a wall there
      [
        if not [wall?] of patch (xcor + item 0 velocity) (ycor + item 1 velocity)
        [ setxy (xcor + item 0 velocity) (ycor + item 1 velocity)]
        if item 0 velocity != 0 and item 1 velocity != 0
        [ sound:play-note "Applause" 40 128 .3 ] ; movement noise
      ]

      ; apply friction on velocity and reset acceleration
      set velocity replace-item 0 velocity ((item 0 velocity * 90) / 100)
      set velocity replace-item 1 velocity ((item 1 velocity * 90) / 100)
      set deltaX 0
      set deltaY 0
    ]
  ]

end

; MOVE UP
to up

  carefully ; catch if the player is dead
  [
    if game_status = "alive" or game_status = "win" or game_status = "empty" ; only allow movement in one of these game states
    [
      ask player 0 [ set facing 0 set shape "player-up"] ; adjust player shape and facing variable
      ifelse equip = "sword"
      [
        ask one-of objfronts with [obj_type = "sword"] [ ht ] ; update weapon visuals
        ask one-of objbehinds with [obj_type = "sword"] [ st ] ; update weapon visuals
      ]
      [
        if equip = "bow"
        [
          ask one-of objfronts with [obj_type = "bow"] [ ht ] ; update weapon visuals
          ask one-of objbehinds with [obj_type = "bow"] [ st ] ; update weapon visuals
        ]
      ]
      ifelse not momentum ; if static movement, just move up
      [
        sound:play-note "woodblock" 5 90 .1 ; movement noise
        ask player 0
        [
          if no-walls-between (pspeed) (0) ; make sure you arent running into a wall
          [ set ycor ycor + pspeed ]
        ]
      ]
      [ set deltaY .3 ] ; if momentum-based movement, accelerate up
    ]
  ][]
end

; MOVE RIGHT
to right_

  carefully ; catch if the player is dead
  [
    if game_status = "alive" or game_status = "win" or game_status = "empty" ; only allow movement in one of these game states
    [
      ask player 0 [ set facing 90 set shape "player-right"]  ; adjust player shape and facing variable
      vis-obj-right ; update weapon visuals

      ifelse not momentum ; if static movement, just move right
      [
        sound:play-note "woodblock" 5 90 .1 ; movement noise
        ask player 0
        [
          if no-walls-between (pspeed) (90) ; make sure you arent running into a wall
          [ set xcor xcor + pspeed ]
        ]
      ]
      [ set deltaX .3 ] ; if momentum-based movement, accelerate right
    ]
  ][]

end

; CHANGE WEAPON VISUALS FOR MOVING RIGHT
to vis-obj-right

  ; adjust each weapon, shown or not, to be in the 'right' position

  ask one-of objfronts with [obj_type = "sword"]
  [
    ask link-with player 0
    [ untie ]
    setxy ([xcor] of player 0 + 2) ([ycor] of player 0 + .5)
    set heading 25
    ask link-with player 0
    [ tie ]
  ]
  ask one-of objbehinds with [obj_type = "sword"]
  [
    ask link-with player 0
    [ untie ]
    setxy ([xcor] of player 0 + 2) ([ycor] of player 0 + .5)
    set heading 25
    ask link-with player 0
    [ tie ]
  ]

  ask one-of objfronts with [obj_type = "bow"]
  [
    ask link-with player 0
    [ untie ]
    setxy ([xcor] of player 0 + 1) ([ycor] of player 0 - .5)
    set heading 0 + 45
    ask link-with player 0
    [ tie ]
  ]
  ask one-of objbehinds with [obj_type = "bow"]
  [
    ask link-with player 0
    [ untie ]
    setxy ([xcor] of player 0 + 1) ([ycor] of player 0 - .5)
    set heading 0 + 45
    ask link-with player 0
    [ tie ]
  ]

end

; MOVE LEFT
to left_

  carefully ; catch if the player is dead
  [
    if game_status = "alive" or game_status = "win" or game_status = "empty" ; only allow movement in one of these game states
  [
      ask player 0 [ set facing 270 set shape "player-left"] ; adjust player shape and facing variable
      vis-obj-left ; update weapon visuals

      ifelse not momentum ; if static movement, just move left
      [
        sound:play-note "woodblock" 5 90 .1 ; movement noise
        ask player 0
        [
          if no-walls-between (pspeed) (270) ; make sure you arent running into a wall
          [ set xcor xcor - pspeed ]
        ]
      ]
      [ set deltaX -.3 ] ; if momentum-based movement, accelerate left
    ]
  ][]
end

; CHANGE WEAPON VISUALS FOR MOVING LEFT
to vis-obj-left

  ; set all weapon objects, shown or not, to be in the 'left' position

  ask one-of objfronts with [obj_type = "sword"]
  [
    ask link-with player 0
    [ untie ]
    setxy ([xcor] of player 0 - 2) ([ycor] of player 0 + .5)
    set heading -25
    ask link-with player 0
    [ tie ]
  ]
  ask one-of objbehinds with [obj_type = "sword"]
  [
    ask link-with player 0
    [ untie ]
    setxy ([xcor] of player 0 - 2) ([ycor] of player 0 + .5)
    set heading -25
    ask link-with player 0
    [ tie ]
  ]

  ask one-of objfronts with [obj_type = "bow"]
  [
    ask link-with player 0
    [ untie ]
    setxy ([xcor] of player 0 - 1) ([ycor] of player 0 - .5)
    set heading 180 - 45
    ask link-with player 0
    [ tie ]
  ]
  ask one-of objbehinds with [obj_type = "bow"]
  [
    ask link-with player 0
    [ untie ]
    setxy ([xcor] of player 0 - 1) ([ycor] of player 0 - .5)
    set heading 180 - 45
    ask link-with player 0
    [ tie ]
  ]

end

; MOVE DOWN
to down

  carefully ; catch if the player is dead
  [
    if game_status = "alive" or game_status = "win" or game_status = "empty" ; only allow movement in one of these game states
    [
      ask player 0 [ set facing 180 set shape "player-down"] ; adjust player shape and facing variable
      ifelse equip = "sword"
      [
        ask one-of objfronts with [obj_type = "sword"] [ st ] ; adjust weapon visuals
        ask one-of objbehinds with [obj_type = "sword"] [ ht ] ; adjust weapon visuals
      ]
      [
        if equip = "bow"
        [
          ask one-of objfronts with [obj_type = "bow"] [ st ] ; adjust weapon visuals
          ask one-of objbehinds with [obj_type = "bow"] [ ht ] ; adjust weapon visuals
        ]
      ]

      ifelse not momentum ; if static movement, just move down
      [
        sound:play-note "woodblock" 5 90 .1 ; movement noise
        ask player 0
        [
          if no-walls-between (pspeed) (180) ; make sure you arent running into a wall
          [ set ycor ycor - pspeed ]
        ]
      ]
      [ set deltaY -.3 ] ; if momentum-based movment, accelerate down
    ]
  ][]
end



; -------------------------------------------------------------------------------
; WEAPONS
; -------------------------------------------------------------------------------

; CHANGE BETWEEN BOW AND SWORD
to change-eqp

  ; hide all weapons
  ask one-of objfronts with [obj_type = "bow"] [ ht ]
  ask one-of objbehinds with [obj_type = "bow"] [ ht ]
  ask one-of objfronts with [obj_type = "sword"] [ ht ]
  ask one-of objbehinds with [obj_type = "sword"] [ ht ]

  ; if using the sword, equip the bow
  ifelse equip = "sword"
  [
    set equip "bow"
    ifelse [facing] of player 0 = 0
    [ ask one-of objbehinds with [obj_type = "bow"] [ st ] ]
    [ ask one-of objfronts with [obj_type = "bow"] [ st ] ]
  ]
  [ ; if using the bow, equip the sword
    if equip = "bow"
    [
      set equip "sword"
      ifelse [facing] of player 0 = 0
      [ ask one-of objbehinds with [obj_type = "sword"] [ st ] ]
      [ ask one-of objfronts with [obj_type = "sword"] [ st ] ]
    ]
  ]

end

; ATTACK FUNCTION
to use-slot1

  ifelse equip = "sword" ; sword attack
  [
    if ticks > attack_cooldown + 500 ; 500 tick cooldown
    [
      sword-slash ; call the slash mechanism
      set attack_cooldown ticks
    ]
  ]
  [ ; bow attack
    if ticks > attack_cooldown + 250 ; 250 tick cooldown
    [
      bow-shot ; call the shoot mechanism
      set attack_cooldown ticks
    ]
  ]

end

; SHOOT THE BOW
to bow-shot

  ; create a new arrow and set it in the direction the player is facing
  sound:play-note "Gunshot" 120 40 .25 ; play a noise
  create-projs 1
  [
    setxy ([xcor] of player 0) ([ycor] of player 0)
    set size 3
    set shape "bow-arrow"
    set color grey
    set proj_type "arrow"
    set heading [facing] of player 0
    fd 2
    if ycor > [ycor] of player 0 [fd 3] ; adjust for the visual effect when looking up
  ]

end

; ATTACK WITH THE SWORD
to sword-slash

  ; create a 'slash' projectile in the direction the player is facing
  sound:play-note "Reverse Cymbal" 64 128 .2 ; sword slash noise
  create-projs 1
  [
    setxy ([xcor] of player 0) ([ycor] of player 0 - .5)
    set size 5
    set shape "slash"
    set color white
    set proj_type "swordslash"
    set heading [facing] of player 0
    fd 4
  ]

end

; CHECK FOR PROJECTILE COLLISIONS
to update-projs

  ask projs with [shape = "slash"] ; slash updates
  [
    if size = 0 [ die ] ; if too small, die
    if [wall?] of patch-here ; if it hits a wall, die, but has a chance to destroy the wall
    [
      if random 3 < 1
      [ ask patch-here [ set destroy? true ] ] ; chance for sword slashes to destroy a wall
      die
    ]
    ifelse proj_type = "swordslash" ; if its a sword slash, check for those collisions
    [ swordslash-collisions ]
    [ axeslash-collisions ] ; otherwise check for axe collisions

    if ticks mod 100 = 0 ; every 100 ticks, make it smaller and move forward 1
    [
      fd 1
      set size size - 1
    ]
  ]

  ; arrow behavior
  ask projs with [shape = "bow-arrow"]
  [
    if [wall?] of patch-here ; if it hits a wall, the arrow breaks
    [ die ]
    arrow-collisions ; check for arrow collisions
    if ticks mod 10 = 0 ; every 10 ticks, move forward one unit
    [ fd 1 ]
  ]

end

; ARROW COLLISIONS
to arrow-collisions

  ifelse heading = 90 or heading = 270 ; if facing horizontally
  [
    if any? enemies in-radius (size / 2) with [abs(ycor - [ycor] of myself) < 2] ; check for enemies in radius but squish vertically bc its an arrow
    [
      ask one-of enemies in-radius (size / 2) with [abs(ycor - [ycor] of myself) < 2]
      [
        set health (health - 25) ; deal 20 damage
        set heading towards player 0 - 180 ; turn the enemy away
      ]
      die ; kill the arrow
    ]
  ]
  [ ; if facing vertically
    if any? enemies in-radius (size / 2) with [abs(xcor - [xcor] of myself) < 2] ; check for enemies in radius but squish horizontally bc its an arrow
    [
      ask one-of enemies in-radius (size / 2) with [abs(xcor - [xcor] of myself) < 2]
      [
        set health (health - 25)
        set heading towards player 0 - 180
      ]
      die ; kill the arrow
    ]
  ]

end

; SWORD COLLISIONS
to swordslash-collisions

  ; check in radius and then use the heading of the slash to check whether its close enough in the x or y directions
  ifelse heading = 90 or heading = 270 ; facing horizontally
  [
    if any? enemies in-radius (size / 2) with [abs(xcor - [xcor] of myself) < 2] ; check if its in the radius and then make sure the enemy is horizontally close enough
    [
      ask one-of enemies in-radius (size / 2) with [abs(xcor - [xcor] of myself) < 2]
      [
        set health (health - 50) ; deal 50 damage
        if et = "ant" ; make ants turn and run if they survive
        [
          set heading towards player 0 - 180
          repeat 5 [ if not [wall?] of patch-ahead 1 [ fd 1 ] ]
        ]
      ]
      die ; kill the projectile
    ]
  ]
  [ ; facing vertically
    if any? enemies in-radius (size / 2) with [abs(ycor - [ycor] of myself) < 2] ; check if its in the radius and then make sure the enemy is vertically close enough
    [
      ask one-of enemies in-radius (size / 2) with [abs(ycor - [ycor] of myself) < 2]
      [
        set health (health - 50) ; deal 50 damage
        if et = "ant" ; make ants turn and run if they survive
        [
          set heading towards player 0 - 180
          repeat 5 [ if not [wall?] of patch-ahead 1 [ fd 1 ] ]
        ]
      ]
      die ; kill the projectile
    ]
  ]

end

; AXE COLLISIONS
to axeslash-collisions

  if proj_type = "axeslash"
  [
    ifelse heading = 90 or heading = 270 ; if facing horizontally
    [
      if any? players in-radius (size / 2) with [abs(xcor - [xcor] of myself) < 2] ; same logic as with swordslash collisions but detecting the player not enemies
      [
        ask one-of players in-radius (size / 2) with [abs(xcor - [xcor] of myself) < 2]
        [
          set health health - 30 ; deal 30 damage to the player
        ]
        die ; kill the projectile
      ]
    ]
    [ ; facing vertically
      if any? players in-radius (size / 2) with [abs(ycor - [ycor] of myself) < 2] ; same logic as with swordslash collisions but detecting the player not enemies
      [
        ask one-of players in-radius (size / 2) with [abs(ycor - [ycor] of myself) < 2]
        [
          set health health - 30 ; deal 30 damage to the player
        ]
        die ; kill the projectile
      ]
    ]
  ]

end
@#$#@#$#@
GRAPHICS-WINDOW
230
23
871
665
-1
-1
3.0
1
10
1
1
1
0
0
0
1
0
210
0
210
0
0
1
ticks
30.0

BUTTON
73
77
158
110
NIL
init-level0
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
74
218
149
251
NIL
up
NIL
1
T
OBSERVER
NIL
W
NIL
NIL
1

BUTTON
112
254
176
287
NIL
right_
NIL
1
T
OBSERVER
NIL
D
NIL
NIL
1

BUTTON
47
254
110
287
NIL
left_
NIL
1
T
OBSERVER
NIL
A
NIL
NIL
1

BUTTON
82
289
145
322
NIL
down
NIL
1
T
OBSERVER
NIL
S
NIL
NIL
1

BUTTON
55
171
176
204
Start / Pause
play
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SWITCH
61
32
173
65
texturize
texturize
0
1
-1000

SWITCH
26
412
202
445
momentum
momentum
1
1
-1000

BUTTON
69
370
152
404
attack!
use-slot1\n
NIL
1
T
OBSERVER
NIL
F
NIL
NIL
1

BUTTON
54
333
167
366
switch weapon
change-eqp
NIL
1
T
OBSERVER
NIL
E
NIL
NIL
1

TEXTBOX
1158
66
1308
84
NIL
11
0.0
1

BUTTON
924
139
1122
172
Kill Boss (for testing purposes)
ask one-of enemies with [et = \"minotaur\"][set health 0]
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

TEXTBOX
900
85
1148
169
This button is for testing/grading purposes only, if you are having difficulty killing the boss and want to see the game progression past that point
11
0.0
1

@#$#@#$#@
# GAMEPLAY GUIDE

## Quick Setup

**Momentum Switch**: select momentum or static based movement (momentum is faster but more difficult to control)
**Texturize Switch**: select either simple sprites and map visuals or more complicated ones
**Init-Level0**: Setup the game for the first level!
**Start/Pause**: The equivalent of a "play" button- starts and stops gameplay

## Controls

Up/Left/Down/Right **movement** is controlled by **W/A/S/D** keyboard presses
To **switch** your weapon between your bow and sword, press **E**
To use your weapon to **attack**, press **F**
	sword attacks deal 50 damage but are slow and at limited range
	bow shots deal 25 damage, are quicker, and have unlimited range

## Maze Behavior

As you play, you may notice the maze is changing. This is a base mechanic of the game and you will need to navigate the maze to find and defeat the boss. However, you can use your sword to destroy a wall if you need to- it may take a few swipes though.

## Enemy Behavior

### Minions

The bugs move randomly thoughout the maze until you get within a certain distance of them, then they'll track you through the maze until you either kill them or escape. individual bugs deal low damage and have low health, but watch out for groups!

Level 1 bugs have 50 health and a 33% chance to drop a healing apple upon death.

### Boss

The Minotaur also moves randomly through the maze until it hears you, at which point it'll chase you, destroying labyrinth walls in its way, until you kill it or escape. Luckily, the Minotaur is slow, but his axe slashes deal heavy damage so watch out.

The level 1 Minotaur has 500 health and deals 30 damage per hit
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

ant 2
true
0
Polygon -7500403 true true 150 19 120 30 120 45 130 66 144 81 127 96 129 113 144 134 136 185 121 195 114 217 120 255 135 270 165 270 180 255 188 218 181 195 165 184 157 134 170 115 173 95 156 81 171 66 181 42 180 30
Polygon -7500403 true true 150 167 159 185 190 182 225 212 255 257 240 212 200 170 154 172
Polygon -7500403 true true 161 167 201 150 237 149 281 182 245 140 202 137 158 154
Polygon -7500403 true true 155 135 185 120 230 105 275 75 233 115 201 124 155 150
Line -7500403 true 120 36 75 45
Line -7500403 true 75 45 90 15
Line -7500403 true 180 35 225 45
Line -7500403 true 225 45 210 15
Polygon -7500403 true true 145 135 115 120 70 105 25 75 67 115 99 124 145 150
Polygon -7500403 true true 139 167 99 150 63 149 19 182 55 140 98 137 142 154
Polygon -7500403 true true 150 167 141 185 110 182 75 212 45 257 60 212 100 170 146 172

apple
true
0
Polygon -7500403 true true 45 180 45 150 60 105 75 90 105 75 135 90 150 90 150 180 60 180
Polygon -6459832 true false 135 15 120 30 135 90 150 90 150 45
Polygon -10899396 true false 150 45 165 30 180 30 165 60 150 75 150 45
Polygon -7500403 true true 255 180 255 150 240 105 225 90 195 75 165 90 150 90 150 180 240 180
Polygon -7500403 true true 45 180 45 210 60 240 75 255 105 270 135 270 150 270 150 180 60 180
Polygon -7500403 true true 255 180 255 210 240 240 225 255 195 270 165 270 150 270 150 180 240 180
Rectangle -7500403 true true 45 165 255 195
Rectangle -7500403 true true 135 90 165 270

axe
true
3
Polygon -6459832 true true 120 210
Polygon -6459832 true true 135 120 135 255 165 255 165 120 150 120
Polygon -7500403 true false 165 60 180 60 195 45 210 15 225 30 240 60 240 105 225 135 210 150 195 120 180 105 165 105 165 60
Polygon -7500403 true false 135 60 120 60 105 45 90 15 75 30 60 60 60 105 75 135 90 150 105 120 120 105 135 105 135 60
Polygon -7500403 true false 180 60 165 45 135 45 120 60 120 105 135 120 165 120 180 105

boss-basic
true
0
Circle -7500403 true true 73 73 152
Polygon -7500403 true true 210 105 225 90 240 60 240 45 240 30 225 45 225 60 210 75 180 90 210 105
Polygon -7500403 true true 90 105 75 90 60 60 60 45 60 30 75 45 75 60 90 75 120 90 90 105
Polygon -2674135 true false 105 135 135 135 135 150 105 135
Polygon -2674135 true false 165 135 165 150 195 135 165 135
Polygon -2674135 true false 135 150 105 150 105 135
Polygon -2674135 true false 165 150 195 150 195 135
Polygon -16777216 true false 105 105 150 120 195 105 210 120 150 135 90 120 90 120

bow
true
15
Polygon -6459832 true false 150 15 165 15 195 30 225 60 240 150 225 225 195 270 165 285 150 285 135 285 150 270 165 270 195 255 210 225 210 75 195 45 165 30 150 30 135 15 150 15
Line -1 true 150 30 150 270

bow-arrow
true
0
Polygon -6459832 true false 150 30
Rectangle -6459832 true false 135 30 165 255
Polygon -6459832 true false 150 0
Polygon -7500403 true true 150 0 105 60 150 45 195 60 150 0
Polygon -1 true false 150 225 120 240 105 285 135 255 150 225
Polygon -1 true false 150 225 180 240 195 285 165 255 150 225
Polygon -1 true false 150 225 150 270 120 300 135 255 150 225
Polygon -1 true false 150 225 150 270 165 300 165 255 150 225
Polygon -1 true false 150 210
Polygon -1 true false 150 210 180 225 195 255 165 240 150 225 150 210
Polygon -1 true false 150 210 120 225 105 255 135 240 150 225 150 210

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

door-left
true
0
Polygon -6459832 true false 105 45 90 45 75 60 75 270 225 270 225 60 210 45 105 45
Polygon -16777216 true false 210 270 210 75 195 60 120 60 105 75 105 270 210 270

door-mid
true
0
Polygon -6459832 true false 195 45 210 45 225 60 225 270 75 270 75 60 90 45 195 45
Polygon -16777216 true false 90 270 90 75 105 60 195 60 210 75 210 270 90 270

door-right
true
0
Polygon -6459832 true false 195 45 210 45 225 60 225 270 75 270 75 60 90 45 195 45
Polygon -16777216 true false 90 270 90 75 105 60 180 60 195 75 195 270 90 270

er
true
0
Polygon -7500403 true true 15 90
Polygon -2674135 true false 165 75 165 225 180 225 180 75 165 75
Polygon -2674135 true false 180 75 210 75 225 90 225 135 210 150 180 150 180 135 195 135 210 120 210 105 195 90 180 90 180 75
Polygon -2674135 true false 180 150 210 150 225 165 225 225 210 225 210 180 195 165 180 165 180 150 210 150
Polygon -16777216 true false 120 30 90 45
Polygon -16777216 true false 270 180
Rectangle -2674135 true false 90 150 90 165
Polygon -2674135 true false 120 75 75 75 75 225 135 225 135 210 90 210 90 150 105 150 120 150 120 135 90 135 90 90 135 90 135 75 120 75
Polygon -2674135 true false 165 75 210 75 225 90 225 135 210 150 225 165 225 225 210 225 210 180 195 165 180 165 180 225 165 225 165 75 180 90 180 135 195 135 210 120 210 105 195 90 180 90

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

ga
true
0
Polygon -7500403 true true 45 180 60 180
Polygon -2674135 true false 210 75 150 225 165 225 195 165 225 165 240 225 255 225 225 75 210 75 150 225 165 225 195 165 225 165 240 225
Polygon -2674135 true false 210 75 165 225 180 225 225 75 210 75
Polygon -6459832 true false 45 150
Polygon -2674135 true false 195 150 240 150 240 165 195 165 195 150
Polygon -2674135 true false 120 75 135 90 135 105 120 105 105 90 60 90 45 105 45 180 45 195 60 210 105 210 120 195 120 180 105 165 75 165 75 150 105 150 120 150 135 165 135 195 135 210 120 225 60 225 45 225 30 210 30 90 45 75 120 75

healthbar
true
15
Polygon -1 true true 30 105 15 120
Polygon -1 true true 150 120 270 120 285 135 285 165 270 180 30 180 15 165 15 135 30 120 150 120

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

key
true
0
Circle -1184463 true false 105 15 90
Circle -7500403 true true 120 30 60
Rectangle -1184463 true false 135 90 150 255
Rectangle -1184463 true false 150 240 195 255
Rectangle -1184463 true false 150 225 165 225
Rectangle -1184463 true false 150 195 180 195
Rectangle -1184463 true false 135 225 180 240
Rectangle -1184463 true false 150 210 210 225
Rectangle -1184463 true false 150 195 195 210
Rectangle -1184463 true false 150 90 165 195

level
true
0
Polygon -7500403 true true 15 90
Polygon -6459832 true false 75 75 75 225 120 225 120 210 90 210 90 165 105 165 105 150 90 150 90 90 120 90 120 75 75 75
Polygon -6459832 true false 120 75 150 225 165 225 195 75 180 75 165 180 150 180 135 75 120 75
Polygon -16777216 true false 120 30 90 45
Polygon -16777216 true false 270 180
Polygon -6459832 true false 195 75 195 225 240 225 240 210 210 210 210 165 225 165 225 150 210 150 210 90 240 90 240 75 195 75
Polygon -6459832 true false 45 75 30 75 30 225 60 225 60 210 45 210 45 75
Polygon -6459832 true false 270 75 255 75 255 225 285 225 285 210 270 210 270 75

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

me
true
0
Polygon -7500403 true true 45 180 60 180
Polygon -6459832 true false 45 150
Polygon -2674135 true false 75 225 60 225 60 75 75 75 105 135 135 75 150 75 150 225 135 225 135 105 105 165 75 105 75 225
Polygon -2674135 true false 90 75
Polygon -2674135 true false 180 75 180 225 240 225 240 210 195 210 195 150 225 150 225 135 195 135 195 90 240 90 240 75 180 75

minotaur-down
true
14
Polygon -6459832 true false 165 90 180 105 180 195 165 210 135 210 120 195 120 105 135 90 165 90
Polygon -6459832 true false 165 105 180 90 180 60 165 45 135 45 120 60 120 90 135 105 165 105
Polygon -7500403 true false 120 15 135 15
Polygon -6459832 true false 120 195
Polygon -6459832 true false 180 105 195 105 210 120 210 195 195 180 180 135 180 105
Polygon -6459832 true false 225 180 240 180
Polygon -6459832 true false 210 180 225 195 210 210 195 195 210 180
Polygon -6459832 true false 120 105 105 105 90 120 90 195 105 180 120 135 120 105
Polygon -6459832 true false 90 180 105 195 90 210 75 195 90 180
Polygon -6459832 true false 135 240
Polygon -1184463 true false 90 180 105 180 90 195 90 180
Polygon -1184463 true false 195 180 210 180 210 195 195 180
Polygon -6459832 true false 210 180 195 195 210 195 210 180
Polygon -6459832 true false 90 180 105 195 75 195 90 180
Polygon -7500403 true false 165 45 165 30 180 15 180 60 165 45
Polygon -7500403 true false 120 60 120 15 135 30 135 45 120 60
Polygon -7500403 true false 180 15 195 30 180 30
Polygon -7500403 true false 120 15 105 30 120 30
Rectangle -6459832 true false 120 195 135 255
Rectangle -6459832 true false 165 195 180 255
Polygon -2674135 true false 165 75 180 75 180 60 165 75
Polygon -2674135 true false 135 75 120 75 120 60 135 75
Polygon -1184463 true false 150 90

minotaur-left
true
14
Polygon -6459832 true false 180 90 195 105 195 195 180 210 135 210 120 195 120 105 135 90 195 90
Polygon -6459832 true false 165 105 180 90 180 60 165 45 135 45 120 60 120 90 135 105 165 105
Polygon -2674135 true false 135 75 135 60 150 60 135 75
Polygon -7500403 true false 165 75 180 60 180 45 180 30 165 15 150 30 165 30 165 60 165 75
Polygon -7500403 true false 135 30 135 15 120 30 135 30
Polygon -7500403 true false 120 15 135 15
Polygon -7500403 true false 135 15 150 30 150 45 135 45
Polygon -6459832 true false 120 195
Polygon -6459832 true false 180 195 165 195 180 195 165 225 165 255 180 255 180 225 180 210
Polygon -6459832 true false 165 225 165 210 180 210
Polygon -6459832 true false 195 105 210 105 225 120 225 195 210 180 195 135 195 105
Polygon -6459832 true false 225 180 240 180
Polygon -6459832 true false 225 180 240 195 225 210 210 195 225 180
Polygon -6459832 true false 120 105 105 105 90 120 90 195 105 180 120 135 120 105
Polygon -6459832 true false 90 180 105 195 90 210 75 195 90 180
Polygon -6459832 true false 150 195 135 195 150 195 135 225 135 255 150 255 150 225 150 210
Polygon -6459832 true false 135 225 135 195 150 210 150 225 135 225
Polygon -6459832 true false 135 240
Polygon -1184463 true false 90 180 105 180 90 195 90 180
Polygon -1184463 true false 210 180 225 180 225 195 210 180
Polygon -6459832 true false 225 180 210 195 225 195 225 180
Polygon -6459832 true false 90 180 105 195 75 195 90 180

minotaur-right
true
14
Polygon -6459832 true false 120 90 105 105 105 195 120 210 165 210 180 195 180 105 165 90 105 90
Polygon -6459832 true false 135 105 120 90 120 60 135 45 165 45 180 60 180 90 165 105 135 105
Polygon -2674135 true false 165 75 165 60 150 60 165 75
Polygon -7500403 true false 135 75 120 60 120 45 120 30 135 15 150 30 135 30 135 60 135 75
Polygon -7500403 true false 165 30 165 15 180 30 165 30
Polygon -7500403 true false 180 15 165 15
Polygon -7500403 true false 165 15 150 30 150 45 165 45
Polygon -6459832 true false 180 195
Polygon -6459832 true false 120 195 135 195 120 195 135 225 135 255 120 255 120 225 120 210
Polygon -6459832 true false 135 225 135 210 120 210
Polygon -6459832 true false 105 105 90 105 75 120 75 195 90 180 105 135 105 105
Polygon -6459832 true false 75 180 60 180
Polygon -6459832 true false 75 180 60 195 75 210 90 195 75 180
Polygon -6459832 true false 180 105 195 105 210 120 210 195 195 180 180 135 180 105
Polygon -6459832 true false 210 180 195 195 210 210 225 195 210 180
Polygon -6459832 true false 150 195 165 195 150 195 165 225 165 255 150 255 150 225 150 210
Polygon -6459832 true false 165 225 165 195 150 210 150 225 165 225
Polygon -6459832 true false 165 240
Polygon -1184463 true false 210 180 195 180 210 195 210 180
Polygon -1184463 true false 90 180 75 180 75 195 90 180
Polygon -6459832 true false 75 180 90 195 75 195 75 180
Polygon -6459832 true false 210 180 195 195 225 195 210 180

minotaur-up
true
14
Polygon -6459832 true false 165 90 180 105 180 195 165 210 135 210 120 195 120 105 135 90 165 90
Polygon -6459832 true false 165 105 180 90 180 60 165 45 135 45 120 60 120 90 135 105 165 105
Polygon -7500403 true false 120 15 135 15
Polygon -6459832 true false 120 195
Polygon -6459832 true false 180 105 195 105 210 120 210 195 195 180 180 135 180 105
Polygon -6459832 true false 225 180 240 180
Polygon -6459832 true false 210 180 225 195 210 210 195 195 210 180
Polygon -6459832 true false 120 105 105 105 90 120 90 195 105 180 120 135 120 105
Polygon -6459832 true false 90 180 105 195 90 210 75 195 90 180
Polygon -6459832 true false 135 240
Polygon -1184463 true false 90 180 105 180 90 195 90 180
Polygon -1184463 true false 195 180 210 180 210 195 195 180
Polygon -6459832 true false 210 180 195 195 210 195 210 180
Polygon -6459832 true false 90 180 105 195 75 195 90 180
Polygon -7500403 true false 165 45 165 30 180 15 180 60 165 45
Polygon -7500403 true false 120 60 120 15 135 30 135 45 120 60
Polygon -7500403 true false 180 15 195 30 180 30
Polygon -7500403 true false 120 15 105 30 120 30
Rectangle -6459832 true false 120 195 135 255
Rectangle -6459832 true false 165 195 180 255

next
true
0
Rectangle -6459832 true false 15 75 30 210
Polygon -6459832 true false 30 75 60 210 45 210 15 75 30 75
Rectangle -6459832 true false 45 75 60 210
Rectangle -6459832 true false 75 75 90 210
Rectangle -6459832 true false 75 75 120 90
Rectangle -6459832 true false 75 135 105 150
Rectangle -6459832 true false 75 195 120 210
Polygon -6459832 true false 195 75 180 75 135 210 150 210 195 75
Polygon -6459832 true false 135 75 150 75 195 210 180 210 135 75
Rectangle -6459832 true false 210 75 285 90
Rectangle -6459832 true false 255 150 255 210
Rectangle -6459832 true false 240 90 255 210

ov
true
0
Polygon -7500403 true true 15 90
Polygon -16777216 true false 120 30 90 45
Polygon -16777216 true false 270 180
Circle -2674135 true false 120 75 0
Polygon -2674135 true false 60 75 120 75 150 105 150 195 120 225 60 225 30 195 30 105 60 75 60 90 45 105 45 195 60 210 120 210 135 195 135 105 120 90 60 90
Polygon -2674135 true false 240 75 255 75 225 225 195 225 165 75 180 75 210 195 240 75

player-down
true
0
Circle -7500403 true true 105 75 90
Polygon -7500403 true true 135 150 120 165 120 195 135 195 135 180 135 165 135 210 165 210 165 165 165 195 180 195 180 165 165 150 135 150
Polygon -7500403 true true 165 195 195 210
Rectangle -16777216 true false 165 105 180 120
Rectangle -16777216 true false 120 105 135 120

player-left
true
0
Circle -7500403 true true 105 75 90
Polygon -7500403 true true 165 150 180 165 180 195 165 195 165 180 165 165 165 210 135 210 135 165 135 195 120 195 120 165 135 150 165 150
Polygon -7500403 true true 135 195 105 210
Rectangle -7500403 true true 135 105 150 120
Rectangle -16777216 true false 120 105 135 120

player-right
true
0
Circle -7500403 true true 105 75 90
Polygon -7500403 true true 135 150 120 165 120 195 135 195 135 180 135 165 135 210 165 210 165 165 165 195 180 195 180 165 165 150 135 150
Polygon -7500403 true true 165 195 195 210
Rectangle -7500403 true true 150 105 165 120
Rectangle -16777216 true false 165 105 180 120

player-up
true
0
Circle -7500403 true true 105 75 90
Polygon -7500403 true true 135 150 120 165 120 195 135 195 135 180 135 165 135 210 165 210 165 165 165 195 180 195 180 165 165 150 135 150
Polygon -7500403 true true 165 195 195 210

prev
true
0
Polygon -7500403 true true 15 90
Rectangle -6459832 true false 15 75 30 225
Polygon -6459832 true false 30 75 60 75 75 90 75 135 60 150 30 150 30 135 45 135 60 120 60 105 45 90 30 90
Polygon -6459832 true false 90 75 90 225 105 225 105 75 90 75
Polygon -6459832 true false 105 75 135 75 150 90 150 135 135 150 105 150 105 135 120 135 135 120 135 105 120 90 105 90 105 75
Polygon -6459832 true false 105 150 135 150 150 165 150 225 135 225 135 180 120 165 105 165 105 150 135 150
Polygon -6459832 true false 165 75 165 225 210 225 210 210 180 210 180 165 195 165 195 150 180 150 180 90 210 90 210 75 165 75
Polygon -6459832 true false 225 75 255 225 270 225 300 75 285 75 270 180 255 180 240 75 225 75
Polygon -16777216 true false 120 30 90 45
Polygon -16777216 true false 270 180

replay p1
true
0
Polygon -7500403 true true 15 90
Rectangle -6459832 true false 15 75 30 225
Polygon -6459832 true false 30 75 60 75 75 90 75 135 60 150 30 150 30 135 45 135 60 120 60 105 45 90 30 90
Polygon -6459832 true false 150 75 150 225 165 225 165 75 150 75
Polygon -6459832 true false 165 75 195 75 210 90 210 135 195 150 165 150 165 135 180 135 195 120 195 105 180 90 165 90 165 75
Polygon -6459832 true false 30 150 60 150 75 165 75 225 60 225 60 180 45 165 30 165 30 150 60 150
Polygon -6459832 true false 90 75 90 225 135 225 135 210 105 210 105 165 120 165 120 150 105 150 105 90 135 90 135 75 90 75
Polygon -16777216 true false 120 30 90 45
Polygon -16777216 true false 270 180
Rectangle -6459832 true false 225 75 240 225
Rectangle -6459832 true false 240 210 270 225

replay p2
true
0
Polygon -7500403 true true 45 180 60 180
Polygon -6459832 true false 75 75 15 225 30 225 60 165 90 165 105 225 120 225 90 75 75 75 15 225 30 225 60 165 90 165 105 225
Polygon -6459832 true false 75 75 30 225 45 225 90 75 75 75
Polygon -6459832 true false 45 150
Polygon -6459832 true false 60 150 105 150 105 165 60 165 60 150
Polygon -6459832 true false 150 150 105 75 120 75 150 135 165 135 195 75 210 75 165 150 150 150
Polygon -6459832 true false 120 75 150 120 165 120 195 75 210 75 165 150 150 150 105 75
Rectangle -6459832 true false 150 150 165 225

skull
true
0
Polygon -1 true false 30 105 45 60
Polygon -1 true false 15 150 15 120
Polygon -1 true false 46 148 43 122
Polygon -1 true false 69 152 70 104
Polygon -1 true false 59 149
Polygon -1 true false 45 150 45 180
Polygon -1 true false 240 135 240 105 225 75 210 60 180 45 150 45 150 135 240 135
Polygon -1 true false 240 150 240 165 225 180 195 195 180 210 180 210 150 210 150 150 240 150
Polygon -1 true false 60 135 60 105 75 75 90 60 120 45 150 45 150 135 60 135
Rectangle -1 true false 60 135 240 150
Polygon -1 true false 60 150 60 165 75 180 105 195 120 210 120 210 150 210 150 150 60 150
Polygon -1 true false 240 165 225 180 195 195 195 225 180 240 150 240 150 165 240 165
Polygon -1 true false 60 165 75 180 105 195 105 225 120 240 150 240 150 165 60 165
Polygon -1 true false 90 60 120 45 180 45 210 60 225 75 240 105 240 165 225 180 195 195 195 225 180 240 120 240 105 225 105 195 75 180 60 165 60 105 75 75 90 60
Polygon -16777216 true false 135 240 135 225
Polygon -16777216 true false 135 240 135 225 135 240
Line -16777216 false 135 240 135 225
Line -16777216 false 165 240 165 225
Line -16777216 false 120 240 120 225
Line -16777216 false 150 240 150 225
Line -16777216 false 180 240 180 225
Polygon -16777216 true false 165 105 195 105 210 120 210 135 195 150 180 150 165 135 165 105
Polygon -16777216 true false 135 105 105 105 90 120 90 135 105 150 120 150 135 135 135 105
Polygon -16777216 true false 150 165 165 195 135 195 150 165

slash
true
15
Polygon -1 true true 15 150 15 135 30 105 75 90 150 90 225 90 270 105 285 135 285 150 285 165 270 150 270 135 255 120 225 105 75 105 45 120 30 135 30 150 15 165 15 150

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

sword
true
0
Polygon -7500403 true true 150 15
Polygon -1 true false 150 0 135 15 135 210 165 210 165 15 150 0
Polygon -1184463 true false 135 210 180 210 195 195 195 225 105 225 105 195 120 210 135 210 165 210
Polygon -6459832 true false 135 225 135 255 165 255 165 225 135 225
Polygon -1184463 true false 120 270 120 285 180 285 180 270 120 270
Rectangle -6459832 true false 135 255 165 270

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.2.2
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
