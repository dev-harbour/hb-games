# hb-games

<h1><p align="center">
Text-based game development
</p></h1>

<p align="justify">
This guide is an introduction to game programming using the Harbour programming language. It covers the basics of text-based game development, including techniques for displaying text on the screen, handling user input, and creating a game loop. We will also provide simple examples of text-based games that you can use as a starting point for your own projects. Whether you're a beginner or an experienced developer, this instruction will help you learn the essential concepts and skills needed to create your own text-based games using Harbour.
</p>

Creating a text-based game in Harbour is a process that typically involves the following steps:

<ul>
<li>
<p align="justify">
Designing the game: The first step in creating a text-based game is to design the game mechanics,
story, and characters. This includes creating a detailed game map, outlining the main quest and any subplots,
and deciding on the types of characters and enemies that will appear in the game.
</p>
</li>

<li>
<p align="justify">
Writing the game logic: Once the game design is complete, you can begin writing the code that will control the game logic.
This includes creating functions to handle player input, update the game state, and display text on the screen.
</p>
</li>

<li>
<p align="justify">
Building the game world: With the game logic in place, you can start building the game world by creating locations,
characters, items, and enemies. This is where you will use Harbour's database functionality to create and manipulate the game data.
</p>
</li>

<li>
<p align="justify">
Adding interaction: At this stage, you can add interactivity to the game by creating dialogues, puzzles, and other types of interactions
that will make the game more engaging for the player. This step also includes user input handling, conditional statements and error management.
</p>
</li>

<li>
<p align="justify">
Testing and debugging: Once the game is complete, it's important to test it thoroughly and fix any bugs or issues that are discovered.
This process involves running the game and playing through it, testing all aspects of the game mechanics, and fixing any errors or crashes that occur.
</p>
</li>

<li>
<p align="justify">
Release and maintenance: After all the steps are done, your game is ready to be distributed and shared. Ongoing maintenance will be necessary to fix
any issue that might arise during the use of the game.
</p>
</li>
</ul>

<p align="justify">
Creating a text-based game can be a fun and rewarding process, but it also requires a significant amount of planning and programming. With Harbour's
powerful set of libraries and functions, as well as its compatibility with existing xBase code, it can make the process of creating a text-based game
a bit easier.
</p>

<p align="justify">
Those are the general steps involved in creating a text-based game using the Harbour programming language, but there may be additional steps or
considerations depending on the specific requirements and complexity of the game you're trying to create. Some other things to keep in mind include:
</p>

<ul>
<li>
Improving the user interface, like adding a menu, color options, and other elements to improve the overall look and feel of the game.
</li>
<li>
Adding sound effects and music to enhance the gaming experience.
</li>
<li>
Adding a save feature, so players can save their progress and continue playing later on.
</li>
<li>
Optimizing the performance of the game, in case the game is too slow or too resource intensive.
</li>
<li>
Integrating different platforms, like a web or mobile versions of the game.
</li>
<li>
Creating a public version of the game and providing support to end-users
</li>
</ul>

<p align="justify">
Creating a text-based game is a challenging but very rewarding process. It requires a good deal of creativity and problem-solving skills.
The Harbour programming language offers a powerful set of libraries and functions that can help to make this process easier, but it's important
to keep in mind that game development takes time, effort and a lot of testing to get it right.
</p>

## Handling user input

<p align="justify">
Handling user input is an important aspect of creating a text-based game using the Harbour programming language. There are several techniques that you can use to handle user input, such as:
</p>

<ul>
<li>
<p align="justify">
Keyboard input: One of the most basic ways to handle user input is to use the keyboard. Harbour provides several built-in functions, such as GET and <b>Inkey</b>, that can be used to capture and process keyboard input.
</p>
</li>

<li>
<p align="justify">
Menus: Menus can be used to provide the player with a list of options to choose from. Menus can be implemented using loops and conditional statements, and can be navigated using the keyboard or other input methods.
</p>
</li>

<li>
<p align="justify">
Parsing input: Another technique is to allow the player to type in commands, and then use the Harbour's string manipulation functions to parse the input and determine the appropriate action to take.
</p>
</li>

<li>
<p align="justify">
Creating a dialog interface: You can also use a dialog box to handle user input, where the player can interact with the game by clicking on buttons, input fields and other widgets, this can be achieved using the built-in GUI libraries and functions of Harbour.
</p>
</li>

<li>
<p align="justify">
Input validation: It's important to validate input to ensure that it is in the correct format and within the expected range. This can be done using conditional statements and error management.
</p>
</li>

<li>
<p align="justify">
Shortcut commands: You can create shortcut commands that the user can input to perform specific actions, this can make the gameplay experience more convenient.
</p>
</li>
</ul>

<p align="justify">
All these techniques can be used in conjunction with each other and can be chosen depending on the game's requirements, for example a RPG game could use a menu system to handle the player's movement, a dialog box for NPC interactions and commands for specific actions.
</p>

<p align="justify">
Keep in mind that it's important to handle user input in a way that is intuitive and easy for the player to understand, and to provide clear feedback when the input is processed.
</p>

## Creating a game loop

<p align="justify">
A game loop is a fundamental concept in game programming that is used to control the flow of the game and update the game state. The game loop is typically implemented as a continuous loop that runs until the game is over. In a text-based game created with Harbour, the game loop typically involves the following steps:
</p>

<ul>
<li>
<p align="justify">
Input handling: The first step in the game loop is to handle user input. This includes capturing input from the keyboard, mouse, or other input devices, and determining the appropriate action to take based on the input.
</p>
</li>

<li>
<p align="justify">
Updating the game state: Once input is handled, the game state is updated. This includes updating the positions of characters and objects, checking for collisions, and determining if the game is over.
</p>
</li>

<li>
<p align="justify">
Displaying the game state: After the game state is updated, it is displayed on the screen. This typically involves displaying text and other visual elements, such as images, to the player.
</p>
</li>

<li>
<p align="justify">
Checking for game over: After the game state is displayed, the game loop checks if the game is over. This could be done by checking if the player reached a certain goal, if a game over condition has been met, or if the player decided to quit the game.
</p>
</li>

<li>
<p align="justify">
Loop: If the game is not over, the game loop repeats, starting with the input handling step, and continuing through the other steps of the loop.
</p>
</li>

<li>
<p align="justify">
End of the game: If the game is over, the game loop exits and the game is closed.
</p>
</li>
</ul>

<p align="justify">
It's important to note that the game loop does not have to be a infinite loop, a game loop can have a set number of iterations or until a specific condition is met. Also the loop could have some sleep time to reduce the CPU usage.
</p>

<p align="justify">
It's also important to keep in mind that the game loop should be optimized for performance, as it is executed repeatedly and any performance bottlenecks
</p>

## Simple examples of text-based games

> - [text-based_1.prg](Text-based-game-development/text-based_1.prg)

``` harbour
#include "inkey.ch"

PROCEDURE Main()

   LOCAL nKey, aMap, nPlayerX, nPlayerY

   aMap := { ;
      { "#", "#", "#", "#", "#", "#", "#", "#", "#", "#" }, ;
      { "#", ".", ".", ".", ".", ".", ".", ".", ".", "#" }, ;
      { "#", ".", "#", "#", ".", "#", "#", ".", ".", "#" }, ;
      { "#", ".", ".", ".", ".", ".", ".", ".", ".", "#" }, ;
      { "#", "#", "#", "#", "#", ".", "#", "#", ".", "#" }, ;
      { "#", ".", ".", ".", ".", ".", "#", ".", ".", "#" }, ;
      { "#", ".", "#", "#", ".", "#", "#", ".", ".", "#" }, ;
      { "#", ".", ".", ".", ".", ".", ".", ".", ".", "#" }, ;
      { "#", "#", "#", "#", "#", "#", "#", "#", "#", "#" } }

   nPlayerX := 2
   nPlayerY := 2

   WHILE( .T. )

      DrawMap( aMap, nPlayerX, nPlayerY )

      nKey := Inkey( 1 )

      IF nKey == K_UP
         nPlayerY := nPlayerY - 1
      ELSEIF nKey == K_DOWN
         nPlayerY := nPlayerY + 1
      ELSEIF nKey == K_LEFT
         nPlayerX := nPlayerX - 1
      ELSEIF nKey == K_RIGHT
         nPlayerX := nPlayerX + 1
      ENDIF
   ENDDO

   RETURN

FUNCTION DrawMap( aMap, nPlayerX, nPlayerY )

   LOCAL nX, nY

   FOR nY := 1 TO Len( aMap )
      FOR nX := 1 TO Len( aMap[ nY ] )
         IF nX == nPlayerX .AND. nY == nPlayerY
            hb_DispOutAt( nY - 1, nX - 1, "P", 0x22 )
         ELSE
            hb_DispOutAt( nY - 1, nX - 1, aMap[ nY ][ nX ], 0x7 )
         ENDIF
      NEXT
   NEXT

   RETURN NIL
```

![Windows](Text-based-game-development/docs/text-based_1.png )

<p align="justify">
This code is a simple text-based game written in the Harbour programming language. The game uses an array, <b>aMap</b>, to represent the map of the game world, and assigns characters to different positions on the map to represent different types of locations (e.g. walls represented by "#" and open spaces represented by "." ). The player's position is tracked using the variables <b>nPlayerX</b> and <b>nPlayerY</b>, which are initially set to 2,2.
</p>
<p align="justify">
The game loop is implemented in the while loop, that iterates continuously until the game is over. The game loop calls the function DrawMap, that takes the map, player's x and y position as inputs. Inside the function, the map is looped through, and the characters of the array are printed on the screen using hb_DispOutAt function, using x and y positions as the coordinates for printing. If the current position is the player's position, a special character is printed.
</p>

<p align="justify">
The game loop also captures input from the keyboard using the <b>Inkey</b> function, which waits for 1 second for an input. If an input is captured, the program checks if it is an arrow key and updates the player's position accordingly.
</p>

<p align="justify">
The game allows the player to move on the map using the arrow keys, and the player character is represented by a special character.
</p>

> - [text-based_2.prg](Text-based-game-development/text-based_2.prg)

``` harbour
#include "inkey.ch"

PROCEDURE Main()

   LOCAL nKey, aMap, nPlayerX, nPlayerY, nLives

   aMap := { ;
      { "#", "#", "#", "#", "#", "#", "#", "#", "#", "#" }, ;
      { "#", ".", ".", ".", ".", ".", ".", ".", ".", "#" }, ;
      { "#", ".", "#", "#", ".", "#", "#", ".", ".", "#" }, ;
      { "#", ".", ".", ".", ".", ".", ".", ".", ".", "#" }, ;
      { "#", "#", "#", "#", "#", ".", "#", "#", ".", "#" }, ;
      { "#", ".", ".", ".", ".", ".", "#", ".", ".", "#" }, ;
      { "#", ".", "#", "#", ".", "#", "#", ".", ".", "#" }, ;
      { "#", ".", ".", ".", ".", ".", ".", ".", ".", "#" }, ;
      { "#", "#", "#", "#", "#", "#", "#", "#", "#", "#" } }

   nPlayerX := 2
   nPlayerY := 2
   nLives := 3

   WHILE( nLives > 0 )

      DrawMap( aMap, nPlayerX, nPlayerY )

      hb_DispOutAt( 20, 1, "Lives: " + Str( nLives ) )

      nKey := Inkey( 1 )

      IF nKey == K_UP
         IF aMap[ nPlayerY - 1 ][ nPlayerX ] == "#"
            nLives -= 1
            hb_DispOutAt( 22, 1, "You hit a wall, you lost 1 life" )
         ELSE
            nPlayerY := nPlayerY - 1
         ENDIF
      ELSEIF nKey == K_DOWN
         IF aMap[ nPlayerY + 1 ][ nPlayerX ] == "#"
            nLives -= 1
            hb_DispOutAt( 22, 1, "You hit a wall, you lost 1 life" )
         ELSE
            nPlayerY := nPlayerY + 1
         ENDIF
      ELSEIF nKey == K_LEFT
         IF aMap[ nPlayerY ][ nPlayerX - 1 ] == "#"
            nLives -= 1
            hb_DispOutAt( 22, 1, "You hit a wall, you lost 1 life" )
         ELSE
            nPlayerX := nPlayerX - 1
         ENDIF
      ELSEIF nKey == K_RIGHT
         IF aMap[ nPlayerY ][ nPlayerX + 1 ] == "#"
            nLives -= 1
            hb_DispOutAt( 22, 1, "You hit a wall, you lost 1 life" )
         ELSE
            nPlayerX := nPlayerX + 1
         ENDIF
      ENDIF
   ENDDO

   IF nLives == 0
      Alert( "Game Over!" )
   ENDIF

   RETURN

FUNCTION DrawMap( aMap, nPlayerX, nPlayerY )

   LOCAL nX, nY

   FOR nY := 1 TO Len( aMap )
      FOR nX := 1 TO Len( aMap[ nY ] )
         IF nX == nPlayerX .AND. nY == nPlayerY
            hb_DispOutAt( nY - 1, nX - 1, "P", 0x22 )
         ELSE
            hb_DispOutAt( nY - 1, nX - 1, aMap[ nY ][ nX ], 0x7 )
         ENDIF
      NEXT
   NEXT

   RETURN NIL
```

![Windows](Text-based-game-development/docs/text-based_2.png )

<p align="justify">
With this code, the game will display the number of lives that the player has left and it will end the game when the player loses all lives. The player will lose 1 live every time he hits a wall and the game over message will be shown. It's important to note that the code is a simple example of how you could implement a text-based game using Harbour, it has limitations and could be improved to make it more interesting, complex and fun!
</p>

<p align="justify">
By enlarging the <b>aMap</b> array, you can create larger and more complex levels for the game, making it more interesting and challenging for the player.
</p>

<p align="justify">
You can add a check within the movement logic, when the player moves to a new position, check if the character is on a "." <b>If</b> it is, then you can remove it from the game board. One way to do this is to change the <b>"."</b> to a blank space <b>" "</b> or another character that represents an empty space. For example, you can add this check within the <b>ELSE</b> block of the movement logic:
</p>

> - [text-based_2.prg](Text-based-game-development/text-based_3.prg)

``` harbour
WHILE( nLives > 0 )

   DrawMap( aMap, nPlayerX, nPlayerY )

   hb_DispOutAt( 20, 1, "Lives: " + Str( nLives ) )

   nKey := Inkey( 1 )

   IF nKey == K_UP
      IF aMap[ nPlayerY - 1 ][ nPlayerX ] == "#"
         nLives -= 1
         hb_DispOutAt( 22, 1, "You hit a wall, you lost 1 life" )
      ELSE
         nPlayerY := nPlayerY - 1
      ENDIF
   ELSEIF nKey == K_DOWN
      IF aMap[ nPlayerY + 1 ][ nPlayerX ] == "#"
         nLives -= 1
         hb_DispOutAt( 22, 1, "You hit a wall, you lost 1 life" )
      ELSE
         nPlayerY := nPlayerY + 1
      ENDIF
   ELSEIF nKey == K_LEFT
      IF aMap[ nPlayerY ][ nPlayerX - 1 ] == "#"
         nLives -= 1
         hb_DispOutAt( 22, 1, "You hit a wall, you lost 1 life" )
      ELSE
         nPlayerX := nPlayerX - 1
      ENDIF
   ELSEIF nKey == K_RIGHT
      IF aMap[ nPlayerY ][ nPlayerX + 1 ] == "#"
         nLives -= 1
         hb_DispOutAt( 22, 1, "You hit a wall, you lost 1 life" )
      ELSE
         nPlayerX := nPlayerX + 1
      ENDIF
   ENDIF

   IF aMap[ nPlayerY ][ nPlayerX ] == "."
      aMap[ nPlayerY ][ nPlayerX ] := " "
   ENDIF

ENDDO
```

<p align="justify">
You would need to make a similar modification in the <b>DrawMap()</b> function so that it knows how to draw the blank space or other character you choose to represent an empty space.
</p>

<p align="justify">
To remove a <b>.</b> from the game board when the player character moves through it, you can add a check in the code that updates the player's position. For example, you can add the following code after updating the player's position:
</p>

``` harbour
IF aMap[ nPlayerY ][ nPlayerX ] == "."
   aMap[ nPlayerY ][ nPlayerX ] := " "
ENDIF
```

<p align="justify">
This checks if the player's current position is a <b>.</b>, and if it is, it changes it to a space character <b>" "</b>. This effectively removes the <b>.</b> from the game board.
</p>

<p align="justify">
You can add this check in each of the four <b>ELSE</b> blocks that update the player's position, depending on the key pressed.
</p>

<p align="justify">
<b>aMap</b> and <b>nPlayerX</b> and <b>nPlayerY</b> which represent the current position of the player on the game board. The function uses nested for loops to iterate through each element of the <b>aMap</b> array, and for each element it checks if the current coordinates match the player's coordinates. If they match, it outputs the player character <b>"P"</b> with a specific color code 0x22. If the current element is a <b>"."</b> it will output an empty space <b>" "</b> with color code 0x7. Else, it will output the element with color code 0x7. It then returns <b>NIL</b>. This function is used to update the game board with the player's current position and display any changes made to the board.
</p>

``` harbour
FUNCTION DrawMap( aMap, nPlayerX, nPlayerY )
   LOCAL nX, nY
   FOR nY := 1 TO Len( aMap )
      FOR nX := 1 TO Len( aMap[ nY ] )
         IF nX == nPlayerX .AND. nY == nPlayerY
            hb_DispOutAt( nY - 1, nX - 1, "P", 0x22 )
         ELSE
            IF aMap[ nY ][ nX ] == "." AND nX == nPlayerX AND nY == nPlayerY
               hb_DispOutAt( nY - 1, nX - 1, " ", 0x7 )
            ELSE
               hb_DispOutAt( nY - 1, nX - 1, aMap[ nY ][ nX ], 0x7 )
            ENDIF
         ENDIF
      NEXT
   NEXT
   RETURN NIL
```

![Windows](Text-based-game-development/docs/text-based_3.png )

<p align="justify">
It is important to note that this is just one possible way to implement this feature, and you may want to adjust it to suit the needs of your specific game.
<p>

---

<p>
<a href="https://www.paypal.me/rafaljopek?locale.x=pl_PL/">If you enjoyed this instruction, please support the project by making a donation through Paypal.<br> Your support will help to ensure that this instruction can continue to be developed and updated.</a>
</p>

