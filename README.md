# CS-3110-Final-Project

Cornell CS 3110 (Final Project)

Group members: Philip Ayoub (pja66), Jacob Goldstein (jgb324), Brendan Sullivan (bzs5), David Schmaier (dms482)

Overview:

This project is a complete implementation of game "go" writen entirely in oCaml. The main functionailty that we impplmented on top of a function game is: a command line UX, a CPU (AI), and a server. First, the CPU. We were able to fully implement minimax search after doing a lot of research and critical thinking. We implemented a basic heuristic with multiple parameters that allow the heuristic to be tweaked for optimal performance. These constant parameters specify how much weight should be put on captured prisoners, territories, filling in own eyes, and connectivity of pieces. This enables a single user to play a game against the “computer.” We had to implement multiple functions and data types that would enable us to implement this specific AI algorithm. The AI also runs relatively fast depending on the depth of the tree which makes sure the user never has to wait for the computer to make its move. We also implemented a server. Theoretically the server can be connected from anywhere in the world with an internet connection. The server has two command implements:  “show”, which displays all the moves made, and “add (x,y)” which adds the move (x,y) to the list of moves made. This functionality allows fans to follow along while a game is taking place, just as if they were watching a go tournament. The UX was improved vastly to include numbers to guide the player where to place their moves and to generally look cleaner.  It displays pieces much better now. 


