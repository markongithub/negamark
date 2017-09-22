# The Product Game is solved.

[The Product Game](http://illuminations.nctm.org/activity.aspx?id=4213) is a Tic-Tac-Toe-like board game designed for elementary school children with the aim of practicing multiplication tables. One adult never outgrew it and wrote a bunch of code to figure out strategy and, ultimately, a [weak solution](https://en.wikipedia.org/wiki/Solved_game) for the game.

## Solution

TL;DR X always wins if she plays optimally.

It's a big more complicated than that. In the version of the game we learned originally, the first player opens by choosing two factors and claiming the square labeled with their product on the board. For example, X could choose 6 and 8, and place an X on 48. This version of the game is [first-player win](https://en.wikipedia.org/wiki/First-player_and_second-player_win). By making an optimal first move, X can win no later than on her 10th move, the 19th move overall.

In the version of the game documented on the National Council of Teachers of Mathematics web site, the first player chooses one factor. The second player then chooses another factor, and claims the square associated with their product. So the second player to move is the first player to claim a square on the board. For our purposes, in keeping with the norms of Tic-Tac-Toe, we refer to this second player as "X". As an example, O could pick the factor 4, then X could pick the factor 5, and place an X on 20.

This slightly more complicated game is really second-player win, but in either case the winner is the first player to claim a square on the board. By choosing an opening factor, O can delay X's win until X's 13th move, the 25th move overall, but X will still ultimately win.

## Installation and Play

Included in this repository is a [transposition table](https://en.wikipedia.org/wiki/Transposition_table) of Product Game states and their [Negamax](https://en.wikipedia.org/wiki/Negamax) values in MySQL format. Using this database, a player can play perfectly up through the 8th move of the game. After that point, this code can solve states quickly enough that no transposition table is necessary.

1. Get a Haskell compiler and Cabal. This should get you partway there:
   ```
   $ apt install cabal-install
   $ cabal update
   ```

2. Clone this repo and its data.
   ```
   $ git clone https://github.com/markongithub/negamark.git
   ```

3. Build and test the code. It will probably complain about missing dependencies. ```cabal install``` those too.
   ```
   negamark$ cabal test && cabal build
   ```

4. Choose a MySQL database. It can be running locally or remotely, with or without a password. All that matters is that some user can access it with the ```mysql``` command. Most of this process is left as an exercise to the reader. Sorry. We hate when people say that too.

5. Populate the database with our prepackaged solution.
   ```
   negamark$ mysql -u <your mysql user> <your mysql database> < ./data/solution.mysql
   ```

6. Try to beat us in dog-eat-dog Product Game competition.
   ```
   negamark$ ./dist/build/PlayPerfectlyMySQL/PlayPerfectlyMySQL
   ```
