import logging
from negamark import NegamarkBoard
from negamark import NegamarkMove
import numpy

class ProductGameBoard(NegamarkBoard):

  forward_lookup = [[ 1,  2,  3,  4,  5,  6],
                    [ 7,  8,  9, 10, 12, 14],
                    [15, 16, 18, 20, 21, 24],
                    [25, 27, 28, 30, 32, 35],
                    [36, 40, 42, 45, 48, 49],
                    [54, 56, 63, 64, 72, 81]]
  number_locations = {
       1: (0, 0),  2: (0, 1),  3: (0, 2),  4: (0, 3),  5: (0, 4),  6: (0, 5),
       7: (1, 0),  8: (1, 1),  9: (1, 2), 10: (1, 3), 12: (1, 4), 14: (1, 5),
      15: (2, 0), 16: (2, 1), 18: (2, 2), 20: (2, 3), 21: (2, 4), 24: (2, 5),
      25: (3, 0), 27: (3, 1), 28: (3, 2), 30: (3, 3), 32: (3, 4), 35: (3, 5),
      36: (4, 0), 40: (4, 1), 42: (4, 2), 45: (4, 3), 48: (4, 4), 49: (4, 5),
      54: (5, 0), 56: (5, 1), 63: (5, 2), 64: (5, 3), 72: (5, 4), 81: (5, 5)}

  def __init__(self, transposition_table, squares=None,
               win_scores=None, simple_scores=None, potential_win_matrix=None):
    super(ProductGameBoard,self).__init__(transposition_table)
    self.rows = 6
    self.columns = 6
    if squares is not None:
      self.squares = squares
    else:
      self.squares = numpy.zeros((6, 6), int)
    if win_scores is not None:
      self.win_scores = win_scores
    else:
      self.win_scores = self.initial_scores()
    if simple_scores is not None:
      self.simple_scores = simple_scores
    else:
      self.simple_scores = map(sum, self.win_scores)
    if potential_win_matrix is not None:
      self.potential_win_matrix = potential_win_matrix
    else:
      self.potential_win_matrix = self.generate_potential_win_matrix()
    self.topFactor = 0
    self.bottomFactor = 0
    self.unique_id_faster = 0L
    self.minimum_search_move = 7
    self.max_transposition_table_move = 32

  def initial_scores(self):
    return numpy.ones((2, self.num_potential_wins()), int)

  def generate_potential_win_matrix(self):
    potential_win_matrix = []
    # initialize empty list of lists of lists - slow as shit but only once
    for x in range(0, self.rows):
      row_list = []
      for y in range(0, self.columns):
        cell_list = []
        row_list.append(cell_list)
      potential_win_matrix.append(row_list)
    win_index = 0
    # So, for every cell, we are going to have a list of "win indices" for that
    # cell. Take row 1 column 3 on a 7X6 board. From there there are six
    # potential wins on the horizontal access. Some going up, some going
    # diagonal, etc.  This table will tell us that from row 1 column 3 we can
    # get to, say, wins 23, 98, and 122.
    # first the left-to-right wins
    for x in range(0, self.rows):
      for y in range(0, self.columns - 4 + 1):
        for i in range(0, 4):
           potential_win_matrix[x][y + i].append(win_index)
        win_index += 1
    # now the downward (upward?) wins
    for x in range(0, self.rows - 4 + 1):
      for y in range(0, self.columns):
        for i in range(0, 4):
           potential_win_matrix[x + i][y].append(win_index)
        win_index += 1
    # now the downward (upward?) and right wins
    for x in range(0, self.rows - 4 + 1):
      for y in range(0, self.columns - 4 + 1):
        for i in range(0, 4):
           potential_win_matrix[x + i][y + i].append(win_index)
        win_index += 1
    # now the downward (upward?) and left wins
    for x in range(0, self.rows - 4 + 1):
      for y in range(4 - 1, self.columns):
        for i in range(0, 4):
           potential_win_matrix[x + i][y - i].append(win_index)
        win_index += 1
    return potential_win_matrix

  def num_potential_wins(self):
    num_potential_wins = 0
    # horizontal wins
    num_potential_wins += self.rows * (self.columns - 4 + 1)
    # vertical wins
    num_potential_wins += self.columns * (self.rows - 4 + 1)
    # down-and-right wins plus down-and-left wins
    num_potential_wins += 2 * ((self.columns - 4 + 1) * (self.rows - 4 + 1))
    return num_potential_wins

  def all_legal_moves(self):
    legal_products = []
    if self.topFactor == 0:
      # first move of the game. You can go anywhere.
      for i in range(1, 10):
        for j in range(i, 10): # no pojnt in checking (3,4) and (4,3)
          legal_products.append(ProductGameMove(i, j))
    else:
      for i in range(1, 10):
        if self.square_open(i * self.bottomFactor):
          legal_products.append(ProductGameMove(i, self.bottomFactor))
        if self.bottomFactor != self.topFactor:
          if self.square_open(i * self.topFactor):
            legal_products.append(ProductGameMove(self.topFactor, i))
    return sorted(legal_products)

  def square_open(self, square_value):
    (x, y) = ProductGameBoard.number_locations[square_value]
    return self.squares[x][y] == ProductGameBoard.OPEN

  def fourSquareValue(self, four_squares):
    stupid_dict = [0, 0, 0]
    stupid_dict[four_squares[0]] += 1
    stupid_dict[four_squares[1]] += 1
    stupid_dict[four_squares[2]] += 1
    stupid_dict[four_squares[3]] += 1
    other_player = self.other_player(self.active_player)
    if stupid_dict[other_player] == 4:
      return -1000
    elif (stupid_dict[self.active_player] == 3 and
          stupid_dict[NegamarkBoard.OPEN] == 1):
      return 10
    elif (stupid_dict[self.active_player] == 2 and
          stupid_dict[NegamarkBoard.OPEN] == 2):
      return 5
    elif (stupid_dict[other_player] == 3 and
          stupid_dict[NegamarkBoard.OPEN]):
      return -10
    elif (stupid_dict[other_player] == 2 and
          stupid_dict[NegamarkBoard.OPEN] == 2):
      return -5
    else:
      return 0

  def winner(self):
    if self.heuristic() <= -1000:
      return self.other_player(self.active_player)
    else:
      return NegamarkBoard.NO_WINNER

  def heuristic(self):
    return self.simple_scores[
       self.active_player - 1] - self.simple_scores[self.other_player(
           self.active_player) - 1]

  def value_from_here(self, x, y):
    value_from_here = 0
    if x <= 2:
      value_from_here += self.valueFourDown(x, y)
    if y <= 2:
      value_from_here += self.valueFourToTheRight(x, y)
    if x <= 2 and y <= 2:
      value_from_here += self.valueFourDownAndRight(x, y)
    if x <= 2 and y >= 3:
      value_from_here += self.valueFourDownAndLeft(x, y)
    return value_from_here

  def valueFourDown(self, x, y):
    return self.fourSquareValue([self.squares[x][y], self.squares[x+1][y],
                                 self.squares[x+2][y], self.squares[x+3][y]])

  def valueFourToTheRight(self, x, y):
    return self.fourSquareValue([self.squares[x][y], self.squares[x][y+1],
                                 self.squares[x][y+2], self.squares[x][y+3]])

  def valueFourDownAndRight(self, x, y):
    return self.fourSquareValue([self.squares[x][y], self.squares[x+1][y+1],
                                 self.squares[x+2][y+2], self.squares[x+3][y+3]])

  def valueFourDownAndLeft(self, x, y):
    return self.fourSquareValue([self.squares[x][y], self.squares[x+1][y-1],
                                 self.squares[x+2][y-2], self.squares[x+3][y-3]])

  def fourInARow(self, x, y):
    if x <= 2 and self.fourDown(x, y):
      return True
    if y <= 2 and self.fourToTheRight(x, y):
      return True
    if x <= 2 and y <= 2 and self.fourDownAndRight(x, y):
      return True
    return x <= 2 and y >= 3 and self.fourDownAndLeft(x, y)

  def cell_as_string(self, x, y):
    if self.squares[x][y] == ProductGameBoard.O:
      return '  O'
    if self.squares[x][y] == ProductGameBoard.X:
      return '  X'
    return str(ProductGameBoard.forward_lookup[x][y]).rjust(3)

  def print_board(self):
    for row in range(0,6):
      pretty_row = ''
      for column in range(0,6):
        pretty_row += self.cell_as_string(row, column)
      print pretty_row
    print "Top marker is on " + str(self.topFactor)
    print "Bottom marker is on " + str(self.bottomFactor)

  def human_move_from_stdin(self):
    looping = True
    (top, bottom) = (0, 0)
    while looping:
      try:
        top = int(input("Top marker: "))
        bottom = int(input("Bottom marker: "))
        if (self.topFactor != 0 and top != self.topFactor
            and bottom != self.bottomFactor):
          print "You can only move one of the markers."
        elif not self.square_open(top * bottom):
          print "Square %d is taken, fool." % (top * bottom)
        else:
          looping = False
      except NameError, SyntaxError:
        print "Fuck."
    return ProductGameMove(top, bottom)

  def make_move(self, move):
    if move.top > move.bottom:
      move.top, move.bottom = move.bottom, move.top #less complexity
    if self.unique_id_faster != 0:
      self.unique_id_faster -= long(9 * (self.topFactor - 1) +
                                    (self.bottomFactor - 1))
    self.topFactor = move.top
    self.bottomFactor = move.bottom
    (x, y) = ProductGameBoard.number_locations[move.bottom * move.top]
    self.squares[x][y] = self.active_player
    self.update_scores(x, y)
    self.unique_id_faster += long(9 * (self.topFactor - 1) +
                                  (self.bottomFactor - 1))
    self.unique_id_faster += long(self.active_player * 3 ** (6 * x + y + 4))
    self.active_player = self.other_player(self.active_player)
    self.moves_so_far += 1

  def unique_id_int(self):
    unique_id = 0L
    unique_id += long(9 * (self.topFactor - 1) + (self.bottomFactor - 1))
    index = 0
    for square_value in self.squares.flatten():
      to_add = long(square_value * 3 ** (4 + index))
      unique_id += to_add
      index += 1
    if unique_id < 0 or unique_id > (2 ** 64):
#      self.print_board()
#      crash
      return 0
    return int(unique_id)

  def update_scores(self, row, column):
    our_index = self.active_player - 1
    opponent_index = self.other_player(self.active_player) - 1
    for potential_win in self.potential_win_matrix[row][column]:
      if self.win_scores[our_index][potential_win] == 8:
        # We win here. We can stop keeping track of anything else.
        self.simple_scores[our_index] = 1000
        self.simple_scores[opponent_index] = -1000
        break
      else:
        our_difference = self.win_scores[our_index][potential_win]
        opponent_difference = self.win_scores[opponent_index][potential_win]
        self.win_scores[our_index][potential_win] <<= 1
        self.win_scores[opponent_index][potential_win] = 0
        self.simple_scores[our_index] += our_difference
        self.simple_scores[opponent_index] -= opponent_difference

  def copy_board(self):
    copied_squares = self.squares.copy()
    copied_win_scores = self.win_scores.copy()
    copied_simple_scores = self.simple_scores[:]
    # We don't copy potential_win_matrix. It never changes so all objects can
    # share it.
    new_board = ProductGameBoard(squares=copied_squares, transposition_table=self.transposition_table,
                                 win_scores = copied_win_scores,
                                 simple_scores = copied_simple_scores,
                                 potential_win_matrix=self.potential_win_matrix)
    new_board.topFactor = self.topFactor
    new_board.bottomFactor = self.bottomFactor
    new_board.unique_id_faster = self.unique_id_faster
    new_board.moves_so_far = self.moves_so_far
    new_board.max_transposition_table_move = self.max_transposition_table_move
    return new_board

  def squares_as_string(self):
    squares_as_string = ''
    for i in range(0, 6):
      for j in range(0, 6):
        squares_as_string += str(self.squares[i][j])
    return squares_as_string

  def unique_id(self):
    return self.unique_id_faster

  def unique_id_using_only_str(self):
    return str(self.squares) + str([self.topFactor, self.bottomFactor])

  def unique_id_using_squares_as_string(self):
    return str(self.topFactor) + " " + str(self.bottomFactor) + " " + self.squares_as_string()

class ProductGameMove(NegamarkMove):
  def __init__(self, top, bottom):
    super(ProductGameMove,self).__init__()
    self.top = top
    self.bottom = bottom

  def __str__(self):
    return "(%d,%d)" % (self.top, self.bottom)

  def __eq__(self, other):
    if isinstance(other, self.__class__):
      return (self.top == other.top
              and self.bottom == other.bottom)
    else:
      return False

def main():

  board = ProductGameBoard(StormMySQLTranspositionTable(
      'mysql://productgame@localhost/productgame'))
  board.is_automated[NegamarkBoard.X] = True
  board.is_automated[NegamarkBoard.O] = False
  board.ai_deadline = 60

  board.play_game()

if __name__ == '__main__':
  main()
