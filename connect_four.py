import logging
FORMAT = '%(asctime)-15s %(message)s'
logging.basicConfig(level=logging.INFO, format=FORMAT)
import math
from negamark import AbstractGameStateCache, NegamarkBoard, NegamarkMove
import numpy
from storm_mysql_game_state_cache import StormMySQLGameStateCache

class ConnectFourBoard(NegamarkBoard):

  zobrist_nightmare = [7281302469423965948L, 11535137346879502140L, 16183058612673694751L, 10026502909839539198L, 2685075969092475931L, 13861215701798137759L, 16385698875654065536L, 822402904205995935L, 10617863152316893861L, 9327665059135154791L, 9120278488375106431L, 3273899307632190887L, 9469685473969652685L, 14069551448994504522L, 5503906241272452551L, 34167834375172777L, 7707234374558629707L, 6921982504939763693L, 10121285955116518457L, 8614577338908492826L, 2432954357549595199L, 7337707938651594288L, 9686374984529235051L, 3089354579533464243L, 5288794958712795763L, 3741839444804713005L, 3251924229072910609L, 6468084355474251133L, 1494510373968177940L, 15375581573269581385L, 6036395739993305979L, 16342183033159371326L, 8891259814109958577L, 11292829739724204624L, 4652171659706614497L, 734954815928583474L, 11808353800905123186L, 2052372241906140641L, 17261096264227414711L, 6142294369383520649L, 4551635071923340435L, 14531285652768634165L, 9429718624479070466L, 3945163968715979872L, 11012145987451389101L, 18128858372206286635L, 8030539348243423809L, 2527841533572567689L, 7666894609277990818L, 15051228724891796198L, 2306731562933026332L, 10630630353488155458L, 15612491633652122958L, 1412899886805961461L, 10935741207607989953L, 1388098205762229958L, 11866411547156727031L, 10564428861999339967L, 6436031515915822608L, 4604373099848445622L, 15978590928576709420L, 4303477751528313923L, 7094134810380433600L, 4209626953647004124L, 3037619575803077483L, 1361325463188045972L, 9413618834317830314L, 10799671478555630594L, 17302043050429613702L, 17392497261167013188L, 8867727171082354371L, 27555881416417709L, 12257215606299596096L, 4505089339051152112L, 11379693309448576075L, 14282639998440780704L, 12082683875741469230L, 3784581016600920472L, 551266601615037502L, 15473035378804042355L, 1513877138935422923L, 6633926970702675840L, 13356486780785402881L, 15201438103161614664L]

  def __init__(self, cache, squares=None,
               win_scores=None, simple_scores=None, potential_win_matrix=None):
    super(ConnectFourBoard,self).__init__(cache)
    self.rows = 6
    self.columns = 7
    if squares is not None:
      self.squares = squares
    else:
      self.squares = numpy.zeros((self.rows, self.columns), int)
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
    self.zobrist = 0L
    self.max_cache_move = 39

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
    legal_moves = []
    for column in range(0, self.columns):
      if self.squares[self.rows - 1][column] == NegamarkBoard.OPEN:
        legal_moves.append(ConnectFourMove(column))
    return legal_moves

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
    if x <= self.rows - 4:
      value_from_here += self.valueFourDown(x, y)
    if y <= self.columns - 4:
      value_from_here += self.valueFourToTheRight(x, y)
    if x <= self.rows - 4 and y <= self.columns - 4:
      value_from_here += self.valueFourDownAndRight(x, y)
    if x <= self.rows - 4 and y >= 3:
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
    if self.squares[x][y] == ConnectFourBoard.O:
      return '  O'
    if self.squares[x][y] == ConnectFourBoard.X:
      return '  X'
    return '  -'

  def print_board(self):
    print '  0  1  2  3  4  5'
    for row in range(self.rows - 1, -1, -1):
      pretty_row = ''
      for column in range(0, self.columns):
        pretty_row += self.cell_as_string(row, column)
      print pretty_row

  def human_move_from_stdin(self):
    looping = True
    column = 0
    while looping:
      try:
        column = int(input("Column to hit: "))
        if column < 0 or column >= self.columns:
          print "How about you pick one in the 0-%d range?" % self.columns - 1
        elif self.squares[self.rows - 1][column] != NegamarkBoard.OPEN:
          print "Column %d row %d is full, assclown." % (column, self.rows - 1)
        else:
          looping = False
      except (NameError, SyntaxError):
        print "What the fuck was that?"
    return ConnectFourMove(column)

  def make_move(self, move):
    for row in range(0, self.rows):
      if self.squares[row][move.column] == NegamarkBoard.OPEN:
        self.squares[row][move.column] = self.active_player
        self.zobrist = self.zobrist ^ self.zobrist_key_for_move(row, move.column)
        self.update_scores(row, move.column)
        break
    self.moves_so_far += 1
    self.active_player = self.other_player(self.active_player)

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

  def zobrist_index_for_move(self, row, column):
    return (self.columns * row + column) * 2 + self.active_player - 1

  def zobrist_key_for_move(self, row, column):
    return ConnectFourBoard.zobrist_nightmare[self.zobrist_index_for_move(row, column)]

  def copy_board(self):
    copied_squares = self.squares.copy()
    copied_win_scores = self.win_scores.copy()
    copied_simple_scores = self.simple_scores[:]
    # We don't copy potential_win_matrix. It never changes so all objects can
    # share it.
    new_board = ConnectFourBoard(squares=copied_squares, cache=self.cache,
                                 win_scores = copied_win_scores,
                                 simple_scores = copied_simple_scores,
                                 potential_win_matrix=self.potential_win_matrix)
    new_board.zobrist = self.zobrist
    return new_board

  def unique_id(self):
#    if self.moves_so_far > self.max_cache_move:
#      return 0
#    logging.debug("Getting the unique_id for %s because %d is totally not greater than %d" % (str(self.history), self.moves_so_far, self.max_cache_move))
#    total = 0
#    exponent = self.max_cache_move - 1
#    for move_column in self.history:
#      total += move_column * (7 ** exponent)
#      exponent -= 1
#      if exponent < 0:
#        return total
#   return total
   if self.zobrist < 0 or self.zobrist > 2 ** 64:
     print "Oh no, this board has an invalid zobrist key."
     self.print_board()
     crash
   if self.zobrist == 971541192403564766:
     print "This one is 971541192403564766:"
     self.print_board()
#   logging.debug("The Zobrist hash of %s is %d." % (str.join(" ",[str(row) for row in self.squares]), self.zobrist)) 
   return self.zobrist


class ConnectFourMove(NegamarkMove):
  def __init__(self, column):
    super(ConnectFourMove,self).__init__()
    self.column = column

  def __str__(self):
    return str(self.column)

  def __eq__(self, other):
    if isinstance(other, self.__class__):
      return self.column == other.column
    else:
      return False

def main():

  board = ConnectFourBoard(StormMySQLGameStateCache(
      'mysql://connect_four@localhost/connect_four'))
#  board.make_move(ConnectFourMove(4))
#  for move in [3, 2, 2, 1, 2, 0, 3, 4]:
  for move in [3, 2, 2, 1, 2, 0]:
    board.make_move(ConnectFourMove(move))
  board.is_automated[NegamarkBoard.X] = True
  board.is_automated[NegamarkBoard.O] = False
  board.ai_deadline = 60*60*24*7
  board.minimum_search_move = 1

  board.play_game()
#  board.choose_ai_move()

if __name__ == '__main__':
#  import cProfile
#  cProfile.run('main()')
  main()
