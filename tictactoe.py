import logging
logging.basicConfig(level=logging.DEBUG)

from copy import deepcopy
from negamark import NegamarkBoard, NegamarkMove
from storm_game_state_cache import StormGameStateCache

class TicTacToeMove(NegamarkMove):

  def __init__(self, destination_square):
    super(TicTacToeMove,self).__init__()
    self.destination_square = destination_square

  def __str__(self):
    return str(self.destination_square)

class TicTacToeBoard(NegamarkBoard):

  WAYS_TO_WIN = ((0, 1, 2), (0, 3, 6), (0, 4, 8),
                 (1, 4, 7), (2, 4, 6), (2, 5, 8),
                 (3, 4, 5), (6, 7, 8))

  def __init__(self, cache):
    super(TicTacToeBoard,self).__init__(cache)
    self.squares = []
    for i in range(0,9):
      self.squares.append(NegamarkBoard.OPEN)

  def all_legal_moves(self):
    moves = []
    for i in range(0,9):
      if self.square_open(i):
        moves.append(TicTacToeMove(i))
    return moves

  def square_open(self, square):
    return self.squares[square] == NegamarkBoard.OPEN

  def winner(self):
    for victory in TicTacToeBoard.WAYS_TO_WIN:
      if (self.squares[victory[0]] != NegamarkBoard.OPEN
          and self.squares[victory[0]] == self.squares[victory[1]]
          and self.squares[victory[0]] == self.squares[victory[2]]):
        return self.squares[victory[0]]
    return NegamarkBoard.NO_WINNER

  def heuristic(self):
    # This is lazy and backwards.
    winner = self.winner()
    if winner == self.active_player:
      return 1000
    elif winner == self.other_player(self.active_player):
      return -1000
    else:
      return 0

  def cell_as_string(self, cell):
    if self.squares[cell] == NegamarkBoard.X:
      return " X"
    elif self.squares[cell] == NegamarkBoard.O:
      return " O"
    else:
      return str(cell).rjust(2)

  def print_board(self):
    for i in range(0,3):
      row = ''
      for j in range(0,3):
        row += self.cell_as_string(3 * i + j)
      print row

  def human_move_from_stdin(self):
    return TicTacToeMove(int(input("No input checking. You can crash me: ")))

  def make_move(self, move):
    self.squares[move.destination_square] = self.active_player
    self.active_player = self.other_player(self.active_player)
    self.moves_so_far += 1

  def copy_board(self):
    new_board = TicTacToeBoard(self.cache)
    new_board.squares = deepcopy(self.squares)
    return new_board

  def unique_id(self):
    unique_id = 0
    for i in range(0, 9):
      unique_id += self.squares[i] * 3 ** i
    return unique_id


def main():

  board = TicTacToeBoard(StormGameStateCache('sqlite:tic_tac_toe_storm.db'))
  board.is_automated[NegamarkBoard.X] = False
  board.is_automated[NegamarkBoard.O] = True
  board.play_game()

if __name__ == '__main__':
        main()
