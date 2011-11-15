#!/usr/bin/python

from negamark import AbstractTranspositionTable, NegamarkBoard, Outcome
from product_game import ProductGameBoard
from product_game import ProductGameMove
import datetime
import numpy
import unittest
#import logging
#logging.basicConfig(level=logging.DEBUG)

class product_game_test(unittest.TestCase):

  def test_methods_implemented(self):
    test_board = ProductGameBoard(AbstractTranspositionTable())
    self.assertTrue(test_board.verify_subclass())

  def test_unique_id_int(self):

    test_board = ProductGameBoard(AbstractTranspositionTable())
    test_board.make_move(ProductGameMove(3, 6)) #that's i=2 j=2, flat 14
    self.assertEqual((NegamarkBoard.X * 3 ** (4 + 14)) + (9 * 2) + 5,
                     test_board.unique_id_int())

  def test_unique_id_int_harder(self):

    test_board = ProductGameBoard(AbstractTranspositionTable())
    test_board.make_move(ProductGameMove(3, 6)) #that's i=2 j=2, flat 14
    test_board.make_move(ProductGameMove(6, 6)) #that's i=4 j=0, flat 24
    self.assertEqual((NegamarkBoard.X * 3 ** (4 + 14)) +
                     (NegamarkBoard.O * 3 ** (4 + 24)) +
                     (9 * (6 - 1)) + (6 - 1),
                     test_board.unique_id_int())
    self.assertEqual(test_board.unique_id_int(), test_board.unique_id_faster)

  def test_unique_id_faster(self):
    test_board = ProductGameBoard(AbstractTranspositionTable())
    test_board.make_move(ProductGameMove(6, 3))
    test_board.make_move(ProductGameMove(7, 3))
    test_board.make_move(ProductGameMove(7, 4))
    test_board.make_move(ProductGameMove(8, 4))
    test_board.make_move(ProductGameMove(8, 6))
    test_board.make_move(ProductGameMove(7, 6))
    test_board.make_move(ProductGameMove(5, 6))
    test_board.make_move(ProductGameMove(6, 6))
    self.assertEqual(test_board.unique_id_int(), test_board.unique_id_faster)

  def test_unique_id_copied(self):
    test_board = ProductGameBoard(AbstractTranspositionTable())
    test_board.make_move(ProductGameMove(1, 1))
    self.assertEqual(test_board.unique_id_int(), test_board.unique_id_faster)
    copied_board = test_board.copy_board()
    self.assertEqual(copied_board.unique_id_faster, test_board.unique_id_faster)
    self.assertEqual(copied_board.unique_id_int(), test_board.unique_id_int())
    self.assertEqual(copied_board.unique_id_int(),
                     copied_board.unique_id_faster)
    new_board = test_board.new_board_from_move(ProductGameMove(1, 2))
    self.assertEqual(new_board.unique_id_int(), new_board.unique_id_faster)

  def test_adding_two_large_integers(self):
    self.assertEqual(9911327689934189018,
                     1806217383896236484 + 8105110306037952534)

  def test_unique_id_negative(self):
    test_board = ProductGameBoard(AbstractTranspositionTable())
    test_board.bottomFactor = 9
    test_board.topFactor = 9
    self.assertEqual(80, test_board.unique_id_int())

  def test_unique_id_negative_signflip(self):
    test_board = ProductGameBoard(AbstractTranspositionTable())
    test_board.squares = numpy.array([[0, 0, 0, 0, 0, 0], [0, 1, 0, 0, 0, 0],
                                      [0, 2, 2, 1, 1, 0], [0, 0, 2, 0, 1, 0],
                                      [0, 2, 0, 2, 2, 0], [0, 0, 0, 1, 1, 2]])
    test_board.bottomFactor = 9
    test_board.topFactor = 9
    self.assertFalse(test_board.unique_id_int() < 0)

  def test_heuristic(self):

    test_board = ProductGameBoard(AbstractTranspositionTable())
    test_board.make_move(ProductGameMove(3, 6))
    test_board.make_move(ProductGameMove(3, 3))
    test_board.make_move(ProductGameMove(3, 9))
    test_board.make_move(ProductGameMove(3, 4))
    test_board.make_move(ProductGameMove(4, 9))
    test_board.make_move(ProductGameMove(2, 4))
    test_board.make_move(ProductGameMove(2, 5))
    self.assertEqual(-2000, test_board.heuristic())

  def test_four_square_value(self):
    test_board = ProductGameBoard(AbstractTranspositionTable())
    self.assertEqual(
        5, test_board.fourSquareValue([NegamarkBoard.OPEN, NegamarkBoard.X,
                                       NegamarkBoard.X, NegamarkBoard.OPEN]))
    self.assertEqual(
        -1000,
        test_board.fourSquareValue([NegamarkBoard.O, NegamarkBoard.O,
                                    NegamarkBoard.O, NegamarkBoard.O]))

  def test_all_legal_moves_start(self):
    board = ProductGameBoard(AbstractTranspositionTable())
    self.assertEqual(sum(range(1,10)), len(board.all_legal_moves()))

  def test_all_legal_moves(self):
    test_board = ProductGameBoard(AbstractTranspositionTable())
    test_board.make_move(ProductGameMove(1, 3))
    test_board.make_move(ProductGameMove(1, 4))
    test_board.make_move(ProductGameMove(1, 5))
    test_board.make_move(ProductGameMove(1, 6))
    test_board.make_move(ProductGameMove(1, 7))
    test_board.make_move(ProductGameMove(1, 9))
    test_board.make_move(ProductGameMove(2, 9))
    test_board.make_move(ProductGameMove(2, 8))
    test_board.make_move(ProductGameMove(2, 7))
    test_board.make_move(ProductGameMove(2, 6))
    test_board.make_move(ProductGameMove(2, 5))
    test_board.make_move(ProductGameMove(2, 1))
    # okay, all multiples of 1 and 2 are taken except 1 and 8
    self.assertEqual(3, len(test_board.all_legal_moves()))
#        [ProductGameMove(1, 1), ProductGameMove(1, 8),
#                      ProductGameMove(2, 4)], test_board.all_legal_moves())

  def test_sort_moves(self):
    good_move = ProductGameMove(1, 1)
    good_move.outcome = Outcome(value=Outcome.TIMEOUT, depth=4, heuristic=0)
    bad_move = ProductGameMove(1, 2)
    bad_move.outcome = Outcome(value=Outcome.TIMEOUT, depth=4, heuristic=-10)
    all_moves = [bad_move, good_move]
    self.assertEqual(good_move,
                     sorted(all_moves, reverse=True)[0])
#                     sorted(all_moves)[0])

  def test_endgame(self):
    test_board = ProductGameBoard(AbstractTranspositionTable())
    test_board.make_move(ProductGameMove(3, 6))
    test_board.make_move(ProductGameMove(3, 3))
    test_board.make_move(ProductGameMove(3, 9))
    test_board.make_move(ProductGameMove(3, 4))
    test_board.make_move(ProductGameMove(4, 4))
    test_board.make_move(ProductGameMove(4, 8))
    test_board.make_move(ProductGameMove(5, 8))
    test_board.make_move(ProductGameMove(5, 5))
    test_board.make_move(ProductGameMove(2, 5))
    deadline = datetime.datetime.now() + datetime.timedelta(seconds=7200)
    outcome_from_two = test_board.negamark(current_depth=1, path='',
                                           max_depth=3, deadline=deadline,
                                           alpha=Outcome(Outcome.LOSS, 0),
                                           beta=Outcome(Outcome.WIN, 0))
    self.assertEqual(outcome_from_two.value, Outcome.LOSS)

  def test_another_scenario(self):
    test_board = ProductGameBoard(AbstractTranspositionTable())

    test_board.make_move(ProductGameMove(6, 3)) #A
    test_board.make_move(ProductGameMove(7, 3))
    test_board.make_move(ProductGameMove(7, 4))
    test_board.make_move(ProductGameMove(8, 4))
    test_board.make_move(ProductGameMove(8, 6))
    test_board.make_move(ProductGameMove(7, 6))
    test_board.make_move(ProductGameMove(5, 6))
    test_board.make_move(ProductGameMove(6, 6))
    test_board.make_move(ProductGameMove(6, 9))
    test_board.make_move(ProductGameMove(9, 9))
    test_board.make_move(ProductGameMove(9, 8))
    test_board.make_move(ProductGameMove(1, 8))
    test_board.make_move(ProductGameMove(2, 8))
    test_board.make_move(ProductGameMove(2, 6))
    test_board.make_move(ProductGameMove(4, 6))
    test_board.make_move(ProductGameMove(4, 1))
    test_board.make_move(ProductGameMove(5, 1))
    test_board.make_move(ProductGameMove(5, 9))
    test_board.make_move(ProductGameMove(5, 8))
    test_board.make_move(ProductGameMove(5, 4))
    test_board.make_move(ProductGameMove(5, 2))
    test_board.make_move(ProductGameMove(1, 2))
    test_board.make_move(ProductGameMove(1, 3)) # amy's last move
    test_board.make_move(ProductGameMove(3, 3)) # my theoretical suicide
#This is move 24
#  1  O  X  O  X  6
#  7  O  9  X  O 14
# 15  X  X  O  O  X
# 25 27  X  X  O 35
#  O  X  O  O  X 49
#  X 56 63 64  X  O
#Top marker is on 1
#Bottom marker is on 3
    self.assertEqual(Outcome.LOSS, test_board.first_pass().value)

if __name__ == '__main__':
      unittest.main()
