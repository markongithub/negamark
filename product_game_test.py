#!/usr/bin/python

from negamark import AbstractGameStateCache, NegamarkBoard, Outcome
from product_game import ProductGameBoard
from product_game import ProductGameMove
import datetime
import unittest

class product_game_test(unittest.TestCase):

  def test_methods_implemented(self):
    test_board = ProductGameBoard(AbstractGameStateCache())
    self.assertTrue(test_board.verify_subclass())

  def test_unique_id_int(self):

    test_board = ProductGameBoard(AbstractGameStateCache())
    test_board.make_move(ProductGameMove(3, 6)) #that's i=2 j=2, flat 14
    self.assertEqual((NegamarkBoard.X * 3 ** (4 + 14)) + (9 * 2) + 5,
                     test_board.unique_id_int())

  def test_unique_id_int_harder(self):

    test_board = ProductGameBoard(AbstractGameStateCache())
    test_board.make_move(ProductGameMove(3, 6)) #that's i=2 j=2, flat 14
    test_board.make_move(ProductGameMove(6, 6)) #that's i=4 j=0, flat 24
    self.assertEqual((NegamarkBoard.X * 3 ** (4 + 14)) +
                     (NegamarkBoard.O * 3 ** (4 + 24)) +
                     (9 * (6 - 1)) + (6 - 1),
                     test_board.unique_id_int())

  def test_heuristic(self):

    test_board = ProductGameBoard(AbstractGameStateCache())
    test_board.make_move(ProductGameMove(3, 6))
    test_board.make_move(ProductGameMove(3, 3))
    test_board.make_move(ProductGameMove(3, 9))
    self.assertEqual(-5, test_board.valueFourDownAndLeft(0, 4))
    self.assertEqual(-5, test_board.valueFourDownAndLeft(1, 3))
    self.assertEqual(-10, test_board.heuristic())
    test_board.make_move(ProductGameMove(3, 4))
    self.assertEqual(0, test_board.heuristic())
    test_board.make_move(ProductGameMove(4, 9))
    self.assertEqual(-5, test_board.heuristic())
    test_board.make_move(ProductGameMove(2, 4))
    self.assertEqual(-5, test_board.heuristic())
    test_board.make_move(ProductGameMove(2, 5))
    self.assertEqual(-1000, test_board.valueFourDownAndLeft(1, 3))
    self.assertEqual(-1000, test_board.heuristic())

  def test_four_square_value(self):
    test_board = ProductGameBoard(AbstractGameStateCache())
    self.assertEqual(
        5, test_board.fourSquareValue([NegamarkBoard.OPEN, NegamarkBoard.X,
                                       NegamarkBoard.X, NegamarkBoard.OPEN]))
    self.assertEqual(
        -1000,
        test_board.fourSquareValue([NegamarkBoard.O, NegamarkBoard.O,
                                    NegamarkBoard.O, NegamarkBoard.O]))

  def test_all_legal_moves_start(self):
    board = ProductGameBoard(AbstractGameStateCache())
    self.assertEqual(sum(range(1,10)), len(board.all_legal_moves()))

  def test_all_legal_moves(self):
    test_board = ProductGameBoard(AbstractGameStateCache())
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
    test_board = ProductGameBoard(AbstractGameStateCache())
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
                                           max_depth=2, deadline=deadline)
    self.assertEqual(outcome_from_two.value, Outcome.LOSS)

  def test_another_heuristic(self):
    test_board = ProductGameBoard(AbstractGameStateCache())
    test_board.make_move(ProductGameMove(3, 6))
    test_board.make_move(ProductGameMove(3, 3))
    test_board.make_move(ProductGameMove(3, 9))
    test_board.make_move(ProductGameMove(3, 1))
    self.assertEqual(10, test_board.heuristic())

if __name__ == '__main__':
      unittest.main()
