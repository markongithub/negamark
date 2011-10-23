#!/usr/bin/python

from negamark import AbstractGameStateCache, NegamarkBoard, Outcome
from connect_four import ConnectFourBoard
from connect_four import ConnectFourMove
import datetime
import numpy
import unittest
#import logging
#logging.basicConfig(level=logging.DEBUG)

class connect_four_test(unittest.TestCase):

  def test_methods_implemented(self):
    test_board = ConnectFourBoard(AbstractGameStateCache())
    self.assertTrue(test_board.verify_subclass())

  def test_unique_id(self):

    test_board = ConnectFourBoard(AbstractGameStateCache())
    test_board.make_move(ConnectFourMove(0))
    self.assertEqual(0L ^ 11535137346879502140L, test_board.unique_id())

  def test_heuristic(self):
    test_board = ConnectFourBoard(AbstractGameStateCache())
    self.assertEqual(69, test_board.simple_scores[NegamarkBoard.X - 1])
    test_board.make_move(ConnectFourMove(3))
    self.assertEqual(76, test_board.simple_scores[NegamarkBoard.X - 1])
    self.assertEqual(62, test_board.simple_scores[NegamarkBoard.O - 1])
    self.assertEqual(-14, test_board.heuristic())
    test_board.make_move(ConnectFourMove(0))
    test_board.make_move(ConnectFourMove(3))
    test_board.make_move(ConnectFourMove(2))
    test_board.make_move(ConnectFourMove(3))
    test_board.make_move(ConnectFourMove(2))
    test_board.make_move(ConnectFourMove(3))
    self.assertTrue(test_board.heuristic() <= -1000)

  def test_num_potential_wins(self):
    test_board = ConnectFourBoard(AbstractGameStateCache())
    self.assertEqual(6 * 4 + 7 * 3 + 2 * 4 * 3, test_board.num_potential_wins())

  def test_initial_win_scores(self):
    test_board = ConnectFourBoard(AbstractGameStateCache())
    self.assertEqual(test_board.num_potential_wins(),
                     len(test_board.win_scores[0]))

  def test_potential_win_matrix(self):
    test_board = ConnectFourBoard(AbstractGameStateCache())
    self.assertEqual(3, len(test_board.potential_win_matrix[0][0])),
    self.assertEqual(13, len(test_board.potential_win_matrix[3][3])),
    self.assertEqual(3, len(test_board.potential_win_matrix[5][6])),
    


if __name__ == '__main__':
      unittest.main()
