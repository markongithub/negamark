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
    self.assertEqual(0L ^ 7281302469423965948L,
                     test_board.unique_id())

  def test_heuristic(self):
    test_board = ConnectFourBoard(AbstractGameStateCache())
    test_board.make_move(ConnectFourMove(3))
    test_board.make_move(ConnectFourMove(0))
    test_board.make_move(ConnectFourMove(3))
    test_board.make_move(ConnectFourMove(2))
    test_board.make_move(ConnectFourMove(3))
    self.assertEqual(-15, test_board.heuristic())


#  def test_endgame(self):
#    test_board = ConnectFourBoard(AbstractGameStateCache())
#    test_board.make_move(ConnectFourMove(1))
#    test_board.make_move(ConnectFourMove(2))
#    test_board.make_move(ConnectFourMove(1))
#    test_board.make_move(ConnectFourMove(1))
#    test_board.make_move(ConnectFourMove(3))
#    test_board.make_move(ConnectFourMove(1))
#    test_board.make_move(ConnectFourMove(1))
#    test_board.make_move(ConnectFourMove(4))
#
#    test_board.minimum_search_move = 0
#    self.assertEqual(5, test_board.choose_ai_move().column)


if __name__ == '__main__':
      unittest.main()
