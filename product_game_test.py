#!/usr/bin/python

from negamark import AbstractGameStateCache
from negamark import NegamarkBoard
from product_game import ProductGameBoard
from product_game import ProductGameMove
import unittest

class product_game_test(unittest.TestCase):

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

if __name__ == '__main__':
      unittest.main()
