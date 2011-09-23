#!/usr/bin/python

from negamark import NegamarkBoard, Outcome
from storm_game_state_cache import StormGameStateCache
import unittest

class storm_game_state_cache_test(unittest.TestCase):

  def test_basic(self):
    test_cache = StormGameStateCache('sqlite:/tmp/test.db')
    test_cache.save_outcome(999, Outcome.WIN, 4)
    self.assertEqual(Outcome(Outcome.WIN, 4), test_cache.get_outcome(999))

  def test_delete(self):
    test_cache = StormGameStateCache('sqlite:/tmp/test2.db')
    test_cache.save_outcome(999, Outcome.WIN, 4)
    test_cache.delete_outcome(999)
    self.assertEqual(None, test_cache.get_outcome(999))

  def test_overwrite(self):
    test_cache = StormGameStateCache('sqlite:/tmp/test3.db')
    test_cache.save_outcome(9999, Outcome.LOSS, 3)
    test_cache.save_outcome(9999, Outcome.TIMEOUT, 9)
    ideal_outcome = Outcome(Outcome.TIMEOUT, 9)
    outcome_from_cache = test_cache.get_outcome(9999)
    self.assertEqual(ideal_outcome,
                     outcome_from_cache)

if __name__ == '__main__':
      unittest.main()
