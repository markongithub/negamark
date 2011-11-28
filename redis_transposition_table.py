import logging
#logging.basicConfig(level=logging.DEBUG)

import redis
from negamark import AbstractTranspositionTable, Outcome

class RedisTranspositionTable(AbstractTranspositionTable):

  def __init__(self, database_uri):
    self.redis = redis.Redis(database_uri)

  def get_outcome(self, state):
    transposition = self.redis.get(state)
    if transposition:
      fields = transposition.split(',')
      if len(fields) == 3:
        heuristic = int(fields[2])
      else:
        heuristic = None
      return Outcome(int(fields[0]), int(fields[1]), heuristic)
    else:
      return None

  def save_outcome(self, state, value, depth, heuristic=0):
    if heuristic is None:
      stored_value = '%d,%d' % (value, depth)
    else:
      stored_value = '%d,%d,%d' % (value, depth, heuristic)
    self.redis.set(state, stored_value)

  def delete_outcome(self, state):
    self.redis.delete(state)

  def flush(self):
    pass