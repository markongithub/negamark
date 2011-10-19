import logging
#logging.basicConfig(level=logging.DEBUG)

import redis
from negamark import AbstractGameStateCache, Outcome

class RedisGameStateCache(AbstractGameStateCache):

  def __init__(self, database_uri):
    self.redis = redis.Redis(database_uri)

  def get_outcome(self, state):
    cached_state = self.redis.hgetall(state)
    if cached_state:
      if 'h' in cached_state:
        heuristic = int(cached_state['h'])
      else:
        heuristic = None
      return Outcome(int(cached_state['v']), int(cached_state['d']), heuristic)
    else:
      return None

  def save_outcome(self, state, value, depth, heuristic=0):
    if heuristic is None:
      self.redis.hmset(state, {'v': value, 'd': depth})
    else:
      self.redis.hmset(state, {'v': value, 'd': depth, 'h': heuristic})
#    self.redis.hset(% state, 'd', depth)
#    self.redis.hset('state:%d' % state, 'heuristic', heuristic)

  def delete_outcome(self, state):
    self.redis.delete(state)

  def flush(self):
    pass
