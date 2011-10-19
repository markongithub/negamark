import logging
#logging.basicConfig(level=logging.DEBUG)

import memcache
import time
from negamark import AbstractGameStateCache, Outcome

class MembaseGameStateCache(AbstractGameStateCache):

  def __init__(self, database_uri):
    self.database_uri = database_uri
    self.membase = memcache.Client([self.database_uri], debug=True)

  def get_outcome(self, state):
    cached_state = self.membase.get(str(state))
    if cached_state:
      fields = cached_state.split(',')
      if len(fields) == 3:
        heuristic = int(fields[2])
      else:
        heuristic = None
      return Outcome(int(fields[0]), int(fields[1]), heuristic)
    else:
      return None

  def save_outcome(self, state, value, depth, heuristic=None):
    if heuristic is None:
      stored_value = '%d,%d' % (value, depth)
    else:
      stored_value = '%d,%d,%d' % (value, depth, heuristic)
    backoff = 2
    while not self.membase.set(str(state), stored_value):
      print "Setting %s to %s returned False." % (str(state), stored_value)
      time.sleep(backoff)
      del self.membase
      self.membase = memcache.Client([self.database_uri], debug=True)
      if backoff < 10:
        backoff += 2


  def delete_outcome(self, state):
    if not self.membase.delete(str(state)):
      crash

  def flush(self):
    pass
