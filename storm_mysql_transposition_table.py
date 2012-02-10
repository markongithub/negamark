import logging
#logging.basicConfig(level=logging.DEBUG)

from negamark import AbstractTranspositionTable, Outcome
from storm.locals import *

class StormMySQLTranspositionTable(AbstractTranspositionTable):

  def __init__(self, database_url):
    database = create_database(database_url)
    self.store = Store(database)
    self.store.execute('CREATE TABLE IF NOT EXISTS transposition (state BIGINT '
                       'UNSIGNED PRIMARY KEY, value INTEGER, depth INTEGER, '
                       'heuristic INTEGER)')

  def get_outcome(self, state):
    transposition = self.store.get(Transposition, state)
    if transposition:
      return Outcome(transposition.value, transposition.depth,
                     transposition.heuristic)
    else:
      return None

  def save_outcome(self, state, value, depth, heuristic=0):
    transposition = self.store.get(Transposition, state)
    if transposition:
      transposition.value=value
      transposition.depth=depth
      transposition.heuristic=heuristic
    else:
      transposition = Transposition(state, value, depth, heuristic)
      self.store.add(transposition)

  def delete_outcome(self, state):
    transposition = self.store.get(Transposition, state)
    if transposition:
      self.store.remove(transposition)

  def flush(self):
    self.store.commit()
    self.store.flush()

class Transposition(object):
  __storm_table__ = 'transposition'
  state = Int(primary=True)
  value = Int()
  depth = Int()
  heuristic = Int()

  def __init__(self, state, value, depth, heuristic=None):
    self.state = state
    self.value = value
    self.depth = depth
    self.heuristic = heuristic

def main():
  table = StormMySQLTranspositionTable('mysql://productgame@localhost/productgame')

if __name__ == '__main__':
  main()

