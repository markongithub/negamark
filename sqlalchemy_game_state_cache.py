from sqlalchemy import *
from sqlalchemy.orm import mapper, sessionmaker

class SQLAlchemyGameStateCache(GameStateCache):
  def __init__(self, database_url):
    engine = create_engine('sqlite:///negamark.db')
    print "Setting up database engine..."
    engine = create_engine(database_url)
    print "Setting up metadata..."
    metadata = MetaData(engine)
    self.cached_states = Table('cached_states', metadata,
                               Column('state', BigInteger, primary_key=True),
                               Column('value', Integer, nullable=False),
                               Column('heuristic', Integer),
                               Column('depth', Integer, nullable=False),)
    mapper(CachedState, self.cached_states)
    print "Creating tables if necessary..."
    metadata.create_all(engine)
    print "Setting up sessionmaker..."
    Session = sessionmaker(bind=engine)
    print "Setting up session..."
    self.session = Session()
    print ("We've cached %d states. Cool!"
           % self.session.query(CachedState).count())

  def get_outcome(self, state):
    cached_state = self.session.query(
        CachedState).filter_by(state=state).first()
    if cached_state:
      return Outcome(cached_state.value, cached_state.depth)
    else:
      return None

  def save_outcome(self, state, value, depth):
    cached_state = CachedState(state, value, depth)
    self.session.merge(cached_state)

  def delete_outcome(self, state):
    to_purge = self.session.query(CachedState).filter_by(state=state).first()
    self.session.delete(to_purge)

  def flush(self):
    self.session.commit()
    self.session.flush()
