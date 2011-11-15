from sqlalchemy import *
from sqlalchemy.orm import mapper, sessionmaker

class SQLAlchemyTranspositionTable(TranspositionTable):
  def __init__(self, database_url):
    engine = create_engine('sqlite:///negamark.db')
    print "Setting up database engine..."
    engine = create_engine(database_url)
    print "Setting up metadata..."
    metadata = MetaData(engine)
    self.transpositions = Table('transpositions', metadata,
                               Column('state', BigInteger, primary_key=True),
                               Column('value', Integer, nullable=False),
                               Column('heuristic', Integer),
                               Column('depth', Integer, nullable=False),)
    mapper(Transposition, self.transpositions)
    print "Creating tables if necessary..."
    metadata.create_all(engine)
    print "Setting up sessionmaker..."
    Session = sessionmaker(bind=engine)
    print "Setting up session..."
    self.session = Session()
    print ("We've cached %d states. Cool!"
           % self.session.query(Transposition).count())

  def get_outcome(self, state):
    transposition = self.session.query(
        Transposition).filter_by(state=state).first()
    if transposition:
      return Outcome(transposition.value, transposition.depth)
    else:
      return None

  def save_outcome(self, state, value, depth):
    transposition = Transposition(state, value, depth)
    self.session.merge(transposition)

  def delete_outcome(self, state):
    to_purge = self.session.query(Transposition).filter_by(state=state).first()
    self.session.delete(to_purge)

  def flush(self):
    self.session.commit()
    self.session.flush()
