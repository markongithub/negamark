#!/usr/bin/python

import atexit
import datetime
import logging
import random

class CachedState(object):
  def __init__(self, state, value, depth):
    self.state = state
    self.value = value
    self.depth = depth


class AbstractGameStateCache(object):

  def get_outcome(self, state):
    return None

  def save_outcome(self, state, value, depth):
    pass

  def delete_outcome(self, state):
    pass

  def flush(self):
    pass


class Outcome(object):

  (LOSS, STALEMATE, TIMEOUT, WIN) = (20, 21, 22, 23)

  def __init__(self, value, depth):
    self.value = value
    self.depth = depth

  def __cmp__(self, other_outcome):
    if self.value > other_outcome.value:
      return 1
    elif self.value < other_outcome.value:
      return -1
    elif self.value == Outcome.WIN or self.value == Outcome.TIMEOUT:
      return cmp(other_outcome.depth, self.depth)
    elif self.value == Outcome.STALEMATE or self.value == Outcome.LOSS:
      return cmp(self.depth, other_outcome.depth)

  def __str__(self):
    if self.value == Outcome.LOSS:
      value_string = "LOSS"
    elif self.value == Outcome.STALEMATE:
      value_string = "STALEMATE"
    elif self.value == Outcome.TIMEOUT:
      value_string = "TIMEOUT"
    else:
      value_string = "WIN"
    return value_string+ "@" + str(self.depth)

  def opposite(self):
    if self.value == Outcome.WIN:
      return Outcome(Outcome.LOSS, self.depth)
    elif self.value == Outcome.LOSS:
      return Outcome(Outcome.WIN, self.depth)
    else:
      return self

class NegamarkMove(object):
  pass


class NegamarkBoard(object):

  (OPEN, O, X, NO_WINNER) = (0, 1, 2, 3)

  def __init__(self, cache=None):
    self.moves_so_far = 0
    self.previous_unique_id = None
    self.active_player = NegamarkBoard.X
    self.ai_deadline = 60
    self.minimum_search_move = 2
    self.is_automated = {}
    self.is_automated[NegamarkBoard.X] = False
    self.is_automated[NegamarkBoard.O] = True
    if cache:
      self.cache = cache
    else:
      self.cache = AbstractGameStateCache()

  def get_cached_outcome(self):
    logging.debug("Checking the cache for state %s." % self.unique_id())
    return self.cache.get_outcome(self.unique_id())

  def save_cached_value(self, outcome_value):
    outcome = Outcome(outcome_value, self.moves_so_far)
    self.save_cached_outcome(outcome)

  def cache_value_and_return(self, outcome_value):
    self.save_cached_value(outcome_value)
    return Outcome(outcome_value, self.moves_so_far)

  def save_cached_outcome(self, outcome):
    logging.debug("Saving outcome %s for %s." % (outcome, self.unique_id()))
    self.cache.save_outcome(self.unique_id(), outcome.value, outcome.depth)

  def uncache_this_outcome(self):
    self.cache.delete_outcome(self.unique_id())

  def cache_outcome_and_return(self, outcome):
    logging.debug("We are caching and returning %s." % outcome)
    self.save_cached_outcome(outcome)
    return outcome

  def unique_id_for_move(self, move):
    new_board = self.new_board_from_move(move)
    return new_board.unique_id()

  def player_name(self, player):
    if player == NegamarkBoard.X:
      return "X"
    else:
      return "O"

  def other_player(self, player):
    if player == NegamarkBoard.X:
      return NegamarkBoard.O
    elif player == NegamarkBoard.O:
      return NegamarkBoard.X
    else:
      raise NegamarkError("Invalid player ID: ", str(player))

  def new_board_from_move(self, move):
    new_board = self.copy_board()
    new_board.moves_so_far = self.moves_so_far
    new_board.active_player = self.active_player
    new_board.previous_unique_id = self.unique_id()
    new_board.make_move(move)
    return new_board

  def negamark(self, current_depth, path, max_depth, deadline):
    indent = " " * current_depth
    cached_outcome = self.get_cached_outcome()
    if cached_outcome:
      logging.debug("The cache says " + str(cached_outcome))
      if cached_outcome.value == Outcome.TIMEOUT:
        #Okay say it's t12. And our current_depth is 3. And our max depth is
        #6. And our moves_so_far is 8. Then on this turn we expect to descend
        #to moves_so_far = 8 - 3 + 6 = 11.
        expected_depth = self.moves_so_far - current_depth + max_depth
        if cached_outcome.depth < expected_depth:
          logging.debug("I think I can traverse that on this turn.")
        else:
          logging.debug("timeout:%d  msf:%d cur:%d max: %d ex:%d." % (
              cached_outcome.depth, self.moves_so_far,
              current_depth, max_depth, expected_depth))
          return cached_outcome
      else:
        return cached_outcome
    else:
      logging.debug("The cache didn't have it.")
    # We have chosen not to return the cached value. We continue.
    winner = self.winner()
    if winner == self.active_player:
      logging.debug("I think %s just won after the other player's turn. That "
                    "really shouldn't happen." %
                    self.player_name(self.active_player))
      return self.cache_value_and_return(Outcome.WIN)
    elif winner == self.other_player(self.active_player):
      return self.cache_value_and_return(Outcome.LOSS)
    legal_moves = self.all_legal_moves()
    num_legal_moves = len(legal_moves)
    if num_legal_moves == 0:
      return self.cache_value_and_return(Outcome.STALEMATE)
    if current_depth > max_depth:
      logging.debug("current_depth %d > max_depth %d after move %d. Timeout." %           (current_depth, max_depth, self.moves_so_far))
      return self.cache_value_and_return(Outcome.TIMEOUT)
    if datetime.datetime.now() > deadline:
      logging.debug("We are past our deadline. Timeout.")
      return self.cache_value_and_return(Outcome.TIMEOUT)
    best_outcome = Outcome(Outcome.LOSS, 0) # no move is this bad
    child_node_index = 0
    for move in legal_moves:
      child_node_index += 1
      branch_name = "%d/%d" % (child_node_index, num_legal_moves)
      logging_method = logging.debug
      if (max_depth - current_depth) > 3:
        logging_method = logging.info
      logging_method(str(path) + " "+ branch_name
                     + " cur %d max %d: then %s could move to %s..."
                     % (current_depth, max_depth,
                        self.player_name(self.active_player), move))
      child_board = self.new_board_from_move(move)
      child_outcome = child_board.negamark(current_depth=current_depth + 1,
                                           path = path + " " + branch_name,
                                           max_depth=max_depth,
                                           deadline=deadline)
      child_opposite = child_outcome.opposite()
      if child_opposite.value == Outcome.WIN:
        logging_method("This is a win. We could wait for a better win but why?")
        return self.cache_outcome_and_return(child_opposite)
      best_outcome = max(best_outcome, child_opposite)
    return self.cache_outcome_and_return(best_outcome)

  def choose_ai_move(self):
    best_val = Outcome(Outcome.LOSS, 0)
    child_node_id = 0
    max_depth = max(1, self.minimum_search_move - self.moves_so_far)
    legal_moves = self.all_legal_moves()
    deadline = datetime.datetime.now() + datetime.timedelta(
        seconds=self.ai_deadline)
    print("%s will pick a move by %s"
          % (self.player_name(self.active_player), str(deadline)))
    while True:
      best_moves = []
      for this_move in legal_moves:
        child_node_id += 1
        branch_name = "%d/%d" % (child_node_id, len(legal_moves))
        logging.info("I wonder what would happen if I moved to %s..."
                     % this_move)
        child_board = self.new_board_from_move(this_move)
        child_outcome = child_board.negamark(current_depth = 1,
                                             path = branch_name,
                                             max_depth=max_depth,
                                             deadline=deadline)
        child_opposite = child_outcome.opposite()
        if child_opposite.value == Outcome.WIN:
          print ("%s is going to win by move %d. No, really."
                 % (self.player_name(self.active_player), child_opposite.depth))
          if child_opposite.depth == (self.moves_so_far + 1):
            print "Hey, that's this move. We are done!"
            return this_move
        if child_opposite > best_val:
          best_val = child_opposite
          best_moves = [this_move]
        elif child_opposite == best_val:
          best_moves.append(this_move)
        else:
          logging.debug("This result sucks. I am ignoring it.")
      if best_val.value == Outcome.LOSS:
        print ("%s will lose by move %d unless %s screws up."
               % (self.player_name(self.active_player), best_val.depth,
                  self.player_name(self.other_player(self.active_player))))
        cached_outcome = self.get_cached_outcome()
        if cached_outcome:
          logging.debug("This situation exists in the cache with a value of %s."
                        % cached_outcome)
        else:
          logging.debug("This situation isn't in the cache. I am adding it.")
          self.save_cached_value(Outcome.LOSS) #TODO add correct depth here
        if self.previous_unique_id:
          self.cache.save_outcome(self.previous_unique_id, outcome.WIN,
                                  self.moves_so_far) #TODO also here
      if best_val.value != Outcome.TIMEOUT:
        return random.choice(best_moves)
      # At this point I think our best move is a timeout. If we have time
      # we should traverse deeper.
      best_val = Outcome(Outcome.LOSS, 0)
      child_node_id = 0
      if datetime.datetime.now() < deadline:
        logging.debug("Incrementing max_depth. We need to go deeper.")
        max_depth += 1
      else:
        print "To be honest, this move is kind of a guess."
        chosen_move = random.choice(best_moves)
        destination_board = self.new_board_from_move(chosen_move)
        cached_outcome = destination_board.get_cached_outcome()
        if cached_outcome:
          if cached_outcome.value == Outcome.TIMEOUT:
            logging.debug("I am nuking the cached timeout for this move.")
            destination_board.uncache_this_outcome()
        else:
          logging.debug("This move isn't in the cache WHAT THE FUCK")
        return chosen_move

  def flush_cache(self):
    logging.info("Flushing the cache.")
    self.cache.flush()

  def play_game(self):
    atexit.register(self.flush_cache)
    while (len(self.all_legal_moves()) > 0 and
           self.winner() == NegamarkBoard.NO_WINNER):
      print "This is move %d" % (self.moves_so_far + 1)
      self.print_board()
      next_move = None
      if not self.is_automated[self.active_player]:
        next_move = self.human_move_from_stdin()
      else:
        next_move = self.choose_ai_move()
        print "%s chooses %s" % (self.player_name(self.active_player),
                                 next_move)
      self.make_move(next_move)
    self.print_board()
    winner = self.winner()
    if winner == NegamarkBoard.X or winner == NegamarkBoard.O:
       print "It looks like " + self.player_name(winner) + " is the winner."
    elif len(self.all_legal_moves()) == 0:
       print "It looks like a stalemate."
    else:
       print "I have no idea why the game ended."
    self.flush_cache()
