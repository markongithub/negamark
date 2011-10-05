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

  def save_outcome(self, state, value, depth, heuristic=None):
    pass

  def delete_outcome(self, state):
    pass

  def flush(self):
    pass


class Outcome(object):

  (LOSS, STALEMATE, TIMEOUT, WIN) = (20, 21, 22, 23)

  def __init__(self, value, depth, heuristic=None):
    self.value = value
    self.depth = depth
    self.heuristic = heuristic

  def __cmp__(self, other_outcome):
    if self.value > other_outcome.value:
      return 1
    elif self.value < other_outcome.value:
      return -1
    elif self.value == Outcome.WIN:
      return cmp(other_outcome.depth, self.depth)
    elif self.value == Outcome.TIMEOUT:
      if self.heuristic is not None and other_outcome.heuristic is not None:
        return cmp(self.heuristic, other_outcome.heuristic)
      else:
        return cmp(other_outcome.depth, self.depth)
    elif self.value == Outcome.STALEMATE or self.value == Outcome.LOSS:
      return cmp(self.depth, other_outcome.depth)

  def __str__(self):
    if self.value == Outcome.LOSS:
      value_string = "LOSS"
    elif self.value == Outcome.STALEMATE:
      value_string = "STALEMATE"
    elif self.value == Outcome.TIMEOUT:
      if self.heuristic is not None:
        value_string = "TIMEOUT-%d" % self.heuristic
      else:
        value_string = "TIMEOUT"
    else:
      value_string = "WIN"
    return value_string+ "@" + str(self.depth)

  def opposite(self):
    if self.value == Outcome.WIN:
      return Outcome(Outcome.LOSS, self.depth)
    elif self.value == Outcome.LOSS:
      return Outcome(Outcome.WIN, self.depth)
    elif self.value == Outcome.TIMEOUT:
      if self.heuristic is None:
        self.heuristic = 0
      return Outcome(Outcome.TIMEOUT, self.depth, -1 * self.heuristic)
    else:
      return self


class NegamarkMove(object):

  def __init__(self):
    self.outcome = Outcome(Outcome.TIMEOUT, 0, 0)

  def __cmp__(self, other):
    return cmp(self.outcome, other.outcome)

class NegamarkBoard(object):

  (OPEN, O, X, NO_WINNER) = (0, 1, 2, 3)

  def __init__(self, cache=None):
    self.moves_so_far = 0
    self.previous_unique_id = None
    self.active_player = NegamarkBoard.X
    self.ai_deadline = 60
    self.minimum_search_move = 2
    self.minimum_info_interval = 120
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
    self.cache.save_outcome(self.unique_id(), outcome.value, outcome.depth,
                            outcome.heuristic)

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

  def first_pass(self):
    cached_outcome = self.get_cached_outcome()
    if cached_outcome:
      logging.debug("The cache says " + str(cached_outcome))
      return cached_outcome
    else:
      logging.debug("The cache didn't have it.")
    heuristic = self.heuristic()
    if heuristic >= 1000:
      logging.debug("I think %s just won after the other player's turn. That "
                    "really shouldn't happen." %
                    self.player_name(self.active_player))
      return Outcome(Outcome.WIN, self.moves_so_far)
    elif heuristic <= -1000:
      return Outcome(Outcome.LOSS, self.moves_so_far)
    legal_moves = self.all_legal_moves()
    num_legal_moves = len(legal_moves)
    if num_legal_moves == 0:
      return Outcome(Outcome.STALEMATE, self.moves_so_far)
    return Outcome(Outcome.TIMEOUT, self.moves_so_far, heuristic)

  def recurse(self, current_depth, path, max_depth, deadline):
    selective_log = logging.debug
#    if (max_depth - current_depth) > 6:
    if current_depth <= 5:
      selective_log = logging.info
    logging_deadline = datetime.datetime.now() + datetime.timedelta(
        seconds=self.minimum_info_interval)
    logging.debug('recurse cd %d path "%s" md %d deadline %s' %
                  (current_depth, path, max_depth, str(deadline)))
    need_a_decision = (current_depth == 0)
    legal_moves = self.all_legal_moves()
    best_move = NegamarkMove()
    best_move.outcome = Outcome(Outcome.LOSS, 0) # no move is this bad
    for move in legal_moves:
      child_board = self.new_board_from_move(move)
      child_outcome = child_board.first_pass()
      move.outcome = child_outcome.opposite()
      logging.debug('First-pass check on move %s returned %s.' %
                    (move, move.outcome))
      best_move = max(best_move, move)
    if current_depth > max_depth:
      logging.debug("current_depth %d > max_depth %d after move %d. Timeout." %           (current_depth, max_depth, self.moves_so_far))
      return max(legal_moves).outcome
    if datetime.datetime.now() > deadline and not need_a_decision:
      logging.debug("We are past our deadline. Timeout.")
      return max(legal_moves).outcome
    child_node_index = 1
    pruned_moves = filter (lambda move: move.outcome.value != Outcome.LOSS,
                           legal_moves)
    num_pruned_moves = len(pruned_moves)
    for move in sorted(pruned_moves, reverse=True):
      if datetime.datetime.now() > logging_deadline:
        selective_log = logging.info
      #high opposites of child outcomes to low - best to worst
      branch_name = "%d/%d" % (child_node_index, num_pruned_moves)
      selective_log(str(path) + " "+ branch_name
                     + " cur %d max %d: then %s could move to %s..."
                     % (current_depth, max_depth,
                        self.player_name(self.active_player), move))
      child_board = self.new_board_from_move(move)
      child_outcome_for_us = None
      first_pass_outcome = move.outcome
      if first_pass_outcome.value == Outcome.TIMEOUT:
        #Okay say it's t12. And our current_depth is 3. And our max depth is
        #6. And our moves_so_far is 8. Then on this turn we expect to descend
        #to moves_so_far = 8 - 3 + 6 = 11.
        expected_depth = self.moves_so_far - current_depth + max_depth
        if first_pass_outcome.depth < expected_depth:
          logging.debug("I think I can traverse that on this turn.")
        else:
          logging.debug("timeout:%d  msf:%d cur:%d max: %d ex:%d." % (
              first_pass_outcome.depth, self.moves_so_far,
              current_depth, max_depth, expected_depth))
          child_outcome_for_us = first_pass_outcome
      else: # it's a definitive win, loss, or stalemate
        child_outcome_for_us = first_pass_outcome
      if not child_outcome_for_us:
        child_outcome = child_board.recurse(current_depth=current_depth + 1,
                                            path = path + " " + branch_name,
                                            max_depth=max_depth,
                                            deadline=deadline)
        child_board.save_cached_outcome(child_outcome)
        child_outcome_for_us = child_outcome.opposite()
      selective_log("Then we could achieve %s", child_outcome_for_us)
      move.outcome = child_outcome_for_us
      if child_outcome_for_us.value == Outcome.WIN:
        logging.debug("This is a win. We could wait for a better win but why?")
        self.save_cached_outcome(child_outcome_for_us)
        if need_a_decision:
          return move
        else:
          return child_outcome_for_us
      best_move = max(best_move, move)
      child_node_index += 1
    self.save_cached_outcome(best_move.outcome)
    if need_a_decision:
      return best_move
    else:
      return best_move.outcome

  def choose_ai_move(self):
    deadline = datetime.datetime.now() + datetime.timedelta(
        seconds=self.ai_deadline)
    print("%s will pick a move by %s"
          % (self.player_name(self.active_player), str(deadline)))
    max_depth = max(1, self.minimum_search_move - self.moves_so_far)
    decision = None
    while datetime.datetime.now() < deadline:
      decision = self.recurse(current_depth = 0,
                              path = '',
                              deadline=deadline,
                              max_depth=max_depth)
      max_depth += 1
      if decision.outcome.value == Outcome.WIN:
        print ('%s is going to win by move %d. It is destiny.' %
               (self.player_name(self.active_player), decision.outcome.depth))
        return decision
      if decision.outcome.value == Outcome.LOSS:
        print ('%s is going to lose by move %d unless %s screws up.' %
               (self.player_name(self.active_player),
                decision.outcome.depth,
                self.player_name(self.other_player(self.active_player))))
        return decision
      if decision.outcome.value == Outcome.STALEMATE:
        print ('The best %s can hope for is a draw.' %
               (self.player_name(self.active_player)))
        return decision
    if decision.outcome.value == Outcome.TIMEOUT:
      print ('This move is a guess but it can hopefully get us to heuristic %d'
             % decision.outcome.heuristic)
    return decision

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

  def verify_subclass(self):
    missing_methods = []
    for method in ['all_legal_moves', 'human_move_from_stdin', 'make_move',
                   'copy_board', 'print_board', 'unique_id']:
      if not hasattr(self, method):
        missing_methods.append(method)
    if missing_methods:
      raise NegamarkError('Some methods missing: %s' % missing_methods)
    else:
      return True
