import logging
FORMAT = '%(asctime)-15s %(message)s'
logging.basicConfig(level=logging.INFO, format=FORMAT)

from negamark import AbstractGameStateCache, NegamarkBoard
from product_game import ProductGameBoard, ProductGameMove
from redis_game_state_cache import RedisGameStateCache
from storm_mysql_game_state_cache import StormMySQLGameStateCache

def main():

  mark_amy = ProductGameBoard(
      StormMySQLGameStateCache('mysql://productgame@localhost/productgame'))
#      RedisGameStateCache('localhost'))
#      AbstractGameStateCache())
  mark_amy.make_move(ProductGameMove(4, 4)) #A

  mark_amy.ai_deadline=999999
#  mark_amy.ai_deadline=300
#  mark_amy.ai_deadline=60
  mark_amy.minimum_search_move = 36
  mark_amy.minimum_info_interval = 300
  mark_amy.max_cache_move=13
  mark_amy.is_automated[NegamarkBoard.X] = False
  mark_amy.is_automated[NegamarkBoard.O] = True

  mark_amy.play_game()
#  mark_amy.choose_ai_move()

if __name__ == '__main__':
  main()
#  import profile
#  profile.run('main()')
