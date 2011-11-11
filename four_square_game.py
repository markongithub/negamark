import logging
FORMAT = '%(asctime)-15s %(message)s'
logging.basicConfig(level=logging.INFO, format=FORMAT)

from negamark import AbstractGameStateCache, NegamarkBoard
from product_game import ProductGameBoard, ProductGameMove
from storm_mysql_game_state_cache import StormMySQLGameStateCache

def main():

  my_board = ProductGameBoard(
      StormMySQLGameStateCache('mysql://productgame@127.0.0.1:3307/productgame'))
#      RedisGameStateCache('localhost'))
#      AbstractGameStateCache())
  my_board.make_move(ProductGameMove(4, 4))

  my_board.ai_deadline=999999
  my_board.minimum_search_move = 24
  my_board.max_cache_move = 12
  my_board.minimum_info_interval = 15
  my_board.is_automated[NegamarkBoard.X] = True
  my_board.is_automated[NegamarkBoard.O] = False

  my_board.choose_ai_move()
  my_board.flush_cache()

if __name__ == '__main__':
  main()
#  import profile
#  profile.run('main()')
