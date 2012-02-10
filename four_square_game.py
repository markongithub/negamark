import logging
FORMAT = '%(asctime)-15s %(message)s'
#logging.basicConfig(level=logging.INFO, format=FORMAT)

from negamark import AbstractTranspositionTable, NegamarkBoard
from product_game import ProductGameBoard, ProductGameMove
from storm_mysql_transposition_table import StormMySQLTranspositionTable

def main():

  my_board = ProductGameBoard(
<<<<<<< Updated upstream
      StormMySQLTranspositionTable('mysql://productgame@127.0.0.1:3307/productgame'))
#      RedisTranspositionTable('localhost'))
#      AbstractTranspositionTable())
  my_board.make_move(ProductGameMove(4, 4))

  my_board.ai_deadline=999999
  my_board.minimum_search_move = 24
  my_board.max_transposition_table_move = 12
=======
      StormMySQLGameStateCache('mysql://productgame@127.0.0.1:3306/productgame'))
#      RedisGameStateCache('localhost'))
#      AbstractGameStateCache())
  my_board.make_move(ProductGameMove(4, 4))

  my_board.ai_deadline=60
  my_board.minimum_search_move = 0
  my_board.max_cache_move = 12
>>>>>>> Stashed changes
  my_board.minimum_info_interval = 15
  my_board.is_automated[NegamarkBoard.X] = False
  my_board.is_automated[NegamarkBoard.O] = True

<<<<<<< Updated upstream
  my_board.choose_ai_move()
  my_board.flush_transposition_table()
=======
  my_board.play_game()
>>>>>>> Stashed changes

if __name__ == '__main__':
  main()
#  import profile
#  profile.run('main()')
