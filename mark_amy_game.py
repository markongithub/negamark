import logging
FORMAT = '%(asctime)-15s %(message)s'
logging.basicConfig(level=logging.INFO, format=FORMAT)

from negamark import NegamarkBoard
from product_game import ProductGameBoard, ProductGameMove
from storm_mysql_transposition_table import StormMySQLTranspositionTable

def main():

  mark_amy = ProductGameBoard(
      StormMySQLTranspositionTable('mysql://productgame@localhost/productgame'))
  mark_amy.make_move(ProductGameMove(6, 3)) #A
#  mark_amy.make_move(ProductGameMove(7, 3)) 
#  mark_amy.make_move(ProductGameMove(7, 4)) 
#  mark_amy.make_move(ProductGameMove(8, 4)) 
#  mark_amy.make_move(ProductGameMove(8, 6)) 

  mark_amy.ai_deadline=60*60*24*7
  mark_amy.minimum_search_move = 0
  mark_amy.minimum_info_interval = 300
  mark_amy.is_automated[NegamarkBoard.X] = False
  mark_amy.is_automated[NegamarkBoard.O] = True

  mark_amy.play_game()
#  mark_amy.choose_ai_move()

if __name__ == '__main__':
  main()
#  import profile
#  profile.run('main()')
