import logging
FORMAT = '%(asctime)-15s %(message)s'
#logging.basicConfig(level=logging.INFO, format=FORMAT)

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
#  mark_amy.make_move(ProductGameMove(7, 6))
#  mark_amy.make_move(ProductGameMove(5, 6))
# After this point O thinks he will lose by move 15
#  mark_amy.make_move(ProductGameMove(6, 6))
#  mark_amy.make_move(ProductGameMove(6, 9))
#  mark_amy.make_move(ProductGameMove(9, 9))
#  mark_amy.make_move(ProductGameMove(9, 8))
#  mark_amy.make_move(ProductGameMove(1, 8))
#  mark_amy.make_move(ProductGameMove(2, 8))
#  mark_amy.make_move(ProductGameMove(2, 6))
#  mark_amy.make_move(ProductGameMove(4, 6))
# After this point O thinks he will lose by move 29
#  mark_amy.make_move(ProductGameMove(4, 1))
#  mark_amy.make_move(ProductGameMove(5, 1))
#  mark_amy.make_move(ProductGameMove(5, 9))
#  mark_amy.make_move(ProductGameMove(5, 8))
#  mark_amy.make_move(ProductGameMove(5, 4))
#  mark_amy.make_move(ProductGameMove(5, 2))
#  mark_amy.make_move(ProductGameMove(1, 2))
#  mark_amy.make_move(ProductGameMove(1, 3))

  mark_amy.ai_deadline=60*3
  mark_amy.is_automated[NegamarkBoard.X] = False
  mark_amy.is_automated[NegamarkBoard.O] = True

  mark_amy.choose_ai_move()

if __name__ == '__main__':
  import profile
  profile.run('main()')
