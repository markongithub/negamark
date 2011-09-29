import logging
FORMAT = '%(asctime)-15s %(message)s'
logging.basicConfig(level=logging.INFO, format=FORMAT)

from negamark import NegamarkBoard
from product_game import ProductGameBoard, ProductGameMove
from storm_game_state_cache import StormGameStateCache

def main():

  mark_amy = ProductGameBoard(
      StormGameStateCache('sqlite:product_game_storm.db'))
  mark_amy.make_move(ProductGameMove(6, 3)) #A
  mark_amy.make_move(ProductGameMove(7, 3))
  mark_amy.make_move(ProductGameMove(7, 4))
  mark_amy.make_move(ProductGameMove(8, 4))
  mark_amy.make_move(ProductGameMove(8, 6))

  mark_amy.ai_deadline=60*60*3
  mark_amy.is_automated[NegamarkBoard.X] = False
  mark_amy.is_automated[NegamarkBoard.O] = True

  mark_amy.play_game()
#  mark_amy.choose_ai_move()

if __name__ == '__main__':
  main()
#  import profile
#  profile.run('main()')
