(library (ad-libitum noise (1))
  (export white)
  (import (chezscheme)
          (ad-libitum common)
          (ad-libitum signal))

  (define~ white (random-amplitude))
  )
