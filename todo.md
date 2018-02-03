
- gOrderByはgOrderでいいな。なぜなら.byはByCompで組み立てるから。
  gByはprojectionを作り、operatorでByCompを作れば見た目はうまく整理できるのでは。

- traversal sourceはVoidで開始じゃなくて()でいいと思う。conduitでもそうなっている。

