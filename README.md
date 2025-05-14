# Interpretador de PCL

~~Leia o post [O Modelo de memória de Porcelain](https://sacolle.github.io/blog/) para saber mais.~~

Leia o post [Porcelain - Unsafe Porcelain](https://sacolle.github.io/blog/posts/porcelain-unsafe-porcelain/). Ele contém a vesão mais atualizada da linguagem.

Houveram atualizações no projeto desde o último post e devem ir ao ar quando estiver pronto.

Para rodar, use:
```
cabal run porcelain [step|full] [filename].pcl
```


### Detecção de Erro.

Devido ao formato de execução de PCL, todos os erros são capturados em tempo de execução. Porém, no contexto de uso dessa linguagem, chamadas para a função `error` de haskell são equivalentes a erros de compilação, possivelmente de checagem de tipo. Emições da Expressão `panic` por outro lado são detecções dos erros em tempo de execução.

Esses erros são tendem a ser categorizados em 5 classes. Isso é elaborado melhor em [Erros de Memória em Porcelain](https://sacolle.github.io/blog/posts/porcelain-emulando-os-erros-de-memoria-de-c/). 
Esse artigo não está completo, mas está.