# CS61AS

These are my solutions to the homework and projects for [CS61AS](https://berkeley-cs61as.github.io/index.html). I'm mainly using this as a structured way to work through the [SICP](https://web.mit.edu/6.001/6.037/sicp.pdf) book and to learn the [Racket](https://racket-lang.org) and [Scheme](https://www.scheme.org/) programming languages.

## Install Racket

```bash
sudo add-apt-repository ppa:plt/racket
sudo apt-get update
sudo apt-get install racket
```

## Running the Autograder

```bash
racket -tm ../grader.rkt -- hw0-1-tests.rkt hw0-1.rkt sum-of-squares
```


## Manual Testing

```bash
racket -it hw0-1.rkt
> (sum-of-squares 3 4)
```
