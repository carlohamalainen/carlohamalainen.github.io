package main

import "fmt";
import "rand";
import "runtime";

func generate(ch chan int) {
    // some setup stuff
    n := 10000;
    vec := make([]int, n);
    for i := 0; i < n; i++ { vec[i] = rand.Intn(10*n) }

    // spit out random numbers
    for {
        ch <- rand.Intn(10);
    }
}


func main() {
    runtime.GOMAXPROCS(2);

    ch1 := make(chan int);
    ch2 := make(chan int);

    go generate(ch1);
    go generate(ch2);

    for j :=  0; j < 2000; j++ {
        blah1 := <-ch1;
        blah2 := <-ch2;

        fmt.Printf("%v %v\n", blah1, blah2);
    }
}

