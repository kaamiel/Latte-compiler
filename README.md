# Latte-compiler

Compiler construction 2020 task 2 – Latte compiler

Task description (in Polish): https://www.mimuw.edu.pl/~ben/Zajecia/Mrj2020/latte.html

Language description (in Polish): https://www.mimuw.edu.pl/~ben/Zajecia/Mrj2020/Latte/

The compiler generates the LLVM code. Implemented functionalities:
* constant folding & propagation,
* dead code elimination,
* registers and phi instead of alloc, load and store,
* global common subexpression elimination (GCSE).

---

Metody realizacji języków programowania 2020 Zadanie 2 – kompilator Latte

Opis zadania: https://www.mimuw.edu.pl/~ben/Zajecia/Mrj2020/latte.html

Opis języka: https://www.mimuw.edu.pl/~ben/Zajecia/Mrj2020/Latte/

Kompilator generuje kod dla LLVM. Zaimplementowane funkcjonalności:
* upraszczanie wyrażeń stałych (po stronie frontendu):
    np. dla `printInt((1 + 2 + 3) * 4 * a + b);` wygeneruje kod identyczny,
    jak dla `printInt(24 * a + b);`,
* pomijanie nieosiągalnego lub nieposiadającego efektów kodu (po stronie frontendu i backendu):
    np. dla
    ```c
    void f1() {
        if (readInt() > 0) {
            return;
            printString("1");
        } else {
            if (false)
                error();
            "2";
            return;
        }
        printString("3");
    }
    ```
    nie zostaną wygenerowane napisy i wywołania funkcji printString i error.
* użycie rejestrów i phi zamiast alloc, load i store,
* GCSE: dodatkowo z uwzględnieniem przemiennych operatorów (np. dla `1 + x` zostanie rozpoznane
    `x + 1`, które już wcześniej wystąpiło), podobnych porównań (np. dla `x > 0` zostanie
    rozpoznane `0 < x`, które już wcześniej wystąpiło), identycznych phi, konkatenacji
    tych samych napisów, użycia tych samych stałych napisowych (identycznych bitcast), wyrażeń
    wygenerowanych w gałęziach if/else, które na pewno zostaną wygenerowane.
