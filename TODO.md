- CAST для Unsigned<N>
- Нужен ли CAST или лучше использовать Into/From?
- Оптимизация NodeId/NodeOutId (память расходуется неэффективно)
- Вместо Vec<BitVecMask> завести более компактное представление
- Инжектить только выводы для splitter
- Добавить макросы для "функционального" пайплайна и отслеживания сигналов:
вместо:
```rust
self.and_then(move |signal| {
    let signal = signal.watch("is_rising::signal");
    let old_signal = {
        let signal = signal.clone();
        reg(clk, rst, T::MIN, move |_| {
            let old_signal = signal.value();
            if old_signal == T::MIN {
                T::MAX
            } else {
                T::MIN
            }
        })
        .watch("is_rising::old_signal")
    };

    apply2(signal, old_signal, |signal, old_signal| {
        signal == T::MAX && signal == old_signal
    })
})
.map(Bit::from)
.watch("is_rising::output")
```
использовать:
```rust
  #[watch]
  signal
    |>> move |#[watch] signal| {
        #[watch]
        let old_signal = (signal) => reg(clk, rst, false, move |_| {
            if signal == T::MIN { T::MAX } else { T::MIN }
        });

        signal <&&> old_signal
   }
   |> Bit::from
)
```
- Добавить возможность подключить синтезированные модули из других крейтов
- Придумать DSL для blackbox
