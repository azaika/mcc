use global_counter::primitive::exact::CounterUsize;

pub type Id = String;

pub fn gen_uniq() -> Id {
    static COUNTER : CounterUsize = CounterUsize::new(0);
    let n = COUNTER.get();
    COUNTER.inc();

    format!("T{}", n)
}
